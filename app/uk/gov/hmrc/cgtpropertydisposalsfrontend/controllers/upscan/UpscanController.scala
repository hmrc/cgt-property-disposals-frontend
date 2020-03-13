/*
 * Copyright 2020 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.upscan

import akka.stream.scaladsl.{FileIO, Source}
import akka.util.ByteString
import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import configs.Configs
import configs.syntax._
import play.api.Configuration
import play.api.libs.Files
import play.api.mvc.{Action, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanFileDescriptor.UpscanFileDescriptorStatus.UPLOADED
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanInitiateResponse._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UpscanFileDescriptor, UpscanInitiateReference}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views._
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UpscanController @Inject() (
  subscriptionService: SubscriptionService,
  upscanService: UpscanService,
  upscanConnector: UpscanConnector,
  configuration: Configuration,
  sessionStore: SessionStore,
  errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  upscanPage: html.upscan.upscan,
  upscanLimitPage: html.upscan.upscan_limit,
  upscanSuccessPage: html.upscan.upscan_success
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withSubscribedUser(request: RequestWithSessionData[_])(
    f: (SessionData, Subscribed) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s: SessionData, r: Subscribed)) =>
        f(s, r)
      case _ =>
        Future.successful(
          SeeOther(uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController.start().url)
        )
    }

  private def getUpscanInitiateConfig[A: Configs](key: String): A =
    configuration.underlying
      .get[A](s"microservice.services.upscan-initiate.$key")
      .value

  private val maxFileSize: Int = getUpscanInitiateConfig[Int]("max-file-size")

  def upscan(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubscribedUser(request) { (_, subscribed) =>
        upscanService.initiate(subscribed.subscribedDetails.cgtReference).value.map {
          case Left(error) =>
            logger.warn(s"could not initiate upscan due to $error")
            errorHandler.errorResult(None)
          case Right(upscanInitiateResponse) =>
            upscanInitiateResponse match {
              case FailedToGetUpscanSnapshot                => errorHandler.errorResult(None)
              case MaximumFileUploadReached                 => Ok(upscanLimitPage())
              case UpscanInititateResponseStored(reference) => Ok(upscanPage(reference))
            }
        }
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  def upload(): Action[MultipartFormData[Files.TemporaryFile]] =
    authenticatedActionWithSessionData(parse.multipartFormData(maxFileSize)).async { implicit request =>
      withSubscribedUser(request) { (_, subscribed) =>
        val multipart: MultipartFormData[Files.TemporaryFile] = request.body
        val result = for {
          reference <- EitherT.fromOption(
                        multipart.dataParts.get("reference").flatMap(_.headOption),
                        Error("missing upscan file descriptor id")
                      )
          maybeUpscanFileDescriptor <- upscanService
                                        .getUpscanFileDescriptor(
                                          subscribed.subscribedDetails.cgtReference,
                                          UpscanInitiateReference(reference)
                                        )
          upscanFileDescriptor <- EitherT.fromOption(
                                   maybeUpscanFileDescriptor,
                                   Error("failed to retrieve upscan file descriptor details")
                                 )
          prepared <- EitherT.fromEither(handleGetFileDescriptorResult(multipart, upscanFileDescriptor))
          _        <- upscanConnector.upload(upscanFileDescriptor.fileDescriptor.uploadRequest.href, prepared)
          _        <- upscanConnector.updateUpscanFileDescriptorStatus(upscanFileDescriptor.copy(status = UPLOADED))

        } yield ()

        result.fold(
          error => {
            logger.warn(s"failed to upload file with error: $error")
            errorHandler.errorResult(None)
          },
          _ => Ok
        )
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  private def handleGetFileDescriptorResult(
    multipart: MultipartFormData[Files.TemporaryFile],
    upscanFileDescriptor: UpscanFileDescriptor
  ): Either[Error, MultipartFormData[Source[ByteString, Any]]] = {
    val userFile =
      multipart.files
        .map(file => file.copy(ref = FileIO.fromPath(file.ref.path): Source[ByteString, Any]))
    val prepared: MultipartFormData[Source[ByteString, Any]] =
      multipart
        .copy(
          files = userFile,
          dataParts = upscanFileDescriptor.fileDescriptor.uploadRequest.fields
            .mapValues(fieldValue => Seq(fieldValue))
        )
    Right(prepared)
  }

}
