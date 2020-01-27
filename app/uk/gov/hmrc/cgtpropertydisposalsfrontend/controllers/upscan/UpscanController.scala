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
import com.google.inject.{Inject, Singleton}
import configs.Configs
import configs.syntax._
import play.api.Configuration
import play.api.libs.Files
import play.api.libs.json.JsValue
import play.api.mvc.{Action, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.UpscanNotifyResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.UpscanServiceResponse._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
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

  private val maxFileSize: Int = getUpscanInitiateConfig[Int]("max-uploads")

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  def upload(): Action[MultipartFormData[Files.TemporaryFile]] = Action(parse.multipartFormData(maxFileSize)).async {
    implicit request: MessagesRequest[MultipartFormData[Files.TemporaryFile]] =>
      val multipart = request.body
      multipart.dataParts.get("href").flatMap(_.headOption).fold(Future.successful(BadRequest("missing href"))) {
        href =>
          val data: Map[String, Seq[String]] = multipart.dataParts.map {
            case (k, v) => (k.stripPrefix("upscan."), v)
          }
          val withSource =
            multipart.files.map(file => file.copy(ref = FileIO.fromPath(file.ref.path): Source[ByteString, Any]))
          val prepared = multipart.copy(files = withSource, dataParts = data)
          upscanConnector.upload(href, prepared).value.map {
            case Left(error) =>
              logger.warn(s"Could not upload file to S3", error)
              errorHandler.errorResult(None)
            case Right(_) => Ok
          }
      }
  }

  def callBack(cgtReference: String): Action[JsValue] = Action.async(parse.json) { implicit request =>
    request.body
      .validate[UpscanNotifyResponse]
      .asOpt
      .fold(Future.successful(BadRequest("Invalid call back response received from upscan notify")))(notifyEvent =>
        upscanService.storeNotifyEvent(CgtReference(cgtReference), notifyEvent).value.map {
          case Left(error) =>
            logger.warn("upscan notifier call back handler failed", error)
            errorHandler.errorResult(None)
          case Right(_) =>
            logger.info("upscan notifier call back succeeded")
            NoContent
        }
      )
  }

  def upscan(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSubscribedUser(request) { (_, subscribed) =>
      upscanService.initiate(subscribed.subscribedDetails.cgtReference).value.map {
        case Left(error) =>
          logger.warn(s"Could not initiate upscan due to $error")
          errorHandler.errorResult(None)
        case Right(upscanResult) =>
          upscanResult match {
            case MaximumFileUploadReached            => Ok(upscanLimitPage())
            case UpscanResponse(_, upscanDescriptor) => Ok(upscanPage(upscanDescriptor))
          }
      }
    }
  }

}
