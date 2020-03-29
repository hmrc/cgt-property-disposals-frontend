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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import akka.stream.scaladsl.{FileIO, Source}
import akka.util.ByteString
import cats.data.EitherT
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.Files
import play.api.mvc.{MultipartFormData, Result}
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers.{status, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.upscan.UpscanController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanFileDescriptor.UpscanFileDescriptorStatus.UPLOADED
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanInitiateResponse.UpscanInititateResponseStored
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UpscanFileDescriptor, UpscanInitiateReference, UpscanInitiateResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class UpscanControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockUpscanService   = mock[UpscanService]
  val mockUpscanConnector = mock[UpscanConnector]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[UpscanService].toInstance(mockUpscanService),
      bind[UpscanConnector].toInstance(mockUpscanConnector)
    )

  lazy val controller: UpscanController = instanceOf[UpscanController]

  val requestWithCSRFToken = FakeRequest().withCSRFToken

  def mockUpscanInitiate(cgtReference: CgtReference)(response: Either[Error, UpscanInitiateResponse]) =
    (mockUpscanService
      .initiate(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT(Future.successful(response)))

  def mockGetUpscanFileDescriptor(cgtReference: CgtReference, upscanInitiateReference: UpscanInitiateReference)(
    response: Either[Error, Option[UpscanFileDescriptor]]
  ) =
    (mockUpscanService
      .getUpscanFileDescriptor(_: CgtReference, _: UpscanInitiateReference)(_: HeaderCarrier))
      .expects(cgtReference, upscanInitiateReference, *)
      .returning(EitherT(Future.successful(response)))

  def mockUpload(url: String, mp: MultipartFormData[Source[ByteString, Any]])(
    response: Either[Error, Unit]
  ) =
    (mockUpscanConnector
      .upload(_: String, _: MultipartFormData[Source[ByteString, Any]])(_: HeaderCarrier))
      .expects(url, mp, *)
      .returning(EitherT(Future.successful(response)))

  def mockUpdateUpscanFileDescriptorStatus(upscanFileDescriptor: UpscanFileDescriptor)(
    response: Either[Error, Unit]
  ) =
    (mockUpscanConnector
      .updateUpscanFileDescriptorStatus(_: UpscanFileDescriptor)(_: HeaderCarrier))
      .expects(upscanFileDescriptor, *)
      .returning(EitherT(Future.successful(response)))

  "The UpscanController" when {

    implicit lazy val messagesApi: MessagesApi = controller.messagesApi

    "an upload request is received" should {

      "upload a file to S3 successfully" in {
        val subscribed              = sample[Subscribed]
        val cgtReference            = sample[CgtReference]
        val upscanInitiateReference = UpscanInitiateReference("some-upscan-ref")
        val upscanFileDescriptor    = sample[UpscanFileDescriptor].copy(cgtReference = cgtReference)

        val subscribedDetails    = subscribed.subscribedDetails
        val newSubscribedDetails = subscribedDetails.copy(cgtReference = cgtReference)

        val mp = MultipartFormData[Files.TemporaryFile](
          dataParts = Map("reference" -> Seq("some-upscan-ref")),
          files     = Seq.empty,
          badParts  = Seq.empty
        )
        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed.copy(subscribedDetails = newSubscribedDetails)))

        val userFile =
          mp.files
            .map(file => file.copy(ref = FileIO.fromPath(file.ref.path): Source[ByteString, Any]))

        val prepared: MultipartFormData[Source[ByteString, Any]] =
          mp.copy(
            files = userFile,
            dataParts = upscanFileDescriptor.fileDescriptor.uploadRequest.fields
              .mapValues(fieldValue => Seq(fieldValue))
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
          mockGetUpscanFileDescriptor(cgtReference, upscanInitiateReference)(Right(Some(upscanFileDescriptor)))
          mockUpload(upscanFileDescriptor.fileDescriptor.uploadRequest.href, prepared)(Right(Unit))
          mockUpdateUpscanFileDescriptorStatus(upscanFileDescriptor.copy(status = UPLOADED))(Right(Unit))
        }

        def performAction(): Future[Result] =
          controller.upload()(FakeRequest().withCSRFToken.withBody[MultipartFormData[Files.TemporaryFile]](mp))

        val result = performAction()

        status(result) shouldBe OK
      }

      "return error if there upload a file to S3 is unsuccessful" in {
        val subscribed              = sample[Subscribed]
        val cgtReference            = sample[CgtReference]
        val upscanInitiateReference = UpscanInitiateReference("some-upscan-ref")

        val subscribedDetails    = subscribed.subscribedDetails
        val newSubscribedDetails = subscribedDetails.copy(cgtReference = cgtReference)

        val mp = MultipartFormData[Files.TemporaryFile](
          dataParts = Map("reference" -> Seq("some-upscan-ref")),
          files     = Seq.empty,
          badParts  = Seq.empty
        )
        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed.copy(subscribedDetails = newSubscribedDetails)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
          mockGetUpscanFileDescriptor(cgtReference, upscanInitiateReference)(
            Left(Error("error getting file descriptor"))
          )
        }

        def performAction(): Future[Result] =
          controller.upload()(FakeRequest().withCSRFToken.withBody[MultipartFormData[Files.TemporaryFile]](mp))

        val result = performAction()

        status(result) shouldBe INTERNAL_SERVER_ERROR
      }

      "return the upscan file descriptor information on a successful call" in {
        val subscribed   = sample[Subscribed]
        val cgtReference = sample[CgtReference]

        val subscribedDetails    = subscribed.subscribedDetails
        val newSubscribedDetails = subscribedDetails.copy(cgtReference = cgtReference)

        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed.copy(subscribedDetails = newSubscribedDetails)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        mockUpscanInitiate(cgtReference)(Right(UpscanInititateResponseStored("")))

        def performAction(): Future[Result] = controller.upscan()(requestWithCSRFToken)
        val result                          = performAction()
        status(result) shouldBe OK
      }

      "return technical error page if the upscan call fails" in {
        val subscribed   = sample[Subscribed]
        val cgtReference = sample[CgtReference]

        val subscribedDetails    = subscribed.subscribedDetails
        val newSubscribedDetails = subscribedDetails.copy(cgtReference = cgtReference)

        val sessionData =
          SessionData.empty.copy(journeyStatus = Some(subscribed.copy(subscribedDetails = newSubscribedDetails)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }
        mockUpscanInitiate(CgtReference(cgtReference.value))(Left(Error("Some error")))
        def performAction(): Future[Result] = controller.upscan()(requestWithCSRFToken)
        checkIsTechnicalErrorPage(performAction())
      }

    }

  }

}
