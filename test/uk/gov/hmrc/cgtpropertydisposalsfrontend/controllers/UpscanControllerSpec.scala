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

import cats.data.EitherT
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{status, _}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.UpscanServiceResponse.{UpscanDescriptor, UpscanRequest, UpscanResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService.{UpscanNotifyResponse, UpscanServiceResponse}
import uk.gov.hmrc.http.HeaderCarrier
import play.api.test.CSRFTokenHelper._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed

import scala.concurrent.Future

class UpscanControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockUpscanService = mock[UpscanService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[UpscanService].toInstance(mockUpscanService)
    )

  lazy val controller: UpscanController = instanceOf[UpscanController]

  val requestWithCSRFToken = FakeRequest().withCSRFToken

  def mockUpscanInitiate(cgtReference: CgtReference)(response: Either[Error, UpscanServiceResponse]) =
    (mockUpscanService
      .initiate(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT(Future.successful(response)))

  def mockStoreUpscanNotifyEvent(cgtReference: CgtReference, upscanNotifyResponse: UpscanNotifyResponse)(
    response: Either[Error, Unit]
  ) =
    (mockUpscanService
      .storeNotifyEvent(_: CgtReference, _: UpscanNotifyResponse)(_: HeaderCarrier))
      .expects(cgtReference, upscanNotifyResponse, *)
      .returning(EitherT(Future.successful(response)))

  "The UpscanController" when {

    implicit lazy val messagesApi: MessagesApi = controller.messagesApi

    val cgtReference = sample[CgtReference]

    "a callback request is received" should {

      "return a 400 if the body contains corrupt JSON" in {
        def performAction(): Future[Result] =
          controller.callBack(cgtReference.value).apply(FakeRequest().withBody(Json.parse("{}")))
        val result = performAction()
        status(result) shouldBe BAD_REQUEST
      }

      "return a 204 if the body contains valid JSON" in {
        val body =
          """
            | {
            |   "reference" : "upscan-ref",
            |   "fileStatus" : "READY",
            |   "downloadUrl" : "http://upscan.bucket",
            |   "uploadDetails" : {
            |       "field-1" : "value-1"
            |   }
            | }
            |""".stripMargin

        mockStoreUpscanNotifyEvent(
          cgtReference,
          UpscanNotifyResponse("upscan-ref", "READY", "http://upscan.bucket", Map("field-1" -> "value-1"))
        )(Right(Unit))
        def performAction(): Future[Result] =
          controller.callBack(cgtReference.value).apply(FakeRequest().withBody(Json.parse(body)))
        val result = performAction()
        status(result) shouldBe NO_CONTENT

      }

    }

    "a success redirect is received" should {
      "redirect to the upscan success page" in {
        def performAction(): Future[Result] = controller.successRedirect(cgtReference.value)(FakeRequest())
        val result = performAction()
        status(result) shouldBe OK
      }
    }

    "an error redirect is received" should {
      "redirect to the technical error page" in {
        def performAction(): Future[Result] = controller.errorRedirect(cgtReference.value)(FakeRequest())
        checkIsTechnicalErrorPage(performAction())
      }
    }

    "an upscan request is received" should {
      "return the upscan file descriptor information on a successful call" in {
        val subscribed = sample[Subscribed]
        val cgtReference = sample[CgtReference]

        val subscribedDetails = subscribed.subscribedDetails
        val newSubscribedDetails = subscribedDetails.copy(cgtReference = cgtReference)

        val sessionData = SessionData.empty.copy(journeyStatus = Some(subscribed.copy(subscribedDetails = newSubscribedDetails)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData))))
        }

        mockUpscanInitiate(cgtReference)(Right(UpscanResponse(cgtReference.value, UpscanDescriptor("href", UpscanRequest("href", Map.empty)))))
        def performAction(): Future[Result] = controller.upscan()(requestWithCSRFToken)
        val result = performAction()
        status(result) shouldBe OK
      }
      "return technical error page if the upscan call fails" in {
        val subscribed = sample[Subscribed]
        val cgtReference = sample[CgtReference]

        val subscribedDetails = subscribed.subscribedDetails
        val newSubscribedDetails = subscribedDetails.copy(cgtReference = cgtReference)

        val sessionData = SessionData.empty.copy(journeyStatus = Some(subscribed.copy(subscribedDetails = newSubscribedDetails)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionData))))
        }
        mockUpscanInitiate(CgtReference(cgtReference.value))(Left(Error("Some error")))
        def performAction(): Future[Result] = controller.upscan()(requestWithCSRFToken)
        checkIsTechnicalErrorPage(performAction())
      }

    }

  }

}
