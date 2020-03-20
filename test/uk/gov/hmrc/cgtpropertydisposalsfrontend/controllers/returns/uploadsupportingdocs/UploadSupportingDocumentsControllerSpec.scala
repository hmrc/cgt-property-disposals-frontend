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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.uploadsupportingdocs

import java.util.UUID

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{LocalDateUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class UploadSupportingDocumentsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[UploadSupportingDocumentsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def sessionWithState(
    uploadSupportingDocAnswer: Option[UploadSupportingDocuments],
    disposalDate: Option[DisposalDate]
  ): (SessionData, FillingOutReturn, SingleDisposalDraftReturn) = {

    val draftReturn = sample[SingleDisposalDraftReturn].copy(uploadSupportingDocuments = uploadSupportingDocAnswer)

    val journey = sample[FillingOutReturn].copy(draftReturn = draftReturn)
    (
      SessionData.empty.copy(journeyStatus = Some(journey)),
      journey,
      draftReturn
    )
  }

  def sessionWithState(
    uploadSupportingDocAnswer1: UploadSupportingDocuments,
    disposalDate: DisposalDate
  ): (SessionData, FillingOutReturn, SingleDisposalDraftReturn) =
    sessionWithState(Some(uploadSupportingDocAnswer1), Some(disposalDate))

  def draftReturnWithCompleteJourneys(
    uploadSupportingDocAnswer: Option[UploadSupportingDocuments],
    disposalDate: DisposalDate
  ) =
    SingleDisposalDraftReturn(
      UUID.randomUUID(),
      sample[CompleteSingleDisposalTriageAnswers].copy(disposalDate = disposalDate),
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      uploadSupportingDocAnswer,
      LocalDateUtils.today()
    )

  "Upload Supporting Documents Controller" when {

    "handling requests to display the do you want to upload supporting documents" must {

      def performAction(): Future[Result] = controller.hasSupportingDocsToUpload()(FakeRequest())

      "display the page" when {

        "the user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithState(None, Some(sample[DisposalDate]))._1)
          }

          status(performAction()) shouldBe 303

        }
      }
    }
  }

}
