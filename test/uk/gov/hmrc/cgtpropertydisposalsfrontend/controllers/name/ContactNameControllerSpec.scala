/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.name

import org.scalacheck.ScalacheckShapeless._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ContactNameFormValidationTests, ControllerSpec, RedirectToStartBehaviour, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData, SubscriptionDetail, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.SubscriptionService

import scala.concurrent.Future

trait ContactNameControllerSpec[J <: JourneyStatus]
    extends ContactNameFormValidationTests
    with RedirectToStartBehaviour {
  this: ControllerSpec with AuthSupport with SessionSupport with ScalaCheckDrivenPropertyChecks =>

  val controller: ContactNameController[J]

  val validJourney: J

  lazy val sessionDataWithValidJourney = SessionData.empty.copy(journeyStatus = Some(validJourney))

  def updateContactName(journey: J, contactName : ContactName): J

  val mockUpdateContactName: Option[(J, Either[Error, Unit]) => Unit]

  val updateSubscriptionDetailUpdated: Boolean

  def isValidJourney(journey: JourneyStatus): Boolean

  override val overrideBindings: List[GuiceableModule] =
    List(
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[SubscriptionService].toInstance(mockSubscriptionService)
    )

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(performAction, isValidJourney)

  def enterContactNamePage(
    performAction: () => Future[Result]
  )(implicit messagesApi: MessagesApi): Unit = {
    behave like redirectToStartBehaviour(performAction)

    "show the page" when {
      "the endpoint is requested" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionDataWithValidJourney))))
        }
        val result = performAction()
        status(result)          shouldBe OK
        contentAsString(result) should include(message("contactName.title"))
      }

      "the endpoint is requested and the user has previously entered a contact name" in {
        val contactName = sample[ContactName]
        val sessionDataWithName =
          sessionDataWithValidJourney.copy(journeyStatus = Some(updateContactName(validJourney,contactName)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionDataWithName))))
        }
        val result = performAction()
        status(result) shouldBe OK

        val content = contentAsString(result)
        content should include(message("contactName.title"))
        content should include(contactName.value)
      }

    }
  }

  def enterContactNameSubmit(
    performAction: Seq[(String, String)] => Future[Result],
    continueCall: Call
  )(implicit messagesApi: MessagesApi): Unit = {
    val contactName = ContactName("Joe Smith")
    val updatedSession =
      sessionDataWithValidJourney.copy(
        journeyStatus = Some(updateContactName(validJourney,contactName)),
        subscriptionDetailUpdated = if(updateSubscriptionDetailUpdated) Some(SubscriptionDetail.Name) else None
      )

    behave like redirectToStartBehaviour(() => performAction(Seq.empty))

    behave like contactNameFormValidationTests(
      performAction,
      () =>
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionDataWithValidJourney))))
        }
    )

    s"redirect to ${continueCall.url}" when {

      "the request submits valid values" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionDataWithValidJourney))))
          mockUpdateContactName.foreach { f =>
            f(updateContactName(validJourney,contactName), Right(()))
          }
          mockStoreSession(updatedSession)(Future.successful(Right(())))
        }

        val result = performAction(Seq("contactName" -> contactName.value))
        checkIsRedirect(result, continueCall)
      }

      "request submits valid values with leading and trailing spaces" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionDataWithValidJourney))))
          mockUpdateContactName.foreach { f =>
            f(updateContactName(validJourney,contactName), Right(()))
          }
          mockStoreSession(updatedSession)(Future.successful(Right(())))
        }

        val result = performAction(Seq("contactName" -> s"${contactName.value} "))
        checkIsRedirect(result, continueCall)
      }

    }

    "not update the session" when {

      "the contact name submitted is the same in the session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(updatedSession))))
        }
        val result = performAction(Seq("contactName" -> contactName.value))
        checkIsRedirect(result, continueCall)
      }

    }

    "display an error page" when {
      "the session cannot be updated" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Future.successful(Right(Some(sessionDataWithValidJourney))))
          mockUpdateContactName.foreach { f =>
            f(updateContactName(validJourney,contactName), Right(()))
          }
          mockStoreSession(updatedSession)(Future.successful(Left(Error(""))))
        }

        checkIsTechnicalErrorPage(performAction(Seq("contactName" -> contactName.value)))
      }
    }

  }

}
