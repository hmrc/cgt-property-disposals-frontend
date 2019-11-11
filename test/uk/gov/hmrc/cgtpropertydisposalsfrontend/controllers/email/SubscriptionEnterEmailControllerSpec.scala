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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.email

import java.util.UUID

import org.scalacheck.ScalacheckShapeless._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionMissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Email, Error, sample}

import scala.concurrent.Future

class SubscriptionEnterEmailControllerSpec
    extends EmailControllerSpec[SubscriptionMissingData, SubscriptionMissingData]
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override val isAmendJourney: Boolean = false

  override val validJourneyStatus: SubscriptionMissingData =
    SubscriptionMissingData(sample[BusinessPartnerRecord].copy(emailAddress = None))

  override val validVerificationCompleteJourneyStatus: SubscriptionMissingData =
    SubscriptionMissingData(sample[BusinessPartnerRecord].copy(emailAddress = Some(sample[Email])))

  override def updateEmail(journey: SubscriptionMissingData, email: Email): SubscriptionMissingData =
    journey.copy(businessPartnerRecord = journey.businessPartnerRecord.copy(emailAddress = Some(email)))

  override val mockUpdateEmail: Option[(SubscriptionMissingData, SubscriptionMissingData, Either[Error, Unit]) => Unit] = None

  override lazy val controller: SubscriptionEnterEmailController = instanceOf[SubscriptionEnterEmailController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: SubscriptionMissingData => true
        case _                          => false
      }
    )

  "SubscriptionEnterEmailController" when {

    "handling requests to display the enter email page" must {

      def performAction(): Future[Result] =
        controller.enterEmail()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)
      behave like enterEmailPage(performAction)
    }

    "handling submitted email addresses" must {

      def requestWithFormData(data: (String, String)*) =
        FakeRequest().withFormUrlEncodedBody(data: _*).withCSRFToken

      def performAction(key: String, value: String): Future[Result] =
        controller.enterEmailSubmit()(requestWithFormData(key -> value))

      behave like redirectToStartBehaviour(() => performAction("", ""))

      behave like enterEmailSubmit(
        performAction,
        ContactName(validJourneyStatus.businessPartnerRecord.name.fold(_.value, n => n.makeSingleName())),
        routes.SubscriptionEnterEmailController.verifyEmail,
        routes.SubscriptionEnterEmailController.checkYourInbox()
      )
    }

    "handling requests to display the check your inbox page" must {

      def performAction(): Future[Result] =
        controller.checkYourInbox()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like checkYourInboxPage(
        performAction,
        routes.SubscriptionEnterEmailController.enterEmail(),
        routes.SubscriptionEnterEmailController.enterEmail().url
      )
    }

    "handling requests to verify an email" must {

      def performAction(id: UUID): Future[Result] =
        controller.verifyEmail(id)(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction(UUID.randomUUID()))

      behave like verifyEmail(
        performAction,
        routes.SubscriptionEnterEmailController.enterEmail(),
        routes.SubscriptionEnterEmailController.emailVerified()
      )

    }

    "handling requests to display the email verfied page" must {

      def performAction(): Future[Result] =
        controller.emailVerified()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like emailVerifiedPage(
        performAction,
        controllers.routes.StartController.start(),
        routes.SubscriptionEnterEmailController.enterEmail()
      )
    }
  }

}
