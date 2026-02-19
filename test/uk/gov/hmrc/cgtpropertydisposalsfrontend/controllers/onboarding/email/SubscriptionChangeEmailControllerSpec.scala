/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.email

import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.MessagesApi
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.EmailControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.email.{routes => emailRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailJourneyType.Onboarding.ChangingSubscriptionEmail
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given

import java.util.UUID
import scala.concurrent.Future

class SubscriptionChangeEmailControllerSpec
    extends EmailControllerSpec[ChangingSubscriptionEmail]
    with SampledScalaCheck
    with RedirectToStartBehaviour {

  override def toJourneyStatus(
    journeyType: ChangingSubscriptionEmail
  ): JourneyStatus = journeyType.journey

  override val validJourneyStatus: ChangingSubscriptionEmail =
    ChangingSubscriptionEmail(sample[SubscriptionReady])

  override val validVerificationCompleteJourneyStatus: ChangingSubscriptionEmail =
    validJourneyStatus

  override def updateEmail(
    changingSubscriptionEmail: ChangingSubscriptionEmail,
    email: Email
  ): ChangingSubscriptionEmail = {
    val journey = changingSubscriptionEmail.journey
    ChangingSubscriptionEmail(
      journey.copy(
        subscriptionDetails = journey.subscriptionDetails.copy(
          emailAddress = email,
          emailSource = EmailSource.ManuallyEntered
        )
      )
    )
  }

  override val mockUpdateEmail
    : Option[(ChangingSubscriptionEmail, ChangingSubscriptionEmail, Either[Error, Unit]) => Unit] =
    None

  override lazy val controller: SubscriptionChangeEmailController = instanceOf[SubscriptionChangeEmailController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: SubscriptionReady => true
        case _                    => false
      }
    )

  "SubscriptionChangeEmailController" when {

    "handling requests to display the enter email page" must {

      def performAction(): Future[Result] =
        controller.enterEmail()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())
      behave like enterEmailPage(() => performAction())

    }

    "handling submitted email addresses" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.enterEmailSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like enterEmailSubmit(
        performAction,
        validJourneyStatus.journey.subscriptionDetails.contactName,
        emailRoutes.SubscriptionChangeEmailController.verifyEmail,
        emailRoutes.SubscriptionChangeEmailController.checkYourInbox()
      )
    }

    "handling requests to display the check your inbox page" must {

      def performAction(): Future[Result] =
        controller.checkYourInbox()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like checkYourInboxPage(
        () => performAction(),
        emailRoutes.SubscriptionChangeEmailController.enterEmail(),
        emailRoutes.SubscriptionChangeEmailController.enterEmail().url
      )
    }

    "handling requests to verify an email" must {

      def performAction(id: UUID): Future[Result] =
        controller.verifyEmail(id)(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction(UUID.randomUUID()))

      behave like verifyEmail(
        performAction,
        emailRoutes.SubscriptionChangeEmailController.enterEmail(),
        emailRoutes.SubscriptionChangeEmailController.emailVerified()
      )

    }

    "handling requests to display the email verified page" must {

      def performAction(): Future[Result] =
        controller.emailVerified()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like emailVerifiedPage(
        () => performAction(),
        controllers.onboarding.routes.SubscriptionController.checkYourDetails(),
        emailRoutes.SubscriptionChangeEmailController.enterEmail()
      )
    }
  }

}
