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

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.EmailControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.email.{routes => emailRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualMissingEmail, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailJourneyType.Onboarding.EnteringRegistrationEmail
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.RegistrationDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus}

import java.util.UUID
import scala.concurrent.Future

class RegistrationEnterEmailControllerSpec
    extends EmailControllerSpec[EnteringRegistrationEmail]
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override def toJourneyStatus(
    journeyType: EnteringRegistrationEmail
  ): JourneyStatus = journeyType.journey.merge

  val individualMissingEmail = sample[IndividualMissingEmail]

  override val validJourneyStatus: EnteringRegistrationEmail =
    EnteringRegistrationEmail(Right(individualMissingEmail))

  override val validVerificationCompleteJourneyStatus: EnteringRegistrationEmail =
    EnteringRegistrationEmail(Left(sample[RegistrationReady]))

  override def updateEmail(
    enteringRegistrationEmail: EnteringRegistrationEmail,
    email: Email
  ): EnteringRegistrationEmail =
    EnteringRegistrationEmail(
      Left(
        RegistrationReady(
          RegistrationDetails(
            individualMissingEmail.name,
            email,
            individualMissingEmail.address,
            EmailSource.ManuallyEntered
          ),
          individualMissingEmail.ggCredId
        )
      )
    )

  override val mockUpdateEmail: Option[
    (
      EnteringRegistrationEmail,
      EnteringRegistrationEmail,
      Either[Error, Unit]
    ) => Unit
  ] =
    None

  override lazy val controller: RegistrationEnterEmailController =
    instanceOf[RegistrationEnterEmailController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: IndividualMissingEmail => true
        case _                         => false
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

      def performAction(data: (String, String)*): Future[Result] =
        controller.enterEmailSubmit()(
          FakeRequest().withFormUrlEncodedBody(data: _*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like enterEmailSubmit(
        performAction,
        ContactName(individualMissingEmail.name.makeSingleName()),
        emailRoutes.RegistrationEnterEmailController.verifyEmail,
        emailRoutes.RegistrationEnterEmailController.checkYourInbox()
      )
    }

    "handling requests to display the check your inbox page" must {

      def performAction(): Future[Result] =
        controller.checkYourInbox()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like checkYourInboxPage(
        performAction,
        emailRoutes.RegistrationEnterEmailController.enterEmail(),
        emailRoutes.RegistrationEnterEmailController.enterEmail().url
      )
    }

    "handling requests to verify an email" must {

      def performAction(id: UUID): Future[Result] =
        controller.verifyEmail(id)(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction(UUID.randomUUID()))

      behave like verifyEmail(
        performAction,
        emailRoutes.RegistrationEnterEmailController.enterEmail(),
        emailRoutes.RegistrationEnterEmailController.emailVerified()
      )

    }

    "handling requests to display the email verified page" must {

      def performAction(): Future[Result] =
        controller.emailVerified()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        {
          case _: RegistrationReady => true
          case _                    => false
        }
      )

      behave like emailVerifiedPage(
        performAction,
        controllers.onboarding.routes.RegistrationController.checkYourAnswers(),
        emailRoutes.RegistrationEnterEmailController.enterEmail()
      )
    }
  }

}
