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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionMissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailJourneyType.Onboarding.EnteringSubscriptionEmail
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.BusinessPartnerRecordGen.given

import java.util.UUID
import scala.concurrent.Future

class SubscriptionEnterEmailControllerSpec
    extends EmailControllerSpec[EnteringSubscriptionEmail]
    with SampledScalaCheck
    with RedirectToStartBehaviour {

  override def toJourneyStatus(
    journeyType: EnteringSubscriptionEmail
  ): JourneyStatus = journeyType.journey

  override val validJourneyStatus: EnteringSubscriptionEmail =
    EnteringSubscriptionEmail(
      SubscriptionMissingData(
        sample[BusinessPartnerRecord].copy(emailAddress = None),
        None,
        None,
        sample[GGCredId],
        None
      )
    )

  override val validVerificationCompleteJourneyStatus: EnteringSubscriptionEmail =
    EnteringSubscriptionEmail(
      SubscriptionMissingData(
        sample[BusinessPartnerRecord].copy(emailAddress = None),
        Some(sample[Email]),
        None,
        sample[GGCredId],
        None
      )
    )

  override def updateEmail(
    enteringSubscriptionEmail: EnteringSubscriptionEmail,
    email: Email
  ): EnteringSubscriptionEmail = {
    val journey = enteringSubscriptionEmail.journey
    EnteringSubscriptionEmail(
      journey.copy(businessPartnerRecord = journey.businessPartnerRecord.copy(emailAddress = Some(email)))
    )
  }

  override val mockUpdateEmail
    : Option[(EnteringSubscriptionEmail, EnteringSubscriptionEmail, Either[Error, Unit]) => Unit] = None

  override lazy val controller: SubscriptionEnterEmailController = instanceOf[SubscriptionEnterEmailController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: SubscriptionMissingData => true
        case _                          => false
      }
    )

  "SubscriptionEnterEmailController" when {

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
        ContactName(
          validJourneyStatus.journey.businessPartnerRecord.name
            .fold(_.value, n => n.makeSingleName)
        ),
        emailRoutes.SubscriptionEnterEmailController.verifyEmail,
        emailRoutes.SubscriptionEnterEmailController.checkYourInbox()
      )
    }

    "handling requests to display the check your inbox page" must {

      def performAction(): Future[Result] =
        controller.checkYourInbox()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like checkYourInboxPage(
        () => performAction(),
        emailRoutes.SubscriptionEnterEmailController.enterEmail(),
        emailRoutes.SubscriptionEnterEmailController.enterEmail().url
      )
    }

    "handling requests to verify an email" must {

      def performAction(id: UUID): Future[Result] =
        controller.verifyEmail(id)(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction(UUID.randomUUID()))

      behave like verifyEmail(
        performAction,
        emailRoutes.SubscriptionEnterEmailController.enterEmail(),
        emailRoutes.SubscriptionEnterEmailController.emailVerified()
      )

    }

    "handling requests to display the email verified page" must {

      def performAction(): Future[Result] =
        controller.emailVerified()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like emailVerifiedPage(
        () => performAction(),
        controllers.routes.StartController.start(),
        emailRoutes.SubscriptionEnterEmailController.enterEmail()
      )
    }
  }

}
