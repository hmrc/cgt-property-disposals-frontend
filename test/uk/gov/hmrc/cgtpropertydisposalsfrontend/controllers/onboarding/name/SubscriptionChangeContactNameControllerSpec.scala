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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.name

import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.MessagesApi
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ContactNameControllerSpec, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, ContactNameSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, models}

class SubscriptionChangeContactNameControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ContactNameControllerSpec[SubscriptionReady]
    with SampledScalaCheck {

  override val controller: SubscriptionChangeContactNameController =
    instanceOf[SubscriptionChangeContactNameController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  override val validJourney: SubscriptionReady = {
    val subscriptionReady = sample[SubscriptionReady]

    subscriptionReady.copy(subscriptionDetails =
      subscriptionReady.subscriptionDetails.copy(contactNameSource = ContactNameSource.DerivedFromBusinessPartnerRecord)
    )
  }

  override val mockUpdateContactName
    : Option[(SubscriptionReady, SubscriptionReady, Either[models.Error, Unit]) => Unit] = None

  override def updateContactName(
    journey: SubscriptionReady,
    contactName: ContactName
  ): SubscriptionReady =
    journey.copy(
      subscriptionDetails = journey.subscriptionDetails
        .copy(
          contactName = contactName,
          contactNameSource = ContactNameSource.ManuallyEntered
        )
    )

  def isValidJourney(journey: JourneyStatus): Boolean =
    journey match {
      case _: SubscriptionReady => true
      case _                    => false
    }

  "SubscriptionEnterContactNameController" when {

    "handling requests to display the enter contact name page" must {
      behave like enterContactNamePage(() => controller.enterContactName()(FakeRequest()))
    }

    "handling submitted names" must {
      behave like enterContactNameSubmit(
        data =>
          controller.enterContactNameSubmit()(
            FakeRequest().withFormUrlEncodedBody(data*).withCSRFToken.withMethod("POST")
          ),
        controllers.onboarding.routes.SubscriptionController.checkYourDetails()
      )
    }

  }

}
