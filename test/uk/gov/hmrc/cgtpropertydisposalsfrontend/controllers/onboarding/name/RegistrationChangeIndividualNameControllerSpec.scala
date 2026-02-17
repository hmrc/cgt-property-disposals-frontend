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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.RegistrationReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given

class RegistrationChangeIndividualNameControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with IndividualNameControllerSpec[RegistrationReady]
    with SampledScalaCheck {

  def isValidJourney(journey: JourneyStatus): Boolean =
    journey match {
      case _: RegistrationReady => true
      case _                    => false
    }

  override val mockUpdateName: Option[(RegistrationReady, RegistrationReady, Either[Error, Unit]) => Unit] = None

  override val controller: RegistrationChangeIndividualNameController =
    instanceOf[RegistrationChangeIndividualNameController]

  override val validJourney: RegistrationReady = sample[RegistrationReady]

  override def updateName(
    name: IndividualName,
    journey: RegistrationReady
  ): RegistrationReady =
    journey.copy(registrationDetails = journey.registrationDetails.copy(name = name))

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  "RegistrationEnterIndividualNameController" when {

    "handling requests to display the enter name page" must {

      behave like enterNamePage(() => controller.enterIndividualName()(FakeRequest()))

    }

    "handling submitted names" must {

      behave like enterNameSubmit(
        data =>
          controller.enterIndividualNameSubmit()(
            FakeRequest().withFormUrlEncodedBody(data*).withCSRFToken.withMethod("POST")
          ),
        controllers.onboarding.routes.RegistrationController.checkYourAnswers()
      )
    }

  }

}
