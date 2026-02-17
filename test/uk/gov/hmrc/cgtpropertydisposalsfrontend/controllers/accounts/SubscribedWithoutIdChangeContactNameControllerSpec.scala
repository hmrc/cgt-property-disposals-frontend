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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts

import cats.data.EitherT
import cats.instances.future._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.MessagesApi
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.name.IndividualNameControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{SubscribedDetails, SubscribedUpdateDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubscribedWithoutIdChangeContactNameControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with IndividualNameControllerSpec[Subscribed]
    with SampledScalaCheck {

  private def mockSubscriptionUpdate(
    subscribedAndVerifierDetails: SubscribedUpdateDetails
  )(result: Either[Error, Unit]) =
    (mockSubscriptionService
      .updateSubscribedDetails(_: SubscribedUpdateDetails)(using _: HeaderCarrier))
      .expects(subscribedAndVerifierDetails, *)
      .returning(EitherT.fromEither[Future](result))

  def isValidJourney(journey: JourneyStatus): Boolean =
    journey match {
      case _: Subscribed => true
      case _             => false
    }

  override val controller: SubscribedWithoutIdChangeContactNameController =
    instanceOf[SubscribedWithoutIdChangeContactNameController]

  override val validJourney: Subscribed = Subscribed(
    SubscribedDetails(
      Right(IndividualName("Joe", "Smith")),
      Email("joe.smith@gmail.com"),
      UkAddress("21 Joe Street", None, None, None, Postcode("BN112JJ")),
      ContactName("Bob Smith"),
      CgtReference("XDCGT01234568798"),
      None,
      registeredWithId = false
    ),
    GGCredId("id"),
    None,
    List.empty,
    List.empty
  )

  override def updateName(
    name: IndividualName,
    journey: Subscribed
  ): Subscribed = {
    val contactName = s"${name.firstName} ${name.lastName}"
    journey.copy(
      subscribedDetails = journey.subscribedDetails
        .copy(name = Right(name), contactName = ContactName(contactName))
    )
  }

  override val mockUpdateName: Option[(Subscribed, Subscribed, Either[Error, Unit]) => Unit] = Some {
    case (
          oldDetails: Subscribed,
          newDetails: Subscribed,
          r: Either[Error, Unit]
        ) =>
      mockSubscriptionUpdate(
        SubscribedUpdateDetails(
          newDetails.subscribedDetails,
          oldDetails.subscribedDetails
        )
      )(r)
  }

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
        controllers.accounts.routes.AccountController.contactNameUpdated()
      )
    }

  }

}
