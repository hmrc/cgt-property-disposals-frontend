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

import cats.data.EitherT
import cats.instances.future._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SubscribedUpdateDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubscribedChangeContactNameControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ContactNameControllerSpec[Subscribed]
    with ScalaCheckDrivenPropertyChecks {

  def mockSubscriptionUpdate(subscribedUpdateDetails: SubscribedUpdateDetails)(result: Either[Error, Unit]) =
    (mockSubscriptionService
      .updateSubscribedDetails(_: SubscribedUpdateDetails)(_: HeaderCarrier))
      .expects(subscribedUpdateDetails, *)
      .returning(EitherT.fromEither[Future](result))

  override val controller: SubscribedChangeContactNameController = instanceOf[SubscribedChangeContactNameController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  override val validJourney: Subscribed = sample[Subscribed]

  override val mockUpdateContactName: Option[(Subscribed, Subscribed, Either[Error, Unit]) => Unit] = Some({
    case (oldDetails: Subscribed, newDetails : Subscribed, r: Either[Error, Unit]) =>
      mockSubscriptionUpdate(SubscribedUpdateDetails(newDetails.subscribedDetails, oldDetails.subscribedDetails))(r)
  })

  override def updateContactName(journey: Subscribed, contactName: ContactName): Subscribed =
    journey.copy(subscribedDetails = journey.subscribedDetails.copy(contactName = contactName))

  def isValidJourney(journey: JourneyStatus): Boolean = journey match {
    case _: Subscribed => true
    case _             => false
  }

  "SubscriptionEnterContactNameController" when {

    "handling requests to display the enter contact name page" must {
      behave like enterContactNamePage(
        () => controller.enterContactName()(FakeRequest())
      )
    }

    "handling submitted contact name" must {
      behave like enterContactNameSubmit(
        data => controller.enterContactNameSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*).withCSRFToken),
        controllers.routes.HomeController.contactNameUpdated()
      )
    }

  }

}
