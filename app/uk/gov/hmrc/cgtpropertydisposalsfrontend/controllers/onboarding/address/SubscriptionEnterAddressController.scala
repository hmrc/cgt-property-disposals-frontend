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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.address

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.Inject
import play.api.mvc.{Call, MessagesControllerComponents, Request, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionMissingData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{AuditAddress, SubscriptionSetupContactAddressEvent}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, UKAddressLookupService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Onboarding.SubscriptionEnterAddressJourney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class SubscriptionEnterAddressController @Inject() (
  val errorHandler: ErrorHandler,
  val ukAddressLookupService: UKAddressLookupService,
  val sessionStore: SessionStore,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  cc: MessagesControllerComponents,
  val auditService: AuditService,
  val enterPostcodePage: views.html.address.enter_postcode,
  val selectAddressPage: views.html.address.select_address,
  val enterUkAddressPage: views.html.address.enter_uk_address,
  val enterNonUkAddressPage: views.html.address.enter_nonUk_address,
  val isUkPage: views.html.address.isUk,
  val exitPage: views.html.address.exit_page
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with Logging
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with AddressController[SubscriptionEnterAddressJourney] {

  override val toJourneyStatus: SubscriptionEnterAddressJourney => JourneyStatus = _.journey

  def isATrust(journey: SubscriptionEnterAddressJourney): Boolean =
    journey.journey.businessPartnerRecord.name.isLeft

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Future[Result], (SessionData, SubscriptionEnterAddressJourney)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, s: SubscriptionMissingData)) => Right(sessionData -> SubscriptionEnterAddressJourney(s))
      case _                                               => Left(Redirect(controllers.routes.StartController.start()))
    }

  def updateAddress(
    journey: SubscriptionEnterAddressJourney,
    address: Address,
    isManuallyEnteredAddress: Boolean
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, JourneyStatus] = {
    auditService.sendEvent(
      "subscriptionSetupContactAddress",
      SubscriptionSetupContactAddressEvent(
        AuditAddress.fromAddress(address)
      ),
      "subscription-setup-contact-address"
    )

    EitherT.pure[Future, Error](
      journey.journey.copy(
        manuallyEnteredAddress = Some(address)
      )
    )
  }

  protected lazy val backLinkCall: SubscriptionEnterAddressJourney => Call =
    _ => controllers.routes.StartController.weNeedMoreDetails()

  protected lazy val isUkCall: Call =
    routes.SubscriptionEnterAddressController.isUk()

  protected lazy val isUkSubmitCall: Call =
    routes.SubscriptionEnterAddressController.isUkSubmit()

  protected lazy val enterUkAddressCall: Call =
    routes.SubscriptionEnterAddressController.enterUkAddress()

  protected lazy val enterUkAddressSubmitCall: Call =
    routes.SubscriptionEnterAddressController.enterUkAddressSubmit()

  protected lazy val enterNonUkAddressCall: Call =
    routes.SubscriptionEnterAddressController.enterNonUkAddress()

  protected lazy val enterNonUkAddressSubmitCall: Call =
    routes.SubscriptionEnterAddressController.enterNonUkAddressSubmit()

  protected lazy val enterPostcodeCall: Call =
    routes.SubscriptionEnterAddressController.enterPostcode()

  protected lazy val enterPostcodeSubmitCall: Call =
    routes.SubscriptionEnterAddressController.enterPostcodeSubmit()

  protected lazy val selectAddressCall: Call =
    routes.SubscriptionEnterAddressController.selectAddress()

  protected lazy val selectAddressSubmitCall: Call =
    routes.SubscriptionEnterAddressController.selectAddressSubmit()

  protected lazy val ukAddressNotAllowedExitPageCall: Option[Call] =
    None

  protected lazy val continueCall: Call =
    controllers.routes.StartController.start()

}
