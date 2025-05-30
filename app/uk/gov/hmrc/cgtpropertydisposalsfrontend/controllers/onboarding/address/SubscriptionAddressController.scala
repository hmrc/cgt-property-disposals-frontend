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
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, AddressSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{AuditAddress, SubscriptionContactAddressChangedEvent}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, UKAddressLookupService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, given}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Onboarding.SubscriptionReadyAddressJourney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubscriptionAddressController @Inject() (
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
    with AddressController[SubscriptionReadyAddressJourney] {

  override val toJourneyStatus: SubscriptionReadyAddressJourney => JourneyStatus = _.journey

  def isATrust(journey: SubscriptionReadyAddressJourney): Boolean =
    journey.journey.subscriptionDetails.name.isLeft

  def validJourney(
    request: RequestWithSessionData[?]
  ): Either[Future[Result], (SessionData, SubscriptionReadyAddressJourney)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, s: SubscriptionReady)) =>
        Right(sessionData -> SubscriptionReadyAddressJourney(s))
      case _                                         => Left(Redirect(controllers.routes.StartController.start()))
    }

  def updateAddress(
    journey: SubscriptionReadyAddressJourney,
    address: Address,
    isManuallyEnteredAddress: Boolean
  )(implicit
    hc: HeaderCarrier,
    request: Request[?]
  ): EitherT[Future, Error, JourneyStatus] = {
    auditService.sendEvent(
      "subscriptionContactAddressChanged",
      SubscriptionContactAddressChangedEvent(
        AuditAddress.fromAddress(journey.journey.subscriptionDetails.address),
        AuditAddress.fromAddress(address),
        if (isManuallyEnteredAddress) "manual-entry" else "postcode-lookup"
      ),
      "subscription-contact-address-changed"
    )

    val addressSource =
      if (address === journey.journey.subscriptionDetails.address) {
        journey.journey.subscriptionDetails.addressSource
      } else {
        AddressSource.ManuallyEntered
      }

    EitherT.pure[Future, Error](
      journey.journey.copy(subscriptionDetails =
        journey.journey.subscriptionDetails.copy(address = address, addressSource = addressSource)
      )
    )
  }

  protected lazy val backLinkCall: SubscriptionReadyAddressJourney => Call =
    _ => controllers.onboarding.routes.SubscriptionController.checkYourDetails()

  protected lazy val isUkCall: Call                                =
    routes.SubscriptionAddressController.isUk()
  protected lazy val isUkSubmitCall: Call                          =
    routes.SubscriptionAddressController.isUkSubmit()
  protected lazy val enterUkAddressCall: Call                      =
    routes.SubscriptionAddressController.enterUkAddress()
  protected lazy val enterUkAddressSubmitCall: Call                =
    routes.SubscriptionAddressController.enterUkAddressSubmit()
  protected lazy val enterNonUkAddressCall: Call                   =
    routes.SubscriptionAddressController.enterNonUkAddress()
  protected lazy val enterNonUkAddressSubmitCall: Call             =
    routes.SubscriptionAddressController.enterNonUkAddressSubmit()
  protected lazy val enterPostcodeCall: Call                       =
    routes.SubscriptionAddressController.enterPostcode()
  protected lazy val enterPostcodeSubmitCall: Call                 =
    routes.SubscriptionAddressController.enterPostcodeSubmit()
  protected lazy val selectAddressCall: Call                       =
    routes.SubscriptionAddressController.selectAddress()
  protected lazy val selectAddressSubmitCall: Call                 =
    routes.SubscriptionAddressController.selectAddressSubmit()
  protected lazy val ukAddressNotAllowedExitPageCall: Option[Call] =
    None
  protected lazy val continueCall: Call                            =
    controllers.onboarding.routes.SubscriptionController.checkYourDetails()

}
