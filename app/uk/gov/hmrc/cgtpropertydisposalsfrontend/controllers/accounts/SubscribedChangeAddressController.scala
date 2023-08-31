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
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.mvc.{Call, MessagesControllerComponents, Request, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedUpdateDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{AuditAddress, SubscribedContactAddressChangedEvent}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, UKAddressLookupService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.ManagingSubscription.SubscribedAddressJourney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class SubscribedChangeAddressController @Inject() (
  val errorHandler: ErrorHandler,
  val ukAddressLookupService: UKAddressLookupService,
  val sessionStore: SessionStore,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val auditService: AuditService,
  subscriptionService: SubscriptionService,
  cc: MessagesControllerComponents,
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
    with AddressController[SubscribedAddressJourney] {

  override val toJourneyStatus: SubscribedAddressJourney => JourneyStatus =
    _.journey

  def isATrust(journey: SubscribedAddressJourney): Boolean =
    journey.journey.subscribedDetails.isATrust

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Future[Result], (SessionData, SubscribedAddressJourney)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, s: Subscribed)) =>
        Right(sessionData -> SubscribedAddressJourney(s))
      case _                                  => Left(Redirect(controllers.routes.StartController.start()))
    }

  def updateAddress(
    journey: SubscribedAddressJourney,
    address: Address,
    isManuallyEnteredAddress: Boolean
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, JourneyStatus] = {
    val updatedSubscribedDetails =
      journey.journey.subscribedDetails.copy(address = address)

    if (journey.journey.subscribedDetails === updatedSubscribedDetails) {
      EitherT.pure[Future, Error](journey.journey)
    } else {
      auditService.sendEvent(
        "contactAddressChanged",
        SubscribedContactAddressChangedEvent(
          AuditAddress.fromAddress(journey.journey.subscribedDetails.address),
          AuditAddress.fromAddress(address),
          if (isManuallyEnteredAddress) "manual-entry" else "postcode-lookup",
          journey.journey.subscribedDetails.cgtReference.value,
          journey.journey.agentReferenceNumber.isDefined,
          journey.journey.agentReferenceNumber.map(_.value)
        ),
        "contact-address-changed"
      )
      subscriptionService
        .updateSubscribedDetails(
          SubscribedUpdateDetails(
            updatedSubscribedDetails,
            journey.journey.subscribedDetails
          )
        )
        .map(_ => journey.journey.copy(subscribedDetails = updatedSubscribedDetails))
    }
  }

  protected lazy val backLinkCall: SubscribedAddressJourney => Call =
    _ => controllers.accounts.routes.AccountController.manageYourDetails()

  protected lazy val isUkCall: Call                                =
    routes.SubscribedChangeAddressController.isUk()
  protected lazy val isUkSubmitCall: Call                          =
    routes.SubscribedChangeAddressController.isUkSubmit()
  protected lazy val enterUkAddressCall: Call                      =
    routes.SubscribedChangeAddressController.enterUkAddress()
  protected lazy val enterUkAddressSubmitCall: Call                =
    routes.SubscribedChangeAddressController.enterUkAddressSubmit()
  protected lazy val enterNonUkAddressCall: Call                   =
    routes.SubscribedChangeAddressController.enterNonUkAddress()
  protected lazy val enterNonUkAddressSubmitCall: Call             =
    routes.SubscribedChangeAddressController.enterNonUkAddressSubmit()
  protected lazy val enterPostcodeCall: Call                       =
    routes.SubscribedChangeAddressController.enterPostcode()
  protected lazy val enterPostcodeSubmitCall: Call                 =
    routes.SubscribedChangeAddressController.enterPostcodeSubmit()
  protected lazy val selectAddressCall: Call                       =
    routes.SubscribedChangeAddressController.selectAddress()
  protected lazy val selectAddressSubmitCall: Call                 =
    routes.SubscribedChangeAddressController.selectAddressSubmit()
  protected lazy val continueCall: Call                            =
    controllers.accounts.routes.AccountController.contactAddressUpdated()
  protected lazy val ukAddressNotAllowedExitPageCall: Option[Call] =
    Some(routes.SubscribedChangeAddressController.showExitPage())
}
