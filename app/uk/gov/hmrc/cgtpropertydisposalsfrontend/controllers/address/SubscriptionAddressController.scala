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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.address

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.mvc._
import shapeless.{Lens, lens}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.audit.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubscriptionAddressController @Inject()(
  val errorHandler: ErrorHandler,
  val ukAddressLookupService: UKAddressLookupService,
  val sessionStore: SessionStore,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  cc: MessagesControllerComponents,
  val auditService: AuditService,
  val enterPostcodePage: views.html.address.enter_postcode,
  val selectAddressPage: views.html.address.select_address,
  val addressDisplay: views.html.components.address_display,
  val enterUkAddressPage: views.html.address.enter_uk_address,
  val enterNonUkAddressPage: views.html.address.enter_nonUk_address,
  val isUkPage: views.html.address.isUk
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with Logging
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with AddressController[SubscriptionReady] {

  override val isSubscribedJourney: Boolean = false

  val subscriptionReadyAddressLens: Lens[SubscriptionReady, Address] =
    lens[SubscriptionReady].subscriptionDetails.address

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, SubscriptionReady)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, s: SubscriptionReady)) => Right(sessionData -> s)
      case _                                         => Left(Redirect(controllers.routes.StartController.start()))
    }

  def updateAddress(journey: SubscriptionReady, address: Address, isManuallyEnteredAddress: Boolean)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, SubscriptionReady] = {
    val auditPath = address match {
      case Address.UkAddress(line1, line2, town, county, postcode) =>
        routes.SubscriptionAddressController.enterUkAddressSubmit().url
      case Address.NonUkAddress(line1, line2, line3, line4, postcode, country) =>
        routes.SubscriptionAddressController.enterNonUkAddressSubmit().url
    }
    auditService.sendSubscriptionContactAddressChangedEvent(
      journey.subscriptionDetails.address,
      address,
      isManuallyEnteredAddress,
      auditPath
    )
    EitherT.pure[Future, Error](subscriptionReadyAddressLens.set(journey)(address))
  }

  override val updateSubscriptionDetailChangedFlag: Boolean = false

  protected lazy val backLinkCall: Call                = controllers.routes.SubscriptionController.checkYourDetails()
  protected lazy val isUkCall: Call                    = routes.SubscriptionAddressController.isUk()
  protected lazy val isUkSubmitCall: Call              = routes.SubscriptionAddressController.isUkSubmit()
  protected lazy val enterUkAddressCall: Call          = routes.SubscriptionAddressController.enterUkAddress()
  protected lazy val enterUkAddressSubmitCall: Call    = routes.SubscriptionAddressController.enterUkAddressSubmit()
  protected lazy val enterNonUkAddressCall: Call       = routes.SubscriptionAddressController.enterNonUkAddress()
  protected lazy val enterNonUkAddressSubmitCall: Call = routes.SubscriptionAddressController.enterNonUkAddressSubmit()
  protected lazy val enterPostcodeCall: Call           = routes.SubscriptionAddressController.enterPostcode()
  protected lazy val enterPostcodeSubmitCall: Call     = routes.SubscriptionAddressController.enterPostcodeSubmit()
  protected lazy val selectAddressCall: Call           = routes.SubscriptionAddressController.selectAddress()
  protected lazy val selectAddressSubmitCall: Call     = routes.SubscriptionAddressController.selectAddressSubmit()
  protected lazy val continueCall: Call                = controllers.routes.SubscriptionController.checkYourDetails()

}
