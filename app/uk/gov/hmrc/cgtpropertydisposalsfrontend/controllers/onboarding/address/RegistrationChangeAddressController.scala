/*
 * Copyright 2020 HM Revenue & Customs
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
import com.google.inject.{Inject, Singleton}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.RegistrationReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{AuditAddress, RegistrationContactAddressChangedEvent}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, UKAddressLookupService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RegistrationChangeAddressController @Inject()(
  val errorHandler: ErrorHandler,
  val ukAddressLookupService: UKAddressLookupService,
  val sessionStore: SessionStore,
  val auditService: AuditService,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  cc: MessagesControllerComponents,
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
    with AddressController[RegistrationReady] {

  override val isSubscribedJourney: Boolean = false

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, RegistrationReady)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, r: RegistrationReady)) => Right(sessionData -> r)
      case _                                         => Left(Redirect(controllers.routes.StartController.start()))
    }

  def updateAddress(journey: RegistrationReady, address: Address, isManuallyEnteredAddress: Boolean)(
    implicit hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, RegistrationReady] = {
    auditService.sendEvent(
      "registrationContactAddressChanged",
      RegistrationContactAddressChangedEvent(
        AuditAddress.fromAddress(journey.registrationDetails.address),
        AuditAddress.fromAddress(address),
        if (isManuallyEnteredAddress) "manual-entry" else "postcode-lookup"
      ),
      "registration-contact-address-changed"
    )

    EitherT.pure[Future, Error](journey.copy(registrationDetails = journey.registrationDetails.copy(address = address)))
  }

  protected lazy val backLinkCall: Call             = controllers.onboarding.routes.RegistrationController.checkYourAnswers()
  protected lazy val isUkCall: Call                 = routes.RegistrationChangeAddressController.isUk()
  protected lazy val isUkSubmitCall: Call           = routes.RegistrationChangeAddressController.isUkSubmit()
  protected lazy val enterUkAddressCall: Call       = routes.RegistrationChangeAddressController.enterUkAddress()
  protected lazy val enterUkAddressSubmitCall: Call = routes.RegistrationChangeAddressController.enterUkAddressSubmit()
  protected lazy val enterNonUkAddressCall: Call    = routes.RegistrationChangeAddressController.enterNonUkAddress()
  protected lazy val enterNonUkAddressSubmitCall: Call =
    routes.RegistrationChangeAddressController.enterNonUkAddressSubmit()
  protected lazy val enterPostcodeCall: Call       = routes.RegistrationChangeAddressController.enterPostcode()
  protected lazy val enterPostcodeSubmitCall: Call = routes.RegistrationChangeAddressController.enterPostcodeSubmit()
  protected lazy val selectAddressCall: Call       = routes.RegistrationChangeAddressController.selectAddress()
  protected lazy val selectAddressSubmitCall: Call = routes.RegistrationChangeAddressController.selectAddressSubmit()
  protected lazy val continueCall: Call            = controllers.onboarding.routes.RegistrationController.checkYourAnswers()
}
