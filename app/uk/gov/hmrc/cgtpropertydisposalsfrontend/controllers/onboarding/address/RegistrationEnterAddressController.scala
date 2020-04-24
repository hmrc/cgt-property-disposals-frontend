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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.IndividualSupplyingInformation
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, UKAddressLookupService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Onboarding.IndividualSupplyingInformationAddressJourney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RegistrationEnterAddressController @Inject() (
  val errorHandler: ErrorHandler,
  val ukAddressLookupService: UKAddressLookupService,
  val sessionStore: SessionStore,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val auditService: AuditService,
  cc: MessagesControllerComponents,
  val enterPostcodePage: views.html.address.enter_postcode,
  val selectAddressPage: views.html.address.select_address,
  val enterUkAddressPage: views.html.address.enter_uk_address,
  val enterNonUkAddressPage: views.html.address.enter_nonUk_address,
  val isUkPage: views.html.address.isUk
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with Logging
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with AddressController[IndividualSupplyingInformationAddressJourney] {

  val toJourneyStatus: IndividualSupplyingInformationAddressJourney => JourneyStatus = _.journey

  // trusts do not use this journey
  def isATrust(journey: IndividualSupplyingInformationAddressJourney): Boolean = false

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, IndividualSupplyingInformationAddressJourney)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, r @ IndividualSupplyingInformation(Some(_), None, _, _, _))) =>
        Right(sessionData -> IndividualSupplyingInformationAddressJourney(r))
      case _ =>
        Left(Redirect(controllers.routes.StartController.start()))
    }

  def updateAddress(
    journey: IndividualSupplyingInformationAddressJourney,
    address: Address,
    isManuallyEnteredAddress: Boolean
  )(
    implicit hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, JourneyStatus] =
    EitherT.pure[Future, Error](journey.journey.copy(address = Some(address)))

  protected lazy val backLinkCall: Call =
    controllers.onboarding.name.routes.RegistrationEnterIndividualNameController.enterIndividualName()
  protected lazy val isUkCall: Call                 = routes.RegistrationEnterAddressController.isUk()
  protected lazy val isUkSubmitCall: Call           = routes.RegistrationEnterAddressController.isUkSubmit()
  protected lazy val enterUkAddressCall: Call       = routes.RegistrationEnterAddressController.enterUkAddress()
  protected lazy val enterUkAddressSubmitCall: Call = routes.RegistrationEnterAddressController.enterUkAddressSubmit()
  protected lazy val enterNonUkAddressCall: Call    = routes.RegistrationEnterAddressController.enterNonUkAddress()
  protected lazy val enterNonUkAddressSubmitCall: Call =
    routes.RegistrationEnterAddressController.enterNonUkAddressSubmit()
  protected lazy val enterPostcodeCall: Call       = routes.RegistrationEnterAddressController.enterPostcode()
  protected lazy val enterPostcodeSubmitCall: Call = routes.RegistrationEnterAddressController.enterPostcodeSubmit()
  protected lazy val selectAddressCall: Call       = routes.RegistrationEnterAddressController.selectAddress()
  protected lazy val selectAddressSubmitCall: Call = routes.RegistrationEnterAddressController.selectAddressSubmit()
  protected lazy val continueCall: Call            = controllers.onboarding.routes.RegistrationController.checkYourAnswers()

}
