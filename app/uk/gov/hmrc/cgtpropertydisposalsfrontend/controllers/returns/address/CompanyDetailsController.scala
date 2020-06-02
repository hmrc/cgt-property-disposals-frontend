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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.Inject
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftSingleIndirectDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, UKAddressLookupService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.EnteringCompanyDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class CompanyDetailsController @Inject() (
  val errorHandler: ErrorHandler,
  val ukAddressLookupService: UKAddressLookupService,
  val sessionStore: SessionStore,
  val auditService: AuditService,
  returnsService: ReturnsService,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  cc: MessagesControllerComponents,
  val enterPostcodePage: views.html.address.enter_postcode,
  val selectAddressPage: views.html.address.select_address,
  val enterUkAddressPage: views.html.address.enter_uk_address,
  val enterNonUkAddressPage: views.html.address.enter_nonUk_address,
  val isUkPage: views.html.address.isUk,
  val exitPage: views.html.address.exit_page,
  checkYourAnswersPage: views.html.returns.address.single_indirect_disposal_check_your_answers
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with Logging
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with AddressController[EnteringCompanyDetails] {

  override val toJourneyStatus: EnteringCompanyDetails => JourneyStatus =
    _.journey

  def isATrust(journey: EnteringCompanyDetails): Boolean =
    journey.journey.subscribedDetails.isATrust

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, EnteringCompanyDetails)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (
              sessionData,
              f @ FillingOutReturn(
                _,
                _,
                _,
                i: DraftSingleIndirectDisposalReturn
              )
            )
          ) =>
        Right(
          sessionData -> EnteringCompanyDetails(
            f,
            i,
            i.triageAnswers.representativeType(),
            f.subscribedDetails.isATrust
          )
        )

      case _ => Left(Redirect(controllers.routes.StartController.start()))
    }

  def updateAddress(
    journey: EnteringCompanyDetails,
    address: Address,
    isManuallyEnteredAddress: Boolean
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, JourneyStatus] = {
    val newJourney = journey.journey.copy(
      draftReturn = journey.draftReturn.copy(
        companyAddress = Some(address)
      )
    )

    returnsService
      .storeDraftReturn(
        newJourney.draftReturn,
        newJourney.subscribedDetails.cgtReference,
        newJourney.agentReferenceNumber
      )
      .map(_ => newJourney)
  }

  private lazy val redirectToEnterUkAddress = Action(
    Redirect(routes.CompanyDetailsController.enterUkAddress())
  )

  override def enterPostcode(): Action[AnyContent] = redirectToEnterUkAddress

  override def enterPostcodeSubmit(): Action[AnyContent] =
    redirectToEnterUkAddress

  override def selectAddress(): Action[AnyContent] = redirectToEnterUkAddress

  override def selectAddressSubmit(): Action[AnyContent] =
    redirectToEnterUkAddress

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (_, journey) =>
          journey.draftReturn.companyAddress.fold(
            Redirect(routes.CompanyDetailsController.isUk())
          )(companyAddress => Ok(checkYourAnswersPage(companyAddress)))

      }
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (_, _) =>
          Redirect(controllers.returns.routes.TaskListController.taskList())
      }
    }

  protected def backLinkCall: EnteringCompanyDetails => Call =
    _.draftReturn.companyAddress.fold(
      controllers.returns.routes.TaskListController.taskList()
    )(_ => routes.CompanyDetailsController.checkYourAnswers())

  protected lazy val isUkCall: Call                                                 =
    routes.CompanyDetailsController.isUk()
  protected lazy val isUkSubmitCall: Call                                           =
    routes.CompanyDetailsController.isUkSubmit()
  protected lazy val enterUkAddressCall: Call                                       =
    routes.CompanyDetailsController.enterUkAddress()
  protected lazy val enterUkAddressSubmitCall: Call                                 =
    routes.CompanyDetailsController.enterUkAddressSubmit()
  protected lazy val enterNonUkAddressCall: Call                                    =
    routes.CompanyDetailsController.enterNonUkAddress()
  protected lazy val enterNonUkAddressSubmitCall: Call                              =
    routes.CompanyDetailsController.enterNonUkAddressSubmit()
  protected lazy val enterPostcodeCall: Call                                        =
    routes.CompanyDetailsController.enterPostcode()
  protected lazy val enterPostcodeSubmitCall: Call                                  =
    routes.CompanyDetailsController.enterPostcodeSubmit()
  protected lazy val selectAddressCall: Call                                        =
    routes.CompanyDetailsController.selectAddress()
  protected lazy val selectAddressSubmitCall: Call                                  =
    routes.CompanyDetailsController.selectAddressSubmit()
  protected lazy val continueCall: Call                                             =
    routes.CompanyDetailsController.checkYourAnswers()
  protected lazy val exitPageCall: Call                                             =
    routes.CompanyDetailsController.showExitPage()
  override protected val enterUkAddressBackLinkCall: EnteringCompanyDetails => Call =
    _ => isUkCall
}
