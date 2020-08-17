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
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents, Request, Result}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExampleCompanyDetailsAnswers.{CompleteExampleCompanyDetailsAnswers, IncompleteExampleCompanyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleIndirectDisposalsReturn, DraftSingleIndirectDisposalReturn, ExampleCompanyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, UKAddressLookupService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.EnteringCompanyDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.CompanyDetailsController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}

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
  val multipleIndirectDisposalsGuidancePage: views.html.returns.address.multiple_indirect_disposals_guidance,
  val multipleIndirectDisposalPricePage: views.html.returns.address.multiple_indirect_disposals_disposal_price,
  val multipleIndirectAcquisitionPricePage: views.html.returns.address.multiple_indirect_disposals_acquisition_price,
  val multipleIndirectCheckYourAnswersPage: views.html.returns.address.multiple_indirect_disposals_check_your_answers,
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
                i: DraftSingleIndirectDisposalReturn,
                _
              )
            )
          ) =>
        Right(
          sessionData -> EnteringCompanyDetails(
            f,
            Right(i),
            i.triageAnswers.representativeType(),
            f.subscribedDetails.isATrust
          )
        )

      case Some(
            (
              sessionData,
              f @ FillingOutReturn(
                _,
                _,
                _,
                i: DraftMultipleIndirectDisposalsReturn,
                _
              )
            )
          ) =>
        Right(
          sessionData -> EnteringCompanyDetails(
            f,
            Left(i),
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
      draftReturn = journey.draftReturn.fold(
        multipleIndirect =>
          multipleIndirect.copy(
            exampleCompanyDetailsAnswers = Some(
              multipleIndirect.exampleCompanyDetailsAnswers
                .getOrElse(IncompleteExampleCompanyDetailsAnswers.empty)
                .fold(
                  _.copy(address = Some(address)),
                  _.copy(address = address)
                )
            )
          ),
        _.copy(companyAddress = Some(address))
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

  def multipleIndirectDisposalsGuidance(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleIndirectDisposalReturn)   =>
            Redirect(routes.PropertyDetailsController.checkYourAnswers())

          case Left(m: DraftMultipleIndirectDisposalsReturn) =>
            val backLink =
              m.exampleCompanyDetailsAnswers
                .fold(controllers.returns.routes.TaskListController.taskList())(_ =>
                  routes.CompanyDetailsController.checkYourAnswers()
                )

            Ok(
              multipleIndirectDisposalsGuidancePage(
                backLink,
                m.triageAnswers.isPeriodOfAdmin
              )
            )
        }
      }
    }

  def multipleIndirectDisposalsGuidanceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleIndirectDisposalReturn)   =>
            Redirect(routes.CompanyDetailsController.checkYourAnswers())
          case Left(m: DraftMultipleIndirectDisposalsReturn) =>
            val redirectTo =
              m.exampleCompanyDetailsAnswers
                .getOrElse(IncompleteExampleCompanyDetailsAnswers.empty)
                .fold(
                  _ => routes.CompanyDetailsController.isUk(),
                  _ => routes.CompanyDetailsController.checkYourAnswers()
                )

            Redirect(redirectTo)
        }
      }
    }

  private def disposalPriceBackLink(
    answers: ExampleCompanyDetailsAnswers
  ): Call =
    answers.fold(
      i =>
        i.address match {
          case Some(UkAddress(_, _, _, _, _))       => routes.CompanyDetailsController.enterUkAddress()
          case Some(NonUkAddress(_, _, _, _, _, _)) => routes.CompanyDetailsController.enterNonUkAddress()
          case None                                 => routes.CompanyDetailsController.isUk()
        },
      _ => routes.CompanyDetailsController.checkYourAnswers()
    )

  def multipleIndirectDisposalPrice(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleIndirectDisposalReturn)   =>
            Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case Left(m: DraftMultipleIndirectDisposalsReturn) =>
            val answers = m.exampleCompanyDetailsAnswers
              .getOrElse(IncompleteExampleCompanyDetailsAnswers.empty)

            val backLink = disposalPriceBackLink(answers)

            val disposalPrice = answers
              .fold(_.disposalPrice, c => Some(c.disposalPrice))

            val form = disposalPrice.fold(multipleIndirectDisposalPriceForm)(c =>
              multipleIndirectDisposalPriceForm.fill(c.inPounds)
            )

            Ok(multipleIndirectDisposalPricePage(form, backLink))
        }
      }
    }

  def multipleIndirectDisposalPriceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleIndirectDisposalReturn)   =>
            Redirect(routes.PropertyDetailsController.checkYourAnswers())

          case Left(m: DraftMultipleIndirectDisposalsReturn) =>
            val answers  = m.exampleCompanyDetailsAnswers
              .getOrElse(IncompleteExampleCompanyDetailsAnswers.empty)
            val backLink = disposalPriceBackLink(answers)

            multipleIndirectDisposalPriceForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    multipleIndirectDisposalPricePage(formWithErrors, backLink)
                  ),
                disposalPrice =>
                  if (
                    answers
                      .fold(_.disposalPrice, c => Some(c.disposalPrice))
                      .contains(AmountInPence.fromPounds(disposalPrice))
                  )
                    Redirect(
                      routes.CompanyDetailsController.checkYourAnswers()
                    )
                  else {
                    val updatedAnswers     =
                      answers
                        .fold(
                          _.copy(disposalPrice = Some(AmountInPence.fromPounds(disposalPrice))),
                          _.copy(disposalPrice = AmountInPence.fromPounds(disposalPrice))
                        )
                    val updatedDraftReturn = m.copy(
                      exampleCompanyDetailsAnswers = Some(updatedAnswers),
                      yearToDateLiabilityAnswers = None,
                      gainOrLossAfterReliefs = None
                    )
                    val result             = for {
                      _ <- returnsService.storeDraftReturn(
                             updatedDraftReturn,
                             r.journey.subscribedDetails.cgtReference,
                             r.journey.agentReferenceNumber
                           )
                      _ <- EitherT(
                             updateSession(sessionStore, request)(
                               _.copy(journeyStatus =
                                 Some(
                                   r.journey.copy(draftReturn = updatedDraftReturn)
                                 )
                               )
                             )
                           )
                    } yield ()

                    result.fold(
                      { e =>
                        logger.warn("Could not update draft return", e)
                        errorHandler.errorResult()
                      },
                      _ =>
                        Redirect(
                          routes.CompanyDetailsController.checkYourAnswers()
                        )
                    )
                  }
              )
        }
      }
    }

  private def acquisitionPriceBackLink(
    answers: ExampleCompanyDetailsAnswers
  ): Call =
    answers.fold(
      _ => routes.CompanyDetailsController.multipleIndirectDisposalPrice(),
      _ => routes.CompanyDetailsController.checkYourAnswers()
    )

  def multipleIndirectAcquisitionPrice(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleIndirectDisposalReturn)   =>
            Redirect(routes.PropertyDetailsController.checkYourAnswers())

          case Left(m: DraftMultipleIndirectDisposalsReturn) =>
            val answers = m.exampleCompanyDetailsAnswers
              .getOrElse(IncompleteExampleCompanyDetailsAnswers.empty)

            val backLink = acquisitionPriceBackLink(answers)

            val acquisitionPrice = answers
              .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))

            val form = acquisitionPrice.fold(multipleIndirectAcquisitionPriceForm)(c =>
              multipleIndirectAcquisitionPriceForm.fill(c.inPounds)
            )

            Ok(multipleIndirectAcquisitionPricePage(form, backLink))
        }
      }
    }

  def multipleIndirectAcquisitionPriceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleIndirectDisposalReturn)   =>
            Redirect(routes.PropertyDetailsController.checkYourAnswers())

          case Left(m: DraftMultipleIndirectDisposalsReturn) =>
            val answers  = m.exampleCompanyDetailsAnswers
              .getOrElse(IncompleteExampleCompanyDetailsAnswers.empty)
            val backLink = acquisitionPriceBackLink(answers)

            multipleIndirectAcquisitionPriceForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    multipleIndirectAcquisitionPricePage(
                      formWithErrors,
                      backLink
                    )
                  ),
                acquisitionPrice =>
                  if (
                    answers
                      .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))
                      .contains(AmountInPence.fromPounds(acquisitionPrice))
                  )
                    Redirect(
                      routes.CompanyDetailsController.checkYourAnswers()
                    )
                  else {
                    val updatedAnswers     =
                      answers
                        .fold(
                          _.copy(acquisitionPrice = Some(AmountInPence.fromPounds(acquisitionPrice))),
                          _.copy(acquisitionPrice = AmountInPence.fromPounds(acquisitionPrice))
                        )
                    val updatedDraftReturn = m.copy(
                      exampleCompanyDetailsAnswers = Some(updatedAnswers),
                      yearToDateLiabilityAnswers = None,
                      gainOrLossAfterReliefs = None
                    )
                    val result             = for {
                      _ <- returnsService.storeDraftReturn(
                             updatedDraftReturn,
                             r.journey.subscribedDetails.cgtReference,
                             r.journey.agentReferenceNumber
                           )
                      _ <- EitherT(
                             updateSession(sessionStore, request)(
                               _.copy(journeyStatus =
                                 Some(
                                   r.journey.copy(draftReturn = updatedDraftReturn)
                                 )
                               )
                             )
                           )
                    } yield ()

                    result.fold(
                      { e =>
                        logger.warn("Could not update draft return", e)
                        errorHandler.errorResult()
                      },
                      _ =>
                        Redirect(
                          routes.CompanyDetailsController.checkYourAnswers()
                        )
                    )
                  }
              )
        }
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (_, journey) =>
          journey.draftReturn match {

            case Right(singleIndirect)  =>
              singleIndirect.companyAddress.fold(
                Redirect(routes.CompanyDetailsController.isUk())
              )(companyAddress => Ok(checkYourAnswersPage(companyAddress)))

            case Left(multipleIndirect) =>
              multipleIndirect.exampleCompanyDetailsAnswers.fold[Future[Result]](
                Redirect(routes.CompanyDetailsController.multipleIndirectDisposalsGuidance())
              ) {

                case IncompleteExampleCompanyDetailsAnswers(None, _, _)                  =>
                  Redirect(
                    routes.CompanyDetailsController.multipleIndirectDisposalsGuidance()
                  )

                case IncompleteExampleCompanyDetailsAnswers(_, None, _)                  =>
                  Redirect(
                    routes.CompanyDetailsController.multipleIndirectDisposalPrice()
                  )

                case IncompleteExampleCompanyDetailsAnswers(_, _, None)                  =>
                  Redirect(
                    routes.CompanyDetailsController.multipleIndirectAcquisitionPrice()
                  )

                case IncompleteExampleCompanyDetailsAnswers(Some(a), Some(dp), Some(ap)) =>
                  val completeAnswers    = CompleteExampleCompanyDetailsAnswers(a, dp, ap)
                  val updatedDraftReturn = multipleIndirect.copy(
                    exampleCompanyDetailsAnswers = Some(completeAnswers)
                  )

                  val result = for {
                    _ <- returnsService.storeDraftReturn(
                           updatedDraftReturn,
                           journey.journey.subscribedDetails.cgtReference,
                           journey.journey.agentReferenceNumber
                         )
                    _ <- EitherT(
                           updateSession(sessionStore, request)(
                             _.copy(journeyStatus =
                               Some(
                                 journey.journey.copy(
                                   draftReturn = updatedDraftReturn
                                 )
                               )
                             )
                           )
                         )
                  } yield ()

                  result.fold(
                    { e =>
                      logger.warn("Could not update draft return", e)
                      errorHandler.errorResult()
                    },
                    _ =>
                      Ok(
                        multipleIndirectCheckYourAnswersPage(completeAnswers)
                      )
                  )

                case c: CompleteExampleCompanyDetailsAnswers                             =>
                  Ok(
                    multipleIndirectCheckYourAnswersPage(c)
                  )

              }
          }
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
    _.draftReturn.fold(
      _.exampleCompanyDetailsAnswers.fold(
        controllers.returns.routes.TaskListController.taskList()
      )(_ => routes.CompanyDetailsController.checkYourAnswers()),
      _.companyAddress.fold(
        controllers.returns.routes.TaskListController.taskList()
      )(_ => routes.CompanyDetailsController.checkYourAnswers())
    )

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
  protected lazy val ukAddressNotAllowedExitPageCall: Option[Call]                  =
    None
  override protected val enterUkAddressBackLinkCall: EnteringCompanyDetails => Call =
    _ => isUkCall
}

object CompanyDetailsController {

  val multipleIndirectDisposalPriceForm: Form[BigDecimal] =
    Form(
      mapping(
        "multipleIndirectDisposalsDisposalPrice" -> of(
          MoneyUtils
            .amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

  val multipleIndirectAcquisitionPriceForm: Form[BigDecimal] =
    Form(
      mapping(
        "multipleIndirectDisposalsAcquisitionPrice" -> of(
          MoneyUtils
            .amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

}
