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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails

import java.time.LocalDate

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.http.Writeable
import play.api.i18n.Messages
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, RebasingCutoffDates, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.AcquisitionDetailsController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.PersonalRepresentativeInPeriodOfAdmin
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{acquisitiondetails => pages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AcquisitionDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  val rebasingEligibilityUtil: RebasingEligibilityUtil,
  acquisitionMethodPage: pages.acquisition_method,
  acquisitionDatePage: pages.acquisition_date,
  acquisitionPricePage: pages.acquisition_price,
  improvementCostsPage: pages.improvement_costs,
  acquisitionFeesPage: pages.acquisition_fees,
  rebasedAcquisitionPricePage: pages.rebased_acquisition_price,
  checkYouAnswersPage: pages.check_your_answers,
  shouldUseRebasePage: pages.should_use_rebase,
  periodOfAdminMarketValuePage: pages.period_of_admin_market_value
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  type JourneyState =
    Either[DraftSingleIndirectDisposalReturn, DraftSingleDisposalReturn]

  private def representativeType(state: JourneyState): Option[RepresentativeType] =
    state.fold(_.triageAnswers.representativeType(), _.triageAnswers.representativeType())

  private def withFillingOutReturnAndAcquisitionDetailsAnswers(
    request: RequestWithSessionData[_]
  )(
    f: (
      SessionData,
      FillingOutReturn,
      JourneyState,
      AcquisitionDetailsAnswers
    ) => Future[Result]
  ): Future[Result] = {
    def defaultAnswers(
      triageAnswers: SingleDisposalTriageAnswers,
      representeeAnswers: Option[RepresenteeAnswers],
      isIndirectDisposal: Boolean
    ): Option[IncompleteAcquisitionDetailsAnswers] = {
      val answers =
        triageAnswers.fold(_.individualUserType, _.individualUserType) match {
          case Some(PersonalRepresentativeInPeriodOfAdmin) =>
            representeeAnswers.collect {
              case CompleteRepresenteeAnswers(_, _, Some(dateOfDeath), _, _) =>
                IncompleteAcquisitionDetailsAnswers.empty.copy(
                  acquisitionDate = Some(AcquisitionDate(dateOfDeath.value)),
                  acquisitionMethod = Some(AcquisitionMethod.Other("period of admin"))
                )
            }

          case _                                           =>
            Some(IncompleteAcquisitionDetailsAnswers.empty)
        }

      if (isIndirectDisposal) answers.map(_.copy(improvementCosts = Some(AmountInPence.zero)))
      else answers
    }

    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some(
            (s, r @ FillingOutReturn(_, _, _, d: DraftSingleDisposalReturn, _))
          ) =>
        d.acquisitionDetailsAnswers.fold[Future[Result]](
          defaultAnswers(d.triageAnswers, d.representeeAnswers, isIndirectDisposal = false).fold[Future[Result]](
            Redirect(controllers.routes.StartController.start())
          )(answers => f(s, r, Right(d), answers))
        )(f(s, r, Right(d), _))

      case Some(
            (
              s,
              r @ FillingOutReturn(
                _,
                _,
                _,
                d: DraftSingleIndirectDisposalReturn,
                _
              )
            )
          ) =>
        d.acquisitionDetailsAnswers.fold[Future[Result]](
          defaultAnswers(d.triageAnswers, d.representeeAnswers, isIndirectDisposal = true).fold[Future[Result]](
            Redirect(controllers.routes.StartController.start())
          )(answers => f(s, r, Left(d), answers))
        )(f(s, r, Left(d), _))

      case _ => Redirect(controllers.routes.StartController.start())
    }
  }

  private def withAssetTypeAndResidentialStatus(
    state: JourneyState
  )(f: (AssetType, Boolean) => Future[Result]): Future[Result] =
    state
      .fold(_.triageAnswers, _.triageAnswers)
      .fold(
        i => i.assetType -> i.wasAUKResident,
        c => Some(c.assetType) -> Some(c.countryOfResidence.isUk())
      ) match {
      case (Some(a), Some(w)) => f(a, w)
      case _                  =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }

  private def withAssetType(
    state: JourneyState
  )(f: AssetType => Future[Result]): Future[Result] =
    state
      .fold(_.triageAnswers, _.triageAnswers)
      .fold(
        i => i.assetType,
        c => Some(c.assetType)
      ) match {
      case Some(a) => f(a)
      case _       =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }

  private def withDisposalDate(
    state: JourneyState
  )(f: DisposalDate => Future[Result]): Future[Result] =
    state
      .fold(_.triageAnswers, _.triageAnswers)
      .fold(_.disposalDate, c => Some(c.disposalDate))
      .fold[Future[Result]](
        Redirect(controllers.returns.routes.TaskListController.taskList())
      )(f)

  private def withAcquisitionDate(
    answers: AcquisitionDetailsAnswers
  )(f: AcquisitionDate => Future[Result]): Future[Result] =
    answers
      .fold(_.acquisitionDate, c => Some(c.acquisitionDate))
      .fold[Future[Result]](
        Redirect(routes.AcquisitionDetailsController.checkYourAnswers())
      )(f)

  private def withPeriodOfAdmin(
    state: JourneyState
  )(f: DateOfDeath => Future[Result]): Future[Result] = {
    val triageAnswers      = state.fold(_.triageAnswers, _.triageAnswers)
    val representeeAnswers = state.fold(_.representeeAnswers, _.representeeAnswers)
    (triageAnswers.representativeType(), representeeAnswers.flatMap(_.fold(_.dateOfDeath, _.dateOfDeath))) match {
      case (Some(PersonalRepresentativeInPeriodOfAdmin), Some(dateOfDeath)) => f(dateOfDeath)
      case _                                                                => Redirect(routes.AcquisitionDetailsController.checkYourAnswers())
    }
  }

  private def withAcquisitionMethod(
    answers: AcquisitionDetailsAnswers
  )(f: AcquisitionMethod => Future[Result]): Future[Result] =
    answers
      .fold(_.acquisitionMethod, c => Some(c.acquisitionMethod))
      .fold[Future[Result]](
        Redirect(routes.AcquisitionDetailsController.checkYourAnswers())
      )(f)

  private def commonDisplayBehaviour[A, P : Writeable, R](
    currentAnswers: AcquisitionDetailsAnswers
  )(form: AcquisitionDetailsAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: AcquisitionDetailsAnswers => Boolean,
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers)) {
      val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => routes.AcquisitionDetailsController.checkYourAnswers()
      )

      Ok(page(form(currentAnswers), backLink))
    } else
      Redirect(redirectToIfNoRequiredPreviousAnswer)

  private def commonSubmitBehaviour[A, P : Writeable, R](
    currentFillingOutReturn: FillingOutReturn,
    currentState: JourneyState,
    currentAnswers: AcquisitionDetailsAnswers
  )(form: Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: AcquisitionDetailsAnswers => Boolean,
    redirectToIfNoRequiredPreviousAnswer: Call
  )(
    updateState: (A, AcquisitionDetailsAnswers, JourneyState) => JourneyState
  )(implicit
    request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers)) {
      lazy val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => routes.AcquisitionDetailsController.checkYourAnswers()
      )
      form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink)),
          { value =>
            val newDraftReturn =
              updateState(value, currentAnswers, currentState)

            val result = for {
              _ <- if (newDraftReturn.merge === currentState.merge)
                     EitherT.pure(())
                   else
                     returnsService.storeDraftReturn(
                       newDraftReturn.merge,
                       currentFillingOutReturn.subscribedDetails.cgtReference,
                       currentFillingOutReturn.agentReferenceNumber
                     )
              _ <- EitherT(
                     updateSession(sessionStore, request)(
                       _.copy(journeyStatus =
                         Some(
                           currentFillingOutReturn
                             .copy(draftReturn = newDraftReturn.merge)
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
              _ => Redirect(routes.AcquisitionDetailsController.checkYourAnswers())
            )
          }
        )
    } else
      Redirect(redirectToIfNoRequiredPreviousAnswer)

  private def commonUpdateDraftReturn(
    d: JourneyState,
    newAnswers: AcquisitionDetailsAnswers
  ): Either[DraftSingleIndirectDisposalReturn, DraftSingleDisposalReturn] =
    d.bimap(
      i =>
        i.copy(
          acquisitionDetailsAnswers = Some(newAnswers),
          yearToDateLiabilityAnswers = i.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
        ),
      s => {

        val reliefDetailsAnswers = s.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief(s.triageAnswers.isPeriodOfAdmin))

        s.copy(
          acquisitionDetailsAnswers = Some(newAnswers),
          initialGainOrLoss = None,
          reliefDetailsAnswers = reliefDetailsAnswers,
          yearToDateLiabilityAnswers = s.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
        )
      }
    )

  def acquisitionMethod(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
        case (_, fillingOutReturn, state, answers) =>
          withAssetType(state) { assetType =>
            commonDisplayBehaviour(answers)(
              form = _.fold(
                _.acquisitionMethod
                  .fold(acquisitionMethodForm)(acquisitionMethodForm.fill),
                c => acquisitionMethodForm.fill(c.acquisitionMethod)
              )
            )(
              page = acquisitionMethodPage(
                _,
                _,
                fillingOutReturn.subscribedDetails.isATrust,
                representativeType(state),
                assetType
              )
            )(
              requiredPreviousAnswer = _ => true,
              controllers.returns.routes.TaskListController.taskList()
            )
          }
      }
    }

  def acquisitionMethodSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
        case (_, fillingOutReturn, state, answers) =>
          withAssetType(state) { assetType =>
            commonSubmitBehaviour(
              fillingOutReturn,
              state,
              answers
            )(
              acquisitionMethodForm
            )(
              acquisitionMethodPage(
                _,
                _,
                fillingOutReturn.subscribedDetails.isATrust,
                representativeType(state),
                assetType
              )
            )(
              requiredPreviousAnswer = _ => noAnswersRequired,
              controllers.returns.routes.TaskListController.taskList()
            )(
              updateState = {
                case (m, answers, draftReturn) =>
                  if (
                    answers
                      .fold(_.acquisitionMethod, c => Some(c.acquisitionMethod))
                      .contains(m)
                  )
                    draftReturn
                  else {
                    val newAnswers = answers
                      .unset(_.acquisitionPrice)
                      .unset(_.rebasedAcquisitionPrice)
                      .unset(_.shouldUseRebase)
                      .unset(_.acquisitionFees)
                      .copy(acquisitionMethod = Some(m))
                    commonUpdateDraftReturn(draftReturn, newAnswers)
                  }
              }
            )
          }
      }
    }

  def acquisitionDate(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
        case (_, fillingOutReturn, state, answers) =>
          withDisposalDate(state) {
            case (disposalDate) =>
              withAssetType(state) { assetType =>
                val form = acquisitionDateForm(disposalDate.value)

                commonDisplayBehaviour(answers)(
                  form = _.fold(
                    _.acquisitionDate.fold(form)(form.fill),
                    c => form.fill(c.acquisitionDate)
                  )
                )(
                  page = acquisitionDatePage(
                    _,
                    _,
                    fillingOutReturn.subscribedDetails.isATrust,
                    representativeType(state),
                    assetType
                  )
                )(
                  requiredPreviousAnswer = _.fold(
                    _.acquisitionMethod,
                    c => Some(c.acquisitionMethod)
                  ).isDefined,
                  redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.acquisitionMethod()
                )
              }
          }
      }
    }

  def acquisitionDateSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
        case (_, fillingOutReturn, state, answers) =>
          withDisposalDate(state) {
            case (disposalDate) =>
              withAssetType(state) { assetType =>
                commonSubmitBehaviour(
                  fillingOutReturn,
                  state,
                  answers
                )(
                  form = acquisitionDateForm(disposalDate.value)
                )(
                  acquisitionDatePage(
                    _,
                    _,
                    fillingOutReturn.subscribedDetails.isATrust,
                    representativeType(state),
                    assetType
                  )
                )(
                  requiredPreviousAnswer = _.fold(
                    _.acquisitionMethod,
                    c => Some(c.acquisitionMethod)
                  ).isDefined,
                  redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.acquisitionMethod()
                )(
                  updateState = {
                    case (d, answers, draftReturn) =>
                      val existingAcquisitionDate =
                        answers
                          .fold(_.acquisitionDate, c => Some(c.acquisitionDate))

                      if (existingAcquisitionDate.contains(d))
                        draftReturn
                      else {
                        val newAnswers = answers
                          .unset(_.acquisitionPrice)
                          .unset(_.rebasedAcquisitionPrice)
                          .unset(_.shouldUseRebase)
                          .unset(_.acquisitionFees)
                          .copy(acquisitionDate = Some(d))

                        commonUpdateDraftReturn(
                          draftReturn,
                          if (assetType === AssetType.IndirectDisposal) newAnswers
                          else newAnswers.unset(_.improvementCosts)
                        )
                      }
                  }
                )
              }
          }
      }
    }

  def acquisitionPrice(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, fillingOutReturn, state, answers) =>
        withAcquisitionDate(answers) { acquisitionDate =>
          withAcquisitionMethod(answers) { acquisitionMethod =>
            withAssetType(state) { assetType =>
              commonDisplayBehaviour(answers)(
                form = _.fold(
                  _.acquisitionPrice.fold(acquisitionPriceForm)(p => acquisitionPriceForm.fill(p.inPounds)),
                  c => acquisitionPriceForm.fill(c.acquisitionPrice.inPounds())
                )
              )(
                page = acquisitionPricePage(
                  _,
                  _,
                  acquisitionMethod,
                  acquisitionDate,
                  fillingOutReturn.subscribedDetails.isATrust,
                  representativeType(state),
                  assetType
                )
              )(
                requiredPreviousAnswer = _.fold(
                  _.acquisitionDate,
                  c => Some(c.acquisitionDate)
                ).isDefined,
                redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.acquisitionDate()
              )
            }
          }
        }
      }
    }

  def acquisitionPriceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, fillingOutReturn, state, answers) =>
        withAcquisitionDate(answers) { acquisitionDate =>
          withAcquisitionMethod(answers) { acquisitionMethod =>
            withAssetType(state) { assetType =>
              commonSubmitBehaviour(
                fillingOutReturn,
                state,
                answers
              )(
                acquisitionPriceForm
              )(page =
                acquisitionPricePage(
                  _,
                  _,
                  acquisitionMethod,
                  acquisitionDate,
                  fillingOutReturn.subscribedDetails.isATrust,
                  representativeType(state),
                  assetType
                )
              )(
                requiredPreviousAnswer = _.fold(
                  _.acquisitionDate,
                  c => Some(c.acquisitionDate)
                ).isDefined,
                redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.acquisitionDate()
              )(
                updateState = { (p, answers, draftReturn) =>
                  if (
                    answers
                      .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))
                      .map(_.inPounds())
                      .contains(p)
                  )
                    draftReturn
                  else {
                    val newAnswers = answers.fold(
                      _.copy(acquisitionPrice = Some(AmountInPence.fromPounds(p))),
                      _.copy(acquisitionPrice = AmountInPence.fromPounds(p))
                    )
                    commonUpdateDraftReturn(draftReturn, newAnswers)
                  }
                }
              )
            }
          }
        }
      }
    }

  def periodOfAdminMarketValue(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, _, state, answers) =>
        withPeriodOfAdmin(state) { dateOfDeath =>
          withAssetType(state) { assetType =>
            commonDisplayBehaviour(answers)(
              form = _.fold(
                _.acquisitionPrice.fold(periodOfAdminMarketValueForm(dateOfDeath))(p =>
                  periodOfAdminMarketValueForm(dateOfDeath).fill(p.inPounds())
                ),
                c => periodOfAdminMarketValueForm(dateOfDeath).fill(c.acquisitionPrice.inPounds())
              )
            )(
              page = periodOfAdminMarketValuePage(
                _,
                _,
                dateOfDeath,
                assetType
              )
            )(
              requiredPreviousAnswer = _ => true,
              redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
            )
          }
        }
      }
    }

  def periodOfAdminMarketValueSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, fillingOutReturn, state, answers) =>
        withPeriodOfAdmin(state) { dateOfDeath =>
          withAssetType(state) { assetType =>
            commonSubmitBehaviour(
              fillingOutReturn,
              state,
              answers
            )(
              periodOfAdminMarketValueForm(dateOfDeath)
            )(page =
              periodOfAdminMarketValuePage(
                _,
                _,
                dateOfDeath,
                assetType
              )
            )(
              requiredPreviousAnswer = _ => true,
              redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
            )(
              updateState = { (p, answers, draftReturn) =>
                if (
                  answers
                    .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))
                    .map(_.inPounds())
                    .contains(p)
                )
                  draftReturn
                else {
                  val newAnswers = answers.fold(
                    _.copy(acquisitionPrice = Some(AmountInPence.fromPounds(p))),
                    _.copy(acquisitionPrice = AmountInPence.fromPounds(p))
                  )
                  commonUpdateDraftReturn(draftReturn, newAnswers)
                }
              }
            )
          }
        }
      }
    }

  def rebasedAcquisitionPrice(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, fillingOutReturn, state, answers) =>
        withAssetTypeAndResidentialStatus(state) { (assetType, wasUkResident) =>
          withAcquisitionDate(answers) { acquisitionDate =>
            if (
              !rebasingEligibilityUtil.isEligibleForRebase(
                wasUkResident,
                assetType,
                acquisitionDate,
                representativeType(state)
              )
            )
              Redirect(routes.AcquisitionDetailsController.checkYourAnswers())
            else
              commonDisplayBehaviour(answers)(
                form = _.fold(
                  _.rebasedAcquisitionPrice
                    .fold(rebasedAcquisitionPriceForm)(a => rebasedAcquisitionPriceForm.fill(a.inPounds())),
                  _.rebasedAcquisitionPrice
                    .fold(rebasedAcquisitionPriceForm)(a => rebasedAcquisitionPriceForm.fill(a.inPounds()))
                )
              )(
                page = rebasedAcquisitionPricePage(
                  _,
                  _,
                  rebasingEligibilityUtil
                    .getRebasingCutOffDate(
                      assetType,
                      wasUkResident
                    ),
                  fillingOutReturn.subscribedDetails.isATrust,
                  representativeType(state),
                  assetType
                )
              )(
                requiredPreviousAnswer = answers =>
                  shouldRedirectFromRebaseAcquisitionQuestions(
                    wasUkResident,
                    answers,
                    acquisitionDate
                  ),
                redirectToIfNoRequiredPreviousAnswer =
                  if (wasUkResident)
                    routes.AcquisitionDetailsController.acquisitionDate()
                  else
                    routes.AcquisitionDetailsController.acquisitionPrice()
              )
          }
        }
      }
    }

  def rebasedAcquisitionPriceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, fillingOutReturn, state, answers) =>
        withAssetTypeAndResidentialStatus(state) { (assetType, wasUkResident) =>
          withAcquisitionDate(answers) { acquisitionDate =>
            rebasingEligibilityUtil
              .rebasingCutOffDateIfEligibleForRebase(
                acquisitionDate,
                assetType,
                wasUkResident,
                representativeType(state)
              )
              .fold[Future[Result]](
                Redirect(routes.AcquisitionDetailsController.checkYourAnswers())
              ) { rebaseDate =>
                commonSubmitBehaviour(fillingOutReturn, state, answers)(
                  form = rebasedAcquisitionPriceForm
                )(
                  page = { (form, backLink) =>
                    val p = form.copy(errors =
                      form.errors
                        .map(
                          _.copy(args = Seq(TimeUtils.govDisplayFormat(rebaseDate)))
                        )
                    )
                    rebasedAcquisitionPricePage(
                      p,
                      backLink,
                      rebaseDate,
                      fillingOutReturn.subscribedDetails.isATrust,
                      representativeType(state),
                      assetType
                    )
                  }
                )(
                  requiredPreviousAnswer = answers => {
                    if (wasUkResident) noAnswersRequired
                    else
                      answers
                        .fold(
                          _.acquisitionPrice,
                          c => Some(c.acquisitionPrice)
                        )
                        .isDefined
                  },
                  redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.acquisitionPrice()
                )(
                  updateState = { (p, answers, draftReturn) =>
                    if (
                      answers
                        .fold(
                          _.rebasedAcquisitionPrice,
                          _.rebasedAcquisitionPrice
                        )
                        .map(_.inPounds())
                        .contains(p)
                    )
                      draftReturn
                    else {
                      val newAnswers =
                        if (wasUkResident)
                          answers.fold(
                            _.copy(
                              rebasedAcquisitionPrice = Some(AmountInPence.fromPounds(p)),
                              shouldUseRebase = Some(true),
                              acquisitionPrice = Some(AmountInPence.fromPounds(p))
                            ),
                            _.copy(
                              rebasedAcquisitionPrice = Some(AmountInPence.fromPounds(p)),
                              shouldUseRebase = true,
                              acquisitionPrice = AmountInPence.fromPounds(p)
                            )
                          )
                        else
                          answers.fold(
                            _.copy(rebasedAcquisitionPrice = Some(AmountInPence.fromPounds(p))),
                            _.copy(rebasedAcquisitionPrice = Some(AmountInPence.fromPounds(p)))
                          )

                      commonUpdateDraftReturn(draftReturn, newAnswers)
                    }
                  }
                )
              }
          }
        }
      }
    }

  def improvementCosts(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, fillingOutReturn, state, answers) =>
        withAssetTypeAndResidentialStatus(state) { (assetType, wasUkResident) =>
          withAcquisitionDate(answers) { acquisitionDate =>
            val rebaseDate = rebasingEligibilityUtil
              .rebasingCutOffDateIfEligibleForRebase(
                acquisitionDate,
                assetType,
                wasUkResident,
                representativeType(state)
              )
            commonDisplayBehaviour(answers)(
              form = _.fold(
                _.improvementCosts.fold(improvementCostsForm)(p => improvementCostsForm.fill(p.inPounds())),
                c => improvementCostsForm.fill(c.improvementCosts.inPounds())
              )
            )(
              page = improvementCostsPage(
                _,
                _,
                fillingOutReturn.subscribedDetails.isATrust,
                answers
                  .fold(_.shouldUseRebase, r => Some(r.shouldUseRebase)),
                rebasingEligibilityUtil
                  .getRebasingCutOffDate(assetType, wasUkResident),
                representativeType(state)
              )
            )(
              requiredPreviousAnswer = { a =>
                if (rebaseDate.isDefined)
                  a.fold(
                      _.rebasedAcquisitionPrice,
                      _.rebasedAcquisitionPrice
                    )
                    .isDefined
                else
                  a.fold(_.acquisitionPrice, c => Some(c.acquisitionPrice)).isDefined
              },
              redirectToIfNoRequiredPreviousAnswer =
                if (rebaseDate.isDefined)
                  routes.AcquisitionDetailsController
                    .rebasedAcquisitionPrice()
                else if (isPeriodOfAdmin(state))
                  routes.AcquisitionDetailsController.periodOfAdminMarketValue()
                else
                  routes.AcquisitionDetailsController.acquisitionPrice()
            )
          }
        }
      }
    }

  def improvementCostsSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, fillingOutReturn, state, answers) =>
        withAssetTypeAndResidentialStatus(state) { (assetType, wasUkResident) =>
          withAcquisitionDate(answers) { acquisitionDate =>
            val rebaseDate = rebasingEligibilityUtil
              .rebasingCutOffDateIfEligibleForRebase(
                acquisitionDate,
                assetType,
                wasUkResident,
                representativeType(state)
              )
            commonSubmitBehaviour(
              fillingOutReturn,
              state,
              answers
            )(
              improvementCostsForm
            )(page =
              improvementCostsPage(
                _,
                _,
                fillingOutReturn.subscribedDetails.isATrust,
                answers
                  .fold(_.shouldUseRebase, r => Some(r.shouldUseRebase)),
                rebasingEligibilityUtil
                  .getRebasingCutOffDate(assetType, wasUkResident),
                representativeType(state)
              )
            )(
              requiredPreviousAnswer = { answers =>
                if (rebaseDate.isDefined)
                  answers
                    .fold(
                      _.rebasedAcquisitionPrice,
                      _.rebasedAcquisitionPrice
                    )
                    .isDefined
                else
                  answers
                    .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))
                    .isDefined
              },
              redirectToIfNoRequiredPreviousAnswer =
                if (rebaseDate.isDefined)
                  routes.AcquisitionDetailsController
                    .rebasedAcquisitionPrice()
                else if (isPeriodOfAdmin(state))
                  routes.AcquisitionDetailsController.periodOfAdminMarketValue()
                else
                  routes.AcquisitionDetailsController.acquisitionPrice()
            )(
              updateState = { (p, answers, draftReturn) =>
                if (
                  answers
                    .fold(_.improvementCosts, c => Some(c.improvementCosts))
                    .map(_.inPounds())
                    .contains(p)
                )
                  draftReturn
                else {
                  val newAnswers = answers.fold(
                    _.copy(improvementCosts = Some(AmountInPence.fromPounds(p))),
                    _.copy(improvementCosts = AmountInPence.fromPounds(p))
                  )

                  commonUpdateDraftReturn(draftReturn, newAnswers)
                }
              }
            )
          }
        }
      }
    }

  def shouldUseRebase(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, _, state, _) =>
        withAssetTypeAndResidentialStatus(state) { (assetType, wasAUkResident) =>
          if (wasAUkResident)
            Redirect(routes.AcquisitionDetailsController.checkYourAnswers())
          else
            Ok(
              shouldUseRebasePage(
                shouldUseRebaseForm,
                routes.AcquisitionDetailsController
                  .rebasedAcquisitionPrice(),
                rebasingEligibilityUtil
                  .getRebasingCutOffDate(assetType, wasAUkResident),
                assetType
              )
            )
        }
      }
    }

  def shouldUseRebaseSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, fillingOutReturn, state, answers) =>
        withAssetTypeAndResidentialStatus(state) { (assetType, wasUkResident) =>
          withAcquisitionDate(answers) { acquisitionDate =>
            if (
              !rebasingEligibilityUtil.isEligibleForRebase(
                wasUkResident,
                assetType,
                acquisitionDate,
                representativeType(state)
              )
            )
              Redirect(
                routes.AcquisitionDetailsController.checkYourAnswers()
              )
            else
              commonSubmitBehaviour(
                fillingOutReturn,
                state,
                answers
              )(
                shouldUseRebaseForm
              )(page =
                shouldUseRebasePage(
                  _,
                  _,
                  rebasingEligibilityUtil
                    .getRebasingCutOffDate(
                      assetType,
                      wasUkResident
                    ),
                  assetType
                )
              )(
                requiredPreviousAnswer = _ => noAnswersRequired,
                redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.shouldUseRebase()
              )(
                updateState = { (p, answers, draftReturn) =>
                  if (
                    answers
                      .fold(
                        _.shouldUseRebase,
                        c => Some(c.shouldUseRebase)
                      )
                      .contains(p)
                  )
                    draftReturn
                  else {
                    val newAnswers = answers.fold(
                      _.copy(shouldUseRebase = Some(p)),
                      _.copy(shouldUseRebase = p)
                    )

                    commonUpdateDraftReturn(draftReturn, newAnswers)
                  }
                }
              )
          }
        }
      }
    }

  def acquisitionFees(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, fillingOutReturn, state, answers) =>
        withAssetTypeAndResidentialStatus(state) { (assetType, wasUkResident) =>
          withAcquisitionDate(answers) { acquisitionDate =>
            commonDisplayBehaviour(answers)(
              form = _.fold(
                _.acquisitionFees.fold(acquisitionFeesForm())(p => acquisitionFeesForm().fill(p.inPounds())),
                c => acquisitionFeesForm().fill(c.acquisitionFees.inPounds())
              )
            )(
              page = acquisitionFeesPage(
                _,
                _,
                fillingOutReturn.subscribedDetails.isATrust,
                answers.fold(_.shouldUseRebase, r => Some(r.shouldUseRebase)),
                rebasingEligibilityUtil
                  .getRebasingCutOffDate(assetType, wasUkResident),
                wasUkResident,
                representativeType(state),
                assetType
              )
            )(
              requiredPreviousAnswer = _.fold(
                _.improvementCosts,
                c => Some(c.improvementCosts)
              ).isDefined,
              redirectToIfNoRequiredPreviousAnswer =
                acquisitionFeeBacklink(fillingOutReturn, wasUkResident, assetType, acquisitionDate)
            )
          }
        }
      }
    }

  def acquisitionFeesSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, fillingOutReturn, state, answers) =>
        withAssetTypeAndResidentialStatus(state) { (assetType, wasUkResident) =>
          withAcquisitionDate(answers) { acquisitionDate =>
            commonSubmitBehaviour(
              fillingOutReturn,
              state,
              answers
            )(
              acquisitionFeesForm(
                Seq(
                  TimeUtils
                    .govDisplayFormat(rebasingEligibilityUtil.getRebasingCutOffDate(assetType, wasUkResident))
                )
              )
            )(page =
              acquisitionFeesPage(
                _,
                _,
                fillingOutReturn.subscribedDetails.isATrust,
                answers.fold(_.shouldUseRebase, r => Some(r.shouldUseRebase)),
                rebasingEligibilityUtil
                  .getRebasingCutOffDate(assetType, wasUkResident),
                wasUkResident,
                representativeType(state),
                assetType
              )
            )(
              requiredPreviousAnswer = _.fold(
                _.improvementCosts,
                c => Some(c.improvementCosts)
              ).isDefined,
              redirectToIfNoRequiredPreviousAnswer =
                acquisitionFeeBacklink(fillingOutReturn, wasUkResident, assetType, acquisitionDate)
            )(
              updateState = { (p, answers, draftReturn) =>
                if (
                  answers
                    .fold(_.acquisitionFees, c => Some(c.acquisitionFees))
                    .map(_.inPounds())
                    .contains(p)
                )
                  draftReturn
                else {
                  val newAnswers = answers.fold(
                    _.copy(acquisitionFees = Some(AmountInPence.fromPounds(p))),
                    _.copy(acquisitionFees = AmountInPence.fromPounds(p))
                  )
                  commonUpdateDraftReturn(draftReturn, newAnswers)
                }
              }
            )
          }
        }
      }
    }

  private def acquisitionFeeBacklink(
    fillingOutReturn: FillingOutReturn,
    wasUkResident: Boolean,
    assetType: AssetType,
    acquisitionDate: AcquisitionDate
  ): Call =
    fillingOutReturn.draftReturn match {
      case d: DraftSingleIndirectDisposalReturn
          if rebasingEligibilityUtil.isEligibleForRebase(
            wasUkResident,
            assetType,
            acquisitionDate,
            d.triageAnswers.representativeType()
          ) =>
        routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
      case _: DraftSingleIndirectDisposalReturn => routes.AcquisitionDetailsController.acquisitionPrice()
      case _                                    => routes.AcquisitionDetailsController.improvementCosts()
    }

  private def isPeriodOfAdmin(state: JourneyState): Boolean =
    state
      .fold(_.triageAnswers, _.triageAnswers)
      .representativeType()
      .contains(PersonalRepresentativeInPeriodOfAdmin)

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) { (_, fillingOutReturn, state, answers) =>
        withAssetTypeAndResidentialStatus(state) { (assetType, wasAUkResident) =>
          answers match {
            case c: CompleteAcquisitionDetailsAnswers =>
              Ok(
                checkYouAnswersPage(
                  c,
                  rebasingEligibilityUtil
                    .getRebasingCutOffDate(assetType, wasAUkResident),
                  wasAUkResident,
                  rebasingEligibilityUtil.isEligibleForRebase(
                    wasAUkResident,
                    assetType,
                    c.acquisitionDate,
                    representativeType(state)
                  ),
                  fillingOutReturn.subscribedDetails.isATrust,
                  representativeType(state),
                  assetType
                )
              )

            case IncompleteAcquisitionDetailsAnswers(
                  None,
                  _,
                  _,
                  _,
                  _,
                  _,
                  _
                ) =>
              Redirect(
                routes.AcquisitionDetailsController.acquisitionMethod()
              )

            case IncompleteAcquisitionDetailsAnswers(
                  _,
                  None,
                  _,
                  _,
                  _,
                  _,
                  _
                ) =>
              Redirect(
                routes.AcquisitionDetailsController.acquisitionDate()
              )

            case IncompleteAcquisitionDetailsAnswers(
                  _,
                  Some(_),
                  None,
                  _,
                  _,
                  _,
                  _
                ) if isPeriodOfAdmin(state) =>
              Redirect(
                routes.AcquisitionDetailsController.periodOfAdminMarketValue()
              )

            case IncompleteAcquisitionDetailsAnswers(
                  _,
                  Some(acquisitionDate),
                  None,
                  _,
                  _,
                  _,
                  _
                )
                if rebasingEligibilityUtil.isEligibleForAcquisitionPrice(
                  wasAUkResident,
                  acquisitionDate.value
                ) =>
              Redirect(
                routes.AcquisitionDetailsController.acquisitionPrice()
              )

            case IncompleteAcquisitionDetailsAnswers(
                  _,
                  Some(acquisitionDate),
                  _,
                  None,
                  _,
                  _,
                  _
                )
                if rebasingEligibilityUtil
                  .isEligibleForRebase(
                    wasAUkResident,
                    assetType,
                    acquisitionDate,
                    representativeType(state)
                  ) =>
              Redirect(
                routes.AcquisitionDetailsController
                  .rebasedAcquisitionPrice()
              )

            case IncompleteAcquisitionDetailsAnswers(
                  _,
                  Some(acquisitionDate),
                  _,
                  _,
                  _,
                  _,
                  None
                )
                if !wasAUkResident && rebasingEligibilityUtil
                  .isEligibleForRebase(
                    wasAUkResident,
                    assetType,
                    acquisitionDate,
                    representativeType(state)
                  ) =>
              Redirect(
                routes.AcquisitionDetailsController.shouldUseRebase()
              )

            case IncompleteAcquisitionDetailsAnswers(
                  _,
                  _,
                  _,
                  _,
                  None,
                  _,
                  _
                ) =>
              Redirect(
                routes.AcquisitionDetailsController.improvementCosts()
              )

            case IncompleteAcquisitionDetailsAnswers(
                  _,
                  _,
                  _,
                  _,
                  _,
                  None,
                  _
                ) =>
              Redirect(
                routes.AcquisitionDetailsController.acquisitionFees()
              )

            case IncompleteAcquisitionDetailsAnswers(
                  Some(m),
                  Some(d),
                  p,
                  r,
                  Some(i),
                  Some(f),
                  b
                ) =>
              p.orElse(r)
                .fold[Future[Result]] {
                  logger.debug(
                    "Could not find acquisition price or rebased acquisition price. Redirecting to task list"
                  )
                  Redirect(controllers.returns.routes.TaskListController.taskList())
                } { acquisitionPrice =>
                  val completeAnswers = CompleteAcquisitionDetailsAnswers(
                    m,
                    d,
                    acquisitionPrice,
                    r,
                    i,
                    f,
                    b.getOrElse(false)
                  )
                  storeCompleteAnswersAndShowCyaPage(
                    completeAnswers,
                    state,
                    fillingOutReturn,
                    assetType,
                    wasAUkResident
                  )
                }
          }
        }
      }
    }

  private def storeCompleteAnswersAndShowCyaPage(
    completeAnswers: CompleteAcquisitionDetailsAnswers,
    state: JourneyState,
    fillingOutReturn: FillingOutReturn,
    assetType: AssetType,
    wasAUkResident: Boolean
  )(implicit request: RequestWithSessionData[_]): Future[Result] = {
    val newDraftReturn =
      state.fold(
        _.copy(acquisitionDetailsAnswers = Some(completeAnswers)),
        _.copy(acquisitionDetailsAnswers = Some(completeAnswers))
      )

    val result = for {
      _ <- returnsService.storeDraftReturn(
             newDraftReturn,
             fillingOutReturn.subscribedDetails.cgtReference,
             fillingOutReturn.agentReferenceNumber
           )
      _ <- EitherT(
             updateSession(sessionStore, request)(
               _.copy(journeyStatus =
                 Some(
                   fillingOutReturn
                     .copy(draftReturn = newDraftReturn)
                 )
               )
             )
           )
    } yield ()

    result.fold(
      { e =>
        logger.warn("Could not update session", e)
        errorHandler.errorResult()
      },
      _ =>
        Ok(
          checkYouAnswersPage(
            completeAnswers,
            rebasingEligibilityUtil
              .getRebasingCutOffDate(assetType, wasAUkResident),
            wasAUkResident,
            rebasingEligibilityUtil
              .isEligibleForRebase(
                wasAUkResident,
                assetType,
                completeAnswers.acquisitionDate,
                representativeType(state)
              ),
            fillingOutReturn.subscribedDetails.isATrust,
            representativeType(state),
            assetType
          )
        )
    )
  }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData { _ =>
      Redirect(controllers.returns.routes.TaskListController.taskList())
    }

  private def shouldRedirectFromRebaseAcquisitionQuestions(
    wasUkResident: Boolean,
    acquisitionDetailsAnswers: AcquisitionDetailsAnswers,
    acquisitionDate: AcquisitionDate
  ): Boolean =
    if (
      wasUkResident && RebasingCutoffDates.ukResidents.isAfter(
        acquisitionDate.value
      )
    )
      acquisitionDetailsAnswers
        .fold(_.acquisitionMethod, c => Some(c.acquisitionMethod))
        .isDefined
    else if (!wasUkResident)
      acquisitionDetailsAnswers
        .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))
        .isDefined
    else true

}

object AcquisitionDetailsController {

  val noAnswersRequired = true

  val acquisitionMethodForm: Form[AcquisitionMethod] = {
    val formatter: Formatter[AcquisitionMethod] = {
      val (acquisitionMethodKey, otherAcquisitionMethodKey) =
        "acquisitionMethod" -> "otherAcquisitionMethod"
      val otherAcquisitionMethodPredicate =
        "^[a-zA-Z0-9 ]{1,35}$".r.pattern.asPredicate()

      def validateOtherAcquisitionMethod(
        s: String
      ): Either[FormError, AcquisitionMethod] =
        if (s.length > 35)
          Left(FormError(otherAcquisitionMethodKey, "error.tooLong"))
        else if (!otherAcquisitionMethodPredicate.test(s))
          Left(FormError(otherAcquisitionMethodKey, "error.invalid"))
        else Right(AcquisitionMethod.Other(s))

      val innerOption = InnerOption { data =>
        FormUtils
          .readValue(otherAcquisitionMethodKey, data, identity)
          .flatMap(validateOtherAcquisitionMethod)
          .leftMap(Seq(_))
      }

      ConditionalRadioUtils.formatter(acquisitionMethodKey)(
        List(
          Right(AcquisitionMethod.Bought),
          Right(AcquisitionMethod.Inherited),
          Right(AcquisitionMethod.Gifted),
          Left(innerOption)
        )
      ) {
        case AcquisitionMethod.Bought       => Map(acquisitionMethodKey -> "0")
        case AcquisitionMethod.Inherited    => Map(acquisitionMethodKey -> "1")
        case AcquisitionMethod.Gifted       => Map(acquisitionMethodKey -> "2")
        case AcquisitionMethod.Other(value) =>
          Map(acquisitionMethodKey -> "3", otherAcquisitionMethodKey -> value)
      }
    }

    Form(
      mapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

  def acquisitionDateForm(today: LocalDate): Form[AcquisitionDate] =
    Form(
      mapping(
        "" -> of(
          TimeUtils.dateFormatter(
            Some(today),
            None,
            "acquisitionDate-day",
            "acquisitionDate-month",
            "acquisitionDate-year",
            "acquisitionDate"
          )
        )
      )(AcquisitionDate(_))(d => Some(d.value))
    )

  val acquisitionPriceForm: Form[BigDecimal] =
    Form(
      mapping(
        "acquisitionPrice" -> of(
          MoneyUtils
            .amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

  val rebasedAcquisitionPriceForm: Form[BigDecimal] =
    Form(
      mapping(
        "rebaseAcquisitionPrice" -> of(
          MoneyUtils
            .amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

  def periodOfAdminMarketValueForm(dateOfDeath: DateOfDeath)(implicit m: Messages): Form[BigDecimal] =
    Form(
      mapping(
        "periodOfAdminMarketValue" -> of(
          MoneyUtils
            .amountInPoundsFormatter(
              _ <= 0,
              _ > MoneyUtils.maxAmountOfPounds,
              requiredErrorArgs = List(TimeUtils.govDisplayFormat(dateOfDeath.value))
            )
        )
      )(identity)(Some(_))
    )

  val shouldUseRebaseForm: Form[Boolean] = Form(
    mapping(
      "shouldUseRebase" -> of(BooleanFormatter.formatter)
    )(identity)(Some(_))
  )

  val improvementCostsForm: Form[BigDecimal] =
    MoneyUtils
      .amountInPoundsYesNoForm("improvementCosts", "improvementCostsValue")

  def acquisitionFeesForm(errorArgs: Seq[String] = Seq.empty): Form[BigDecimal] =
    MoneyUtils
      .amountInPoundsYesNoForm("acquisitionFees", "acquisitionFeesValue", None, errorArgs)

}
