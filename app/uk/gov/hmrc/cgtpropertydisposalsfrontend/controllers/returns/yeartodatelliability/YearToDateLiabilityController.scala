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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability

import java.nio.file.Files.readAllBytes

import akka.stream.scaladsl.{FileIO, Source}
import akka.util.ByteString
import cats.Eq
import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText, of}
import play.api.http.Writeable
import play.api.libs.Files
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.supportingdocs.SupportingEvidenceController.FileUpload
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.yeartodatelliability.YearToDateLiabilityController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.validateAmountOfMoney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.DraftReturnId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.CalculatedYTDAnswers.{CompleteCalculatedYTDAnswers, IncompleteCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.NonCalculatedYTDAnswers.{CompleteNonCalculatedYTDAnswers, IncompleteNonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.YearToDateLiabilityAnswers.{CalculatedYTDAnswers, NonCalculatedYTDAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanFileDescriptor.UpscanFileDescriptorStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UpscanFileDescriptor, UpscanInitiateReference}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, ConditionalRadioUtils, Error, FormUtils, SessionData, TimeUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UpscanService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{CgtCalculationService, ReturnsService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{ytdliability => pages}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class YearToDateLiabilityController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cgtCalculationService: CgtCalculationService,
  upscanService: UpscanService,
  upscanConnector: UpscanConnector,
  cc: MessagesControllerComponents,
  val config: Configuration,
  estimatedIncomePage: pages.estimated_income,
  personalAllowancePage: pages.personal_allowance,
  hasEstimatedDetailsPage: pages.has_estimated_details,
  taxDuePage: pages.tax_due,
  mandatoryEvidencePage: pages.upload_mandatory_evidence,
  calculatedCheckYouAnswersPage: pages.calculated_check_your_answers,
  taxableGainOrLossPage: pages.taxable_gain_or_loss,
  nonCalculatedEnterTaxDuePage: pages.non_calculated_enter_tax_due,
  nonCalculatedCheckYourAnswersPage: pages.non_calculated_check_your_answers,
  expiredMandatoryEvidencePage: pages.expired_mandatory_evidence,
  mandatoryEvidenceCheckUpscanPage: pages.mandatory_evidence_upscan_check
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private val maxFileSize: Int = config.get[Int]("microservice.services.upscan-initiate.max-file-size")

  private def withFillingOutReturnAndYTDLiabilityAnswers(
    request: RequestWithSessionData[_]
  )(
    f: (
      SessionData,
      FillingOutReturn,
      YearToDateLiabilityAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r @ FillingOutReturn(_, _, _, d: DraftSingleDisposalReturn))) =>
        d.yearToDateLiabilityAnswers match {
          case Some(y) => f(s, r, y)

          case None =>
            d.reliefDetailsAnswers match {
              case Some(CompleteReliefDetailsAnswers(_, _, Some(_: OtherReliefsOption.OtherReliefs))) =>
                f(s, r, IncompleteNonCalculatedYTDAnswers.empty)

              case Some(_: CompleteReliefDetailsAnswers) =>
                f(s, r, IncompleteCalculatedYTDAnswers.empty)

              case _ =>
                Redirect(controllers.returns.routes.TaskListController.taskList())
            }
        }

      case Some((s, r @ FillingOutReturn(_, _, _, d: DraftMultipleDisposalsReturn))) =>
        d.yearToDateLiabilityAnswers.fold[Future[Result]](
          f(s, r, IncompleteNonCalculatedYTDAnswers.empty)
        )(f(s, r, _))

      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def withCalculatedTaxDue(
    answers: CalculatedYTDAnswers,
    triageAnswers: CompleteSingleDisposalTriageAnswers,
    disposalDetailsAnswers: CompleteDisposalDetailsAnswers,
    acquisitionDetailsAnswers: CompleteAcquisitionDetailsAnswers,
    reliefDetailsAnswers: CompleteReliefDetailsAnswers,
    exemptionAndLossesAnswers: CompleteExemptionAndLossesAnswers,
    estimatedIncome: AmountInPence,
    personalAllowance: AmountInPence,
    calculateIfMissing: Boolean,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftSingleDisposalReturn
  )(f: CalculatedTaxDue => Future[Result])(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case complete: CompleteCalculatedYTDAnswers => f(complete.calculatedTaxDue)

      case incomplete: IncompleteCalculatedYTDAnswers =>
        incomplete.calculatedTaxDue match {
          case Some(c) => f(c)

          case None if !calculateIfMissing =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case None if calculateIfMissing =>
            val result =
              for {
                calculatedTaxDue <- cgtCalculationService
                                     .calculateTaxDue(
                                       CalculateCgtTaxDueRequest(
                                         triageAnswers,
                                         disposalDetailsAnswers,
                                         acquisitionDetailsAnswers,
                                         reliefDetailsAnswers,
                                         exemptionAndLossesAnswers,
                                         estimatedIncome,
                                         personalAllowance,
                                         draftReturn.initialGainOrLoss,
                                         fillingOutReturn.subscribedDetails.isATrust
                                       )
                                     )
                _ <- EitherT(
                      updateSession(sessionStore, request)(
                        _.copy(
                          journeyStatus = Some(
                            fillingOutReturn.copy(draftReturn =
                              draftReturn.copy(yearToDateLiabilityAnswers =
                                Some(
                                  incomplete.copy(calculatedTaxDue = Some(calculatedTaxDue))
                                )
                              )
                            )
                          )
                        )
                      )
                    )
              } yield calculatedTaxDue

            result
              .biSemiflatMap({ e =>
                logger.warn("Could not get and store calculated tax due", e)
                errorHandler.errorResult()
              }, f)
              .merge
        }

    }

  private def withCompleteJourneys(draftReturn: DraftSingleDisposalReturn)(
    f: (
      CompleteSingleDisposalTriageAnswers,
      CompleteDisposalDetailsAnswers,
      CompleteAcquisitionDetailsAnswers,
      CompleteReliefDetailsAnswers,
      CompleteExemptionAndLossesAnswers
    ) => Future[Result]
  ): Future[Result] =
    (
      draftReturn.triageAnswers,
      draftReturn.disposalDetailsAnswers,
      draftReturn.acquisitionDetailsAnswers,
      draftReturn.reliefDetailsAnswers,
      draftReturn.exemptionAndLossesAnswers
    ) match {
      case (
          t: CompleteSingleDisposalTriageAnswers,
          Some(d: CompleteDisposalDetailsAnswers),
          Some(a: CompleteAcquisitionDetailsAnswers),
          Some(r: CompleteReliefDetailsAnswers),
          Some(e: CompleteExemptionAndLossesAnswers)
          ) =>
        f(t, d, a, r, e)

      case _ =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }

  private def withAssetTypeAndResidentialStatus(
    draftReturn: DraftSingleDisposalReturn
  )(f: (AssetType, Boolean) => Future[Result]): Future[Result] =
    draftReturn.triageAnswers.fold(
      i => i.assetType       -> i.wasAUKResident,
      c => Some(c.assetType) -> Some(c.countryOfResidence.isUk())
    ) match {
      case (Some(a), Some(w)) => f(a, w)
      case _                  => Redirect(controllers.returns.routes.TaskListController.taskList())
    }

  private def withDisposalDate(
    draftReturn: DraftSingleDisposalReturn
  )(f: DisposalDate => Future[Result]): Future[Result] =
    draftReturn.triageAnswers
      .fold(_.disposalDate, c => Some(c.disposalDate))
      .fold[Future[Result]](
        Redirect(controllers.returns.routes.TaskListController.taskList())
      )(f)

  private def withEstimatedIncome(
    answers: CalculatedYTDAnswers
  )(f: AmountInPence => Future[Result]): Future[Result] =
    answers
      .fold(_.estimatedIncome, c => Some(c.estimatedIncome))
      .fold[Future[Result]](Redirect(routes.YearToDateLiabilityController.checkYourAnswers()))(f)

  private def withPersonalAllowance(
    answers: CalculatedYTDAnswers
  )(f: AmountInPence => Future[Result]): Future[Result] =
    withEstimatedIncome(answers) { estimatedIncome =>
      if (estimatedIncome.value > 0L)
        answers
          .fold(_.personalAllowance, c => c.personalAllowance)
          .fold[Future[Result]](
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          )(f)
      else
        f(AmountInPence.zero)
    }

  private def withHasEstimatedDetails(
    answers: YearToDateLiabilityAnswers
  )(f: Boolean => Future[Result]): Future[Result] =
    answers match {
      case _: NonCalculatedYTDAnswers =>
        Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

      case calculatedAnswers: CalculatedYTDAnswers =>
        calculatedAnswers
          .fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
          .fold[Future[Result]](
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          )(f)
    }

  private def withTaxDueAndCalculatedTaxDue(
    answers: CalculatedYTDAnswers
  )(f: (AmountInPence, CalculatedTaxDue) => Future[Result]): Future[Result] = {
    val taxDue           = answers.fold(_.taxDue, c => Some(c.taxDue))
    val calculatedTaxDue = answers.fold(_.calculatedTaxDue, c => Some(c.calculatedTaxDue))

    (taxDue, calculatedTaxDue) match {
      case (Some(t), Some(c)) => f(t, c)
      case _                  => Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
    }
  }

  private def commonDisplayBehaviour[Y <: YearToDateLiabilityAnswers, A, P: Writeable, R](
    currentAnswers: Y
  )(form: Y => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: Y => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      val backLink = currentAnswers match {
        case c: CalculatedYTDAnswers =>
          c.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )

        case n: NonCalculatedYTDAnswers =>
          n.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )
      }

      Ok(page(form(currentAnswers), backLink))
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  private def commonSubmitBehaviour[Y <: YearToDateLiabilityAnswers, D <: DraftReturn: Eq, A, P: Writeable, R](
    currentFillingOutReturn: FillingOutReturn,
    currentDraftReturn: D,
    currentAnswers: Y
  )(form: Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: Y => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(
    updateAnswers: (A, D) => D
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      lazy val backLink = currentAnswers match {
        case c: CalculatedYTDAnswers =>
          c.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )

        case n: NonCalculatedYTDAnswers =>
          n.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )
      }
      form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink)), { value =>
            val newDraftReturn = updateAnswers(value, currentDraftReturn)

            val result = for {
              _ <- if (newDraftReturn === currentDraftReturn) EitherT.pure(())
                  else
                    returnsService.storeDraftReturn(
                      newDraftReturn,
                      currentFillingOutReturn.subscribedDetails.cgtReference,
                      currentFillingOutReturn.agentReferenceNumber
                    )
              _ <- EitherT(
                    updateSession(sessionStore, request)(
                      _.copy(journeyStatus = Some(currentFillingOutReturn.copy(draftReturn = newDraftReturn)))
                    )
                  )
            } yield ()
            result.fold(
              { e =>
                logger.warn("Could not update draft return", e)
                errorHandler.errorResult()
              },
              _ => Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
            )

          }
        )
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  def estimatedIncome(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      (answers, fillingOutReturn.draftReturn) match {
        case (_, _) if isATrust(fillingOutReturn) =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        case (calculatedAnswers: CalculatedYTDAnswers, draftReturn: DraftSingleDisposalReturn) =>
          withAssetTypeAndResidentialStatus(draftReturn) { (_, wasUkResident) =>
            withDisposalDate(draftReturn) { disposalDate =>
              commonDisplayBehaviour(calculatedAnswers)(
                form = _.fold(
                  _.estimatedIncome.fold(estimatedIncomeForm)(a => estimatedIncomeForm.fill(a.inPounds())),
                  c => estimatedIncomeForm.fill(c.estimatedIncome.inPounds())
                )
              )(
                page = estimatedIncomePage(_, _, disposalDate, wasUkResident)
              )(
                requiredPreviousAnswer = _ => Some(()),
                controllers.returns.routes.TaskListController.taskList()
              )
            }
          }

        case _ =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
      }
    }
  }

  def estimatedIncomeSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      (answers, fillingOutReturn.draftReturn) match {
        case (_, _) if isATrust(fillingOutReturn) =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        case (calculatedAnswers: CalculatedYTDAnswers, draftReturn: DraftSingleDisposalReturn) =>
          withAssetTypeAndResidentialStatus(draftReturn) { (_, wasUkResident) =>
            withDisposalDate(draftReturn) { disposalDate =>
              commonSubmitBehaviour(fillingOutReturn, draftReturn, calculatedAnswers)(
                form = estimatedIncomeForm
              )(
                page = { (form, backLink) => estimatedIncomePage(form, backLink, disposalDate, wasUkResident) }
              )(
                requiredPreviousAnswer               = _ => Some(()),
                redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
              )(
                updateAnswers = { (i, draftReturn) =>
                  val estimatedIncome = AmountInPence.fromPounds(i)

                  if (calculatedAnswers
                        .fold(_.estimatedIncome, c => Some(c.estimatedIncome))
                        .contains(estimatedIncome)) {
                    draftReturn
                  } else {
                    val newAnswers =
                      calculatedAnswers
                        .unset(_.personalAllowance)
                        .unset(_.hasEstimatedDetails)
                        .unset(_.calculatedTaxDue)
                        .unset(_.taxDue)
                        .unset(_.mandatoryEvidence)
                        .unset(_.expiredEvidence)
                        .unset(_.upscanSuccessful)
                        .copy(estimatedIncome = Some(AmountInPence.fromPounds(i)))

                    draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
                  }
                }
              )
            }
          }

        case _ =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
      }
    }
  }

  def personalAllowance(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      (answers, fillingOutReturn.draftReturn) match {
        case (_, _) if isATrust(fillingOutReturn) =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        case (calculatedAnswers: CalculatedYTDAnswers, draftReturn: DraftSingleDisposalReturn) =>
          withAssetTypeAndResidentialStatus(draftReturn) { (_, wasUkResident) =>
            withDisposalDate(draftReturn) { disposalDate =>
              withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
                if (estimatedIncome.value > 0L) {
                  commonDisplayBehaviour(calculatedAnswers)(
                    form = { a =>
                      val emptyForm = personalAllowanceForm(disposalDate)
                      a.fold(
                        _.personalAllowance.fold(emptyForm)(a => emptyForm.fill(a.inPounds())),
                        _.personalAllowance.fold(emptyForm)(a => emptyForm.fill(a.inPounds()))
                      )
                    }
                  )(
                    page = personalAllowancePage(_, _, disposalDate, wasUkResident)
                  )(
                    requiredPreviousAnswer = _.fold(_.estimatedIncome, c => Some(c.estimatedIncome)),
                    routes.YearToDateLiabilityController.estimatedIncome()
                  )
                } else {
                  Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
                }
              }
            }
          }
        case _ =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
      }
    }
  }

  def personalAllowanceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      (answers, fillingOutReturn.draftReturn) match {
        case (_, _) if isATrust(fillingOutReturn) =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        case (calculatedAnswers: CalculatedYTDAnswers, draftReturn: DraftSingleDisposalReturn) =>
          withAssetTypeAndResidentialStatus(draftReturn) { (_, wasUkResident) =>
            withDisposalDate(draftReturn) { disposalDate =>
              withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
                if (estimatedIncome.value > 0L) {
                  commonSubmitBehaviour(fillingOutReturn, draftReturn, calculatedAnswers)(
                    form = personalAllowanceForm(disposalDate)
                  )(page = (form, backLink) => personalAllowancePage(form, backLink, disposalDate, wasUkResident))(
                    requiredPreviousAnswer = _.fold(_.estimatedIncome, c => Some(c.estimatedIncome)),
                    routes.YearToDateLiabilityController.estimatedIncome()
                  ) { (p, draftReturn) =>
                    val personalAllowance = AmountInPence.fromPounds(p)
                    if (calculatedAnswers
                          .fold(_.personalAllowance, _.personalAllowance)
                          .contains(personalAllowance)) {
                      draftReturn
                    } else {
                      val newAnswers = calculatedAnswers
                        .unset(_.hasEstimatedDetails)
                        .unset(_.calculatedTaxDue)
                        .unset(_.taxDue)
                        .unset(_.mandatoryEvidence)
                        .unset(_.expiredEvidence)
                        .unset(_.upscanSuccessful)
                        .copy(personalAllowance = Some(AmountInPence.fromPounds(p)))

                      draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))

                    }
                  }
                } else {
                  Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
                }
              }
            }
          }
        case _ =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
      }
    }
  }

  def hasEstimatedDetails(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      (answers, fillingOutReturn.draftReturn) match {
        case (nonCalculatedAnswers: NonCalculatedYTDAnswers, _: DraftReturn) =>
          commonDisplayBehaviour(nonCalculatedAnswers)(
            form = _.fold(
              _.hasEstimatedDetails.fold(hasEstimatedDetailsForm)(hasEstimatedDetailsForm.fill),
              c => hasEstimatedDetailsForm.fill(c.hasEstimatedDetails)
            )
          )(
            page = hasEstimatedDetailsPage(_, _, isATrust(fillingOutReturn))
          )(
            requiredPreviousAnswer = answers =>
              if (isATrust(fillingOutReturn)) {
                Some(AmountInPence(0))
              } else answers.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss)),
            redirectToIfNoRequiredPreviousAnswer =
              if (isATrust(fillingOutReturn)) controllers.returns.routes.TaskListController.taskList()
              else routes.YearToDateLiabilityController.taxableGainOrLoss()
          )

        case (calculatedAnswers: CalculatedYTDAnswers, _: DraftSingleDisposalReturn) if !isATrust(fillingOutReturn) =>
          withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
            displayForEstimatedDetails(fillingOutReturn, calculatedAnswers, Some(estimatedIncome))
          }

        case (calculatedAnswers: CalculatedYTDAnswers, _: DraftSingleDisposalReturn) if isATrust(fillingOutReturn) =>
          displayForEstimatedDetails(fillingOutReturn, calculatedAnswers, None)

        case _ =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

      }
    }
  }

  private def displayForEstimatedDetails(
    fillingOutReturn: FillingOutReturn,
    calculatedAnswers: CalculatedYTDAnswers,
    estimatedIncome: Option[AmountInPence]
  )(implicit request: RequestWithSessionData[_]) =
    commonDisplayBehaviour(calculatedAnswers)(
      form = _.fold(
        _.hasEstimatedDetails.fold(hasEstimatedDetailsForm)(hasEstimatedDetailsForm.fill),
        c => hasEstimatedDetailsForm.fill(c.hasEstimatedDetails)
      )
    )(
      page = hasEstimatedDetailsPage(_, _, isATrust(fillingOutReturn))
    )(
      requiredPreviousAnswer = { a =>
        estimatedIncome match {
          case None                      => Some(AmountInPence(0))
          case Some(ei) if ei.value > 0L => a.fold(_.personalAllowance, _.personalAllowance)
          case _                         => a.fold(_.estimatedIncome, c => Some(c.estimatedIncome))
        }
      },
      redirectToIfNoRequiredPreviousAnswer = {
        estimatedIncome match {
          case None                      => controllers.returns.routes.TaskListController.taskList()
          case Some(ei) if ei.value > 0L => routes.YearToDateLiabilityController.personalAllowance()
          case _                         => routes.YearToDateLiabilityController.estimatedIncome()
        }
      }
    )

  def hasEstimatedDetailsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      (answers, fillingOutReturn.draftReturn) match {
        case (nonCalculatedAnswers: NonCalculatedYTDAnswers, draftReturn: DraftReturn) =>
          handleNonCalculatedEstimatedDetailsSubmit(nonCalculatedAnswers, draftReturn, fillingOutReturn)

        case (calculatedAnswers: CalculatedYTDAnswers, draftReturn: DraftSingleDisposalReturn) =>
          handleNonCalculatedEstimatedDetailsSubmit(calculatedAnswers, draftReturn, fillingOutReturn)

        case _ =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
      }
    }
  }

  private def handleNonCalculatedEstimatedDetailsSubmit(
    calculatedAnswers: CalculatedYTDAnswers,
    draftReturn: DraftSingleDisposalReturn,
    fillingOutReturn: FillingOutReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    if (isATrust(fillingOutReturn))
      estimatedDetailsSubmit(calculatedAnswers, draftReturn, fillingOutReturn, None)
    else
      withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
        estimatedDetailsSubmit(calculatedAnswers, draftReturn, fillingOutReturn, Some(estimatedIncome))
      }

  private def estimatedDetailsSubmit(
    calculatedAnswers: CalculatedYTDAnswers,
    draftReturn: DraftSingleDisposalReturn,
    fillingOutReturn: FillingOutReturn,
    estimatedIncome: Option[AmountInPence]
  )(implicit request: RequestWithSessionData[_]) =
    commonSubmitBehaviour(fillingOutReturn, draftReturn, calculatedAnswers)(
      form = hasEstimatedDetailsForm
    )(
      page = hasEstimatedDetailsPage(_, _, isATrust(fillingOutReturn))
    )(
      requiredPreviousAnswer = { a =>
        estimatedIncome match {
          case None                      => Some(AmountInPence(0))
          case Some(ei) if ei.value > 0L => a.fold(_.personalAllowance, _.personalAllowance)
          case _                         => a.fold(_.estimatedIncome, c => Some(c.estimatedIncome))
        }
      },
      redirectToIfNoRequiredPreviousAnswer = {
        estimatedIncome match {
          case None                      => controllers.returns.routes.TaskListController.taskList()
          case Some(ei) if ei.value > 0L => routes.YearToDateLiabilityController.personalAllowance()
          case _                         => routes.YearToDateLiabilityController.estimatedIncome()
        }
      }
    ) { (hasEstimated, draftReturn) =>
      if (calculatedAnswers
            .fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
            .contains(hasEstimated)) {
        draftReturn
      } else {
        val newAnswers = calculatedAnswers
          .unset(_.calculatedTaxDue)
          .unset(_.taxDue)
          .unset(_.mandatoryEvidence)
          .unset(_.upscanSuccessful)
          .copy(hasEstimatedDetails = Some(hasEstimated))

        draftReturn.copy(yearToDateLiabilityAnswers = Some(newAnswers))
      }
    }

  private def handleNonCalculatedEstimatedDetailsSubmit(
    nonCalculatedAnswers: NonCalculatedYTDAnswers,
    draftReturn: DraftReturn,
    fillingOutReturn: FillingOutReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    commonSubmitBehaviour(fillingOutReturn, draftReturn, nonCalculatedAnswers)(
      form = hasEstimatedDetailsForm
    )(
      page = hasEstimatedDetailsPage(_, _, isATrust(fillingOutReturn))
    )(
      requiredPreviousAnswer = answers =>
        if (isATrust(fillingOutReturn)) Some(AmountInPence(0))
        else answers.fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss)),
      redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.taxableGainOrLoss()
    ) { (hasEstimated, draftReturn) =>
      if (nonCalculatedAnswers
            .fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails))
            .contains(hasEstimated)) {
        draftReturn
      } else {
        val newAnswers = nonCalculatedAnswers
          .unset(_.taxDue)
          .unset(_.mandatoryEvidence)
          .unset(_.expiredEvidence)
          .unset(_.upscanSuccessful)
          .copy(hasEstimatedDetails = Some(hasEstimated))

        draftReturn.fold(
          _.copy(
            yearToDateLiabilityAnswers = Some(newAnswers)
          ),
          _.copy(yearToDateLiabilityAnswers = Some(newAnswers))
        )
      }
    }

  def taxDue(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      (answers, fillingOutReturn.draftReturn) match {
        case (calculatedAnswers: CalculatedYTDAnswers, draftReturn: DraftSingleDisposalReturn)
            if isATrust(fillingOutReturn) =>
          taxDueDisplay(fillingOutReturn, calculatedAnswers, draftReturn, AmountInPence(0), AmountInPence(0))
        case (calculatedAnswers: CalculatedYTDAnswers, draftReturn: DraftSingleDisposalReturn) =>
          withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
            withPersonalAllowance(calculatedAnswers) { personalAllowance =>
              taxDueDisplay(fillingOutReturn, calculatedAnswers, draftReturn, estimatedIncome, personalAllowance)
            }
          }

        case _ =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

      }
    }
  }

  private def taxDueDisplay(
    fillingOutReturn: FillingOutReturn,
    calculatedAnswers: CalculatedYTDAnswers,
    draftReturn: DraftSingleDisposalReturn,
    estimatedIncome: AmountInPence,
    personalAllowance: AmountInPence
  )(implicit request: RequestWithSessionData[_]) =
    withHasEstimatedDetails(calculatedAnswers) { _ =>
      withCompleteJourneys(draftReturn) {
        (triage, disposalDetails, acquisitionDetails, reliefDetails, exemptionsAndLossesDetails) =>
          withCalculatedTaxDue(
            calculatedAnswers,
            triage,
            disposalDetails,
            acquisitionDetails,
            reliefDetails,
            exemptionsAndLossesDetails,
            estimatedIncome,
            personalAllowance,
            calculateIfMissing = true,
            fillingOutReturn,
            draftReturn
          ) { calculatedTaxDue =>
            commonDisplayBehaviour(calculatedAnswers)(
              form = _.fold(
                _.taxDue.fold(taxDueForm)(t => taxDueForm.fill(t.inPounds())),
                c => taxDueForm.fill(c.taxDue.inPounds())
              )
            )(page =
              taxDuePage(
                _,
                _,
                triage,
                disposalDetails,
                acquisitionDetails,
                reliefDetails,
                exemptionsAndLossesDetails,
                estimatedIncome,
                personalAllowance,
                calculatedTaxDue,
                fillingOutReturn.subscribedDetails.isATrust
              )
            )(
              _.fold(
                _.hasEstimatedDetails,
                c => Some(c.hasEstimatedDetails)
              ),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            )
          }
      }
    }

  def taxDueSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      (answers, fillingOutReturn.draftReturn) match {
        case (calculatedAnswers: CalculatedYTDAnswers, draftReturn: DraftSingleDisposalReturn)
            if isATrust(fillingOutReturn) =>
          baseTaxDueSubmit(fillingOutReturn, calculatedAnswers, draftReturn, AmountInPence(0), AmountInPence(0))
        case (calculatedAnswers: CalculatedYTDAnswers, draftReturn: DraftSingleDisposalReturn) =>
          withEstimatedIncome(calculatedAnswers) { estimatedIncome =>
            withPersonalAllowance(calculatedAnswers) { personalAllowance =>
              baseTaxDueSubmit(fillingOutReturn, calculatedAnswers, draftReturn, estimatedIncome, personalAllowance)
            }
          }

        case _ =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

      }
    }
  }

  private def baseTaxDueSubmit(
    fillingOutReturn: FillingOutReturn,
    calculatedAnswers: CalculatedYTDAnswers,
    draftReturn: DraftSingleDisposalReturn,
    estimatedIncome: AmountInPence,
    personalAllowance: AmountInPence
  )(implicit request: RequestWithSessionData[_]) =
    withHasEstimatedDetails(calculatedAnswers) { _ =>
      withCompleteJourneys(draftReturn) {
        (triage, disposalDetails, acquisitionDetails, reliefDetails, exemptionsAndLossesDetails) =>
          withCalculatedTaxDue(
            calculatedAnswers,
            triage,
            disposalDetails,
            acquisitionDetails,
            reliefDetails,
            exemptionsAndLossesDetails,
            estimatedIncome,
            personalAllowance,
            calculateIfMissing = false,
            fillingOutReturn,
            draftReturn
          ) { calculatedTaxDue =>
            commonSubmitBehaviour(fillingOutReturn, draftReturn, calculatedAnswers)(
              form = taxDueForm
            )(page =
              taxDuePage(
                _,
                _,
                triage,
                disposalDetails,
                acquisitionDetails,
                reliefDetails,
                exemptionsAndLossesDetails,
                estimatedIncome,
                personalAllowance,
                calculatedTaxDue,
                fillingOutReturn.subscribedDetails.isATrust
              )
            )(
              _.fold(
                _.hasEstimatedDetails,
                c => Some(c.hasEstimatedDetails)
              ),
              routes.YearToDateLiabilityController.hasEstimatedDetails()
            ) { (t, draftReturn) =>
              val taxDue = AmountInPence.fromPounds(t)
              if (calculatedAnswers.fold(_.taxDue, c => Some(c.taxDue)).contains(taxDue)) {
                draftReturn
              } else {
                val updatedAnswers =
                  calculatedAnswers
                    .unset(_.mandatoryEvidence)
                    .unset(_.expiredEvidence)
                    .unset(_.upscanSuccessful)
                    .copy(taxDue = Some(taxDue))
                draftReturn.copy(yearToDateLiabilityAnswers = Some(updatedAnswers))
              }
            }
          }
      }
    }

  def uploadMandatoryEvidence(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      def commonDisposalMandatoryEvidence(backLink: Call): Future[Result] =
        upscanService
          .initiate(
            DraftReturnId(fillingOutReturn.draftReturn.id.toString),
            fillingOutReturn.subscribedDetails.cgtReference,
            TimeUtils.now()
          )
          .fold(
            { e =>
              logger.warn("Could not initiate upscan", e)
              errorHandler.errorResult()
            },
            success => Ok(mandatoryEvidencePage(mandatoryEvidenceForm, success.upscanInitiateReference, backLink))
          )

      (answers, fillingOutReturn.draftReturn) match {
        case (calculatedAnswers: CalculatedYTDAnswers, _: DraftSingleDisposalReturn) =>
          withTaxDueAndCalculatedTaxDue(calculatedAnswers) { (taxDue, calculatedTaxDue) =>
            if (calculatedTaxDue.amountOfTaxDue === taxDue) {
              Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
            } else {
              val backLink = calculatedAnswers.fold(
                _ => routes.YearToDateLiabilityController.taxDue(),
                _ => routes.YearToDateLiabilityController.checkYourAnswers()
              )
              commonDisposalMandatoryEvidence(backLink)
            }
          }

        case (nonCalculatedYTDAnswers: NonCalculatedYTDAnswers, _) =>
          val backLink = nonCalculatedYTDAnswers.fold(
            _ => routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue(),
            _ => routes.YearToDateLiabilityController.checkYourAnswers()
          )
          commonDisposalMandatoryEvidence(backLink)

        case _ =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

      }
    }
  }

  def uploadMandatoryEvidenceSubmit(): Action[MultipartFormData[Files.TemporaryFile]] =
    authenticatedActionWithSessionData(parse.multipartFormData(maxFileSize)).async { implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        (answers, fillingOutReturn.draftReturn) match {
          case (calculatedAnswers: CalculatedYTDAnswers, draftReturn: DraftSingleDisposalReturn) =>
            withTaxDueAndCalculatedTaxDue(calculatedAnswers) { (taxDue, calculatedTaxDue) =>
              if (calculatedTaxDue.amountOfTaxDue === taxDue) {
                Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
              } else {
                lazy val backLink = calculatedAnswers.fold(
                  _ => routes.YearToDateLiabilityController.taxDue(),
                  _ => routes.YearToDateLiabilityController.checkYourAnswers()
                )
                handleMandatoryEvidenceSubmit(calculatedAnswers, draftReturn, fillingOutReturn, backLink)
              }
            }

          case (nonCalculatedYTDAnswers: NonCalculatedYTDAnswers, draftReturn) =>
            val backLink = nonCalculatedYTDAnswers.fold(
              _ => routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue(),
              _ => routes.YearToDateLiabilityController.checkYourAnswers()
            )
            handleMandatoryEvidenceSubmit(nonCalculatedYTDAnswers, draftReturn, fillingOutReturn, backLink)

          case _ =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        }
      }
    }

  private def handleMandatoryEvidenceSubmit(
    currentAnswers: YearToDateLiabilityAnswers,
    currentDraftReturn: DraftReturn,
    currentJourney: FillingOutReturn,
    backLink: Call
  )(implicit request: RequestWithSessionData[MultipartFormData[Files.TemporaryFile]]): Future[Result] = {
    val multiPartFormData = request.body
    val upscanReference   = multiPartFormData.dataParts.get("reference").flatMap(_.headOption)
    val mandatoryEvidence = multiPartFormData.file("file")

    (upscanReference, mandatoryEvidence) match {
      case (None, Some(_)) =>
        logger.warn("Could not find upscan file descriptor id")
        errorHandler.errorResult()

      case (Some(_), None) =>
        logger.warn("Could not find file in form")
        errorHandler.errorResult()

      case (None, None) =>
        logger.warn("Could not find upscan file descriptor id or file in form")
        errorHandler.errorResult()

      case (Some(upscanReference), Some(file)) =>
        if (file.filename.trim().isEmpty)
          BadRequest(
            mandatoryEvidencePage(
              mandatoryEvidenceForm.bindFromRequest(multiPartFormData.asFormUrlEncoded),
              UpscanInitiateReference(upscanReference),
              backLink
            )
          )
        else {
          val result = for {
            fileDescriptor <- uploadFile(
                               currentDraftReturn,
                               UpscanInitiateReference(upscanReference),
                               multiPartFormData,
                               readAllBytes(file.ref).size
                             )
            _ <- updateMandatoryEvidence(
                  currentAnswers,
                  currentDraftReturn,
                  currentJourney,
                  upscanReference,
                  file.filename,
                  fileDescriptor
                )
          } yield ()

          result.fold(
            error => {
              logger.warn(s"failed to upload file with error: $error")
              errorHandler.errorResult()
            },
            _ => Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
          )
        }
    }
  }

  private def updateMandatoryEvidence(
    currentAnswers: YearToDateLiabilityAnswers,
    currentDraftReturn: DraftReturn,
    currentJourney: FillingOutReturn,
    upscanReference: String,
    filename: String,
    fileDescriptor: UpscanFileDescriptor
  )(implicit request: RequestWithSessionData[_]): EitherT[Future, Error, Unit] = {
    val newMandatoryEvidence = MandatoryEvidence(upscanReference, filename, fileDescriptor.timestamp)
    val newAnswers = currentAnswers match {
      case c: CalculatedYTDAnswers =>
        c.unset(_.expiredEvidence).unset(_.upscanSuccessful).copy(mandatoryEvidence = Some(newMandatoryEvidence))
      case n: NonCalculatedYTDAnswers =>
        n.unset(_.expiredEvidence).unset(_.upscanSuccessful).copy(mandatoryEvidence = Some(newMandatoryEvidence))
    }

    val newDraftReturn = currentDraftReturn.fold(
      _.copy(yearToDateLiabilityAnswers = Some(newAnswers)),
      _.copy(yearToDateLiabilityAnswers = Some(newAnswers))
    )

    for {
      _ <- returnsService.storeDraftReturn(
            newDraftReturn,
            currentJourney.subscribedDetails.cgtReference,
            currentJourney.agentReferenceNumber
          )
      _ <- EitherT(
            updateSession(sessionStore, request)(
              _.copy(journeyStatus = Some(currentJourney.copy(draftReturn = newDraftReturn)))
            )
          )
    } yield ()
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def uploadFile(
    draftReturn: DraftReturn,
    upscanInitiateReference: UpscanInitiateReference,
    submittedFile: MultipartFormData[Files.TemporaryFile],
    submittedFileSize: Int
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanFileDescriptor] =
    for {
      maybeUpscanFileDescriptor <- upscanService
                                    .getUpscanFileDescriptor(
                                      DraftReturnId(draftReturn.id.toString),
                                      upscanInitiateReference
                                    )
      upscanFileDescriptor <- EitherT
                               .fromOption(
                                 maybeUpscanFileDescriptor,
                                 Error("failed to retrieve upscan file descriptor details")
                               )
      prepared <- EitherT.pure[Future, Error](
                   handleGetFileDescriptorResult(submittedFile, upscanFileDescriptor)
                 )
      _ <- upscanConnector
            .upload(upscanFileDescriptor.fileDescriptor.uploadRequest.href, prepared, submittedFileSize)
      _ <- upscanConnector
            .updateUpscanFileDescriptorStatus(upscanFileDescriptor.copy(status = UpscanFileDescriptorStatus.UPLOADED))
    } yield upscanFileDescriptor

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Any"))
  private def handleGetFileDescriptorResult(
    multipart: MultipartFormData[Files.TemporaryFile],
    upscanFileDescriptor: UpscanFileDescriptor
  ): MultipartFormData[Source[ByteString, Any]] = {
    val userFile =
      multipart.files
        .map(file => file.copy(ref = FileIO.fromPath(file.ref.path): Source[ByteString, Any]))
    multipart
      .copy(
        files = userFile,
        dataParts = upscanFileDescriptor.fileDescriptor.uploadRequest.fields
          .mapValues(fieldValue => Seq(fieldValue))
      )
  }

  def taxableGainOrLoss(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      answers match {
        case (_: CalculatedYTDAnswers) =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        case (nonCalculatedAnswers: NonCalculatedYTDAnswers) =>
          commonDisplayBehaviour(
            nonCalculatedAnswers
          )(form =
            _.fold(
              _.taxableGainOrLoss.fold(taxableGainOrLossForm)(a => taxableGainOrLossForm.fill(a.inPounds())),
              c => taxableGainOrLossForm.fill(c.taxableGainOrLoss.inPounds())
            )
          )(
            page = taxableGainOrLossPage(
              _,
              _,
              fillingOutReturn.subscribedDetails.isATrust,
              fillingOutReturn.draftReturn match {
                case _: DraftMultipleDisposalsReturn => true
                case _: DraftSingleDisposalReturn    => false
              }
            )
          )(
            requiredPreviousAnswer               = _ => Some(()),
            redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
          )
      }
    }
  }

  def taxableGainOrLossSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      answers match {
        case _: CalculatedYTDAnswers =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        case nonCalculatedAnswers: NonCalculatedYTDAnswers =>
          commonSubmitBehaviour(
            fillingOutReturn,
            fillingOutReturn.draftReturn,
            nonCalculatedAnswers
          )(form = taxableGainOrLossForm)(
            page = taxableGainOrLossPage(
              _,
              _,
              fillingOutReturn.subscribedDetails.isATrust,
              fillingOutReturn.draftReturn match {
                case _: DraftMultipleDisposalsReturn => true
                case _: DraftSingleDisposalReturn    => false
              }
            )
          )(
            requiredPreviousAnswer               = _ => Some(()),
            redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
          ) { (amount, draftReturn) =>
            val taxableGainOrLoss = AmountInPence.fromPounds(amount)
            if (nonCalculatedAnswers
                  .fold(_.taxableGainOrLoss, c => Some(c.taxableGainOrLoss))
                  .contains(taxableGainOrLoss))
              draftReturn
            else {
              val newAnswers = nonCalculatedAnswers
                .unset(_.hasEstimatedDetails)
                .unset(_.taxDue)
                .unset(_.mandatoryEvidence)
                .unset(_.expiredEvidence)
                .unset(_.upscanSuccessful)
                .copy(taxableGainOrLoss = Some(taxableGainOrLoss))

              draftReturn.fold(
                _.copy(yearToDateLiabilityAnswers = Some(newAnswers)),
                _.copy(yearToDateLiabilityAnswers = Some(newAnswers))
              )
            }
          }
      }
    }
  }

  def nonCalculatedEnterTaxDue(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, _, answers) =>
      answers match {
        case _: CalculatedYTDAnswers =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

        case nonCalculatedAnswers: NonCalculatedYTDAnswers =>
          commonDisplayBehaviour(
            nonCalculatedAnswers
          )(form =
            _.fold(
              _.taxDue.fold(nonCalculatedTaxDueForm)(a => nonCalculatedTaxDueForm.fill(a.inPounds())),
              c => nonCalculatedTaxDueForm.fill(c.taxDue.inPounds())
            )
          )(
            page = nonCalculatedEnterTaxDuePage(_, _)
          )(
            requiredPreviousAnswer               = _.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)),
            redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.hasEstimatedDetails()
          )

      }
    }
  }

  def nonCalculatedEnterTaxDueSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
        answers match {
          case _: CalculatedYTDAnswers =>
            Redirect(routes.YearToDateLiabilityController.checkYourAnswers())

          case nonCalculatedAnswers: NonCalculatedYTDAnswers =>
            commonSubmitBehaviour(
              fillingOutReturn,
              fillingOutReturn.draftReturn,
              nonCalculatedAnswers
            )(form = nonCalculatedTaxDueForm)(
              page = nonCalculatedEnterTaxDuePage(_, _)
            )(
              requiredPreviousAnswer               = _.fold(_.hasEstimatedDetails, c => Some(c.hasEstimatedDetails)),
              redirectToIfNoRequiredPreviousAnswer = routes.YearToDateLiabilityController.hasEstimatedDetails()
            ) { (amount, draftReturn) =>
              val taxDue = AmountInPence.fromPounds(amount)

              if (nonCalculatedAnswers.fold(_.taxDue, c => Some(c.taxDue)).contains(taxDue))
                draftReturn
              else {
                val newAnswers = nonCalculatedAnswers
                  .unset(_.mandatoryEvidence)
                  .unset(_.expiredEvidence)
                  .unset(_.upscanSuccessful)
                  .copy(taxDue = Some(taxDue))
                draftReturn.fold(
                  _.copy(yearToDateLiabilityAnswers = Some(newAnswers)),
                  _.copy(yearToDateLiabilityAnswers = Some(newAnswers))
                )
              }
            }
        }
      }
  }

  def mandatoryEvidenceExpired(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, _, answers) =>
      answers match {
        case IncompleteNonCalculatedYTDAnswers(_, _, _, _, Some(expired), _) =>
          Ok(expiredMandatoryEvidencePage(expired))

        case IncompleteCalculatedYTDAnswers(_, _, _, _, _, _, Some(expired), _) =>
          Ok(expiredMandatoryEvidencePage(expired))

        case _ =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
      }
    }
  }

  def scanningMandatoryEvidence(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      def checkAndHandleUpscanStatus(
        mandatoryEvidence: MandatoryEvidence,
        answers: Either[IncompleteNonCalculatedYTDAnswers, IncompleteCalculatedYTDAnswers]
      ): Future[Result] = {
        val result = for {
          maybeFileDescriptor <- upscanService
                                  .getUpscanFileDescriptor(
                                    DraftReturnId(fillingOutReturn.draftReturn.id.toString),
                                    UpscanInitiateReference(mandatoryEvidence.reference)
                                  )
          fileDescriptor <- EitherT.fromOption[Future](
                             maybeFileDescriptor,
                             Error(
                               s"Could not find file descriptor for mandatory evidence with reference ${mandatoryEvidence.reference}"
                             )
                           )
          _ <- fileDescriptor.status match {
                case UpscanFileDescriptorStatus.READY =>
                  storeUpscanSuccessOrFailure(upscanSuccess = true, answers, fillingOutReturn)
                case UpscanFileDescriptorStatus.FAILED =>
                  storeUpscanSuccessOrFailure(upscanSuccess = false, answers, fillingOutReturn)
                case _ => EitherT.pure[Future, Error](())
              }
        } yield fileDescriptor.status

        result.fold(
          { e =>
            logger.warn("Error while trying to get and handle an upscan status", e)
            errorHandler.errorResult()
          },
          upscanStatus =>
            if (upscanStatus === UpscanFileDescriptorStatus.FAILED)
              Ok(mandatoryEvidenceCheckUpscanPage(mandatoryEvidence, hasFailed = true))
            else if (upscanStatus === UpscanFileDescriptorStatus.UPLOADED)
              Ok(mandatoryEvidenceCheckUpscanPage(mandatoryEvidence, hasFailed = false))
            else
              Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
        )
      }

      answers match {
        case n @ IncompleteNonCalculatedYTDAnswers(_, _, _, Some(mandatoryEvidence), _, None) =>
          checkAndHandleUpscanStatus(mandatoryEvidence, Left(n))

        case c @ IncompleteCalculatedYTDAnswers(_, _, _, _, _, Some(mandatoryEvidence), _, None) =>
          checkAndHandleUpscanStatus(mandatoryEvidence, Right(c))

        case IncompleteNonCalculatedYTDAnswers(_, _, _, Some(mandatoryEvidence), _, Some(false)) =>
          Ok(mandatoryEvidenceCheckUpscanPage(mandatoryEvidence, hasFailed = true))

        case IncompleteCalculatedYTDAnswers(_, _, _, _, _, Some(mandatoryEvidence), _, Some(false)) =>
          Ok(mandatoryEvidenceCheckUpscanPage(mandatoryEvidence, hasFailed = true))

        case _ =>
          Redirect(routes.YearToDateLiabilityController.checkYourAnswers())
      }
    }
  }

  def scanningMandatoryEvidenceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData { _ =>
    Redirect(routes.YearToDateLiabilityController.scanningMandatoryEvidence())
  }

  private def storeUpscanSuccessOrFailure(
    upscanSuccess: Boolean,
    answers: Either[IncompleteNonCalculatedYTDAnswers, IncompleteCalculatedYTDAnswers],
    fillingOutReturn: FillingOutReturn
  )(implicit request: RequestWithSessionData[_], hc: HeaderCarrier): EitherT[Future, Error, Unit] = {
    val newAnswers = answers.fold(
      _.copy(upscanSuccessful = Some(upscanSuccess)),
      _.copy(upscanSuccessful = Some(upscanSuccess))
    )
    val newDraftReturn = fillingOutReturn.draftReturn.fold(
      _.copy(yearToDateLiabilityAnswers = Some(newAnswers)),
      _.copy(yearToDateLiabilityAnswers = Some(newAnswers))
    )

    for {
      _ <- returnsService.storeDraftReturn(
            newDraftReturn,
            fillingOutReturn.subscribedDetails.cgtReference,
            fillingOutReturn.agentReferenceNumber
          )
      _ <- EitherT(
            updateSession(sessionStore, request)(
              _.copy(journeyStatus =
                Some(
                  fillingOutReturn.copy(draftReturn = newDraftReturn)
                )
              )
            )
          )
    } yield ()
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) { (_, fillingOutReturn, answers) =>
      (answers, fillingOutReturn.draftReturn) match {
        case (c: CalculatedYTDAnswers, s: DraftSingleDisposalReturn) =>
          withDisposalDate(s)(disposalDate => checkYourAnswersHandleCalculated(c, fillingOutReturn, s, disposalDate))

        case (n: NonCalculatedYTDAnswers, d) =>
          checkYourAnswersHandleNonCalculated(n, fillingOutReturn, d)

        case (_: CalculatedYTDAnswers, _: DraftMultipleDisposalsReturn) =>
          logger.warn("Found calculated year to date liability answers on a multiple disposals draft return")
          errorHandler.errorResult()

      }
    }

  }

  private def checkYourAnswersHandleNonCalculated(
    answers: NonCalculatedYTDAnswers,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftReturn
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case IncompleteNonCalculatedYTDAnswers(_, _, _, _, expiredEvidence, _) if expiredEvidence.nonEmpty =>
        Redirect(routes.YearToDateLiabilityController.mandatoryEvidenceExpired())

      case IncompleteNonCalculatedYTDAnswers(_, _, _, Some(mandatoryEvidence), _, upscanSuccess)
          if !upscanSuccess.contains(true) =>
        Redirect(routes.YearToDateLiabilityController.scanningMandatoryEvidence())

      case IncompleteNonCalculatedYTDAnswers(None, _, _, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.taxableGainOrLoss())

      case IncompleteNonCalculatedYTDAnswers(_, None, _, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.hasEstimatedDetails())

      case IncompleteNonCalculatedYTDAnswers(_, _, None, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.nonCalculatedEnterTaxDue())

      case IncompleteNonCalculatedYTDAnswers(_, _, _, None, _, _) =>
        Redirect(routes.YearToDateLiabilityController.uploadMandatoryEvidence())

      case IncompleteNonCalculatedYTDAnswers(Some(t), Some(e), Some(d), Some(m), _, _) =>
        val completeAnswers = CompleteNonCalculatedYTDAnswers(t, e, d, m)
        val updatedDraftReturn = draftReturn.fold(
          _.copy(yearToDateLiabilityAnswers = Some(completeAnswers)),
          _.copy(yearToDateLiabilityAnswers = Some(completeAnswers))
        )
        val updatedJourney = fillingOutReturn.copy(draftReturn = updatedDraftReturn)
        val result = for {
          _ <- returnsService.storeDraftReturn(
                updatedDraftReturn,
                fillingOutReturn.subscribedDetails.cgtReference,
                fillingOutReturn.agentReferenceNumber
              )
          _ <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
        } yield ()

        result.fold(
          { e =>
            logger.warn("Cold not store complete year to date liability answers", e)
            errorHandler.errorResult()
          },
          _ => Ok(nonCalculatedCheckYourAnswersPage(completeAnswers))
        )

      case c: CompleteNonCalculatedYTDAnswers =>
        Ok(nonCalculatedCheckYourAnswersPage(c))

    }

  private def isATrust(fillingOutReturn: FillingOutReturn): Boolean = fillingOutReturn.subscribedDetails.isATrust

  private def checkYourAnswersHandleCalculated(
    answers: CalculatedYTDAnswers,
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftSingleDisposalReturn,
    disposalDate: DisposalDate
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    answers match {
      case c: CompleteCalculatedYTDAnswers =>
        Ok(calculatedCheckYouAnswersPage(c, disposalDate))

      case IncompleteCalculatedYTDAnswers(_, _, _, _, _, _, expiredEvidence, _) if expiredEvidence.nonEmpty =>
        Redirect(routes.YearToDateLiabilityController.mandatoryEvidenceExpired())

      case IncompleteCalculatedYTDAnswers(_, _, _, _, _, Some(mandatoryEvidence), _, upscanSuccess)
          if !upscanSuccess.contains(true) =>
        Redirect(routes.YearToDateLiabilityController.scanningMandatoryEvidence())

      case IncompleteCalculatedYTDAnswers(None, _, _, _, _, _, _, _) if !isATrust(fillingOutReturn) =>
        Redirect(routes.YearToDateLiabilityController.estimatedIncome())

      case IncompleteCalculatedYTDAnswers(Some(p), None, _, _, _, _, _, _)
          if !isATrust(fillingOutReturn) && p.value > 0L =>
        Redirect(routes.YearToDateLiabilityController.personalAllowance())

      case IncompleteCalculatedYTDAnswers(_, _, None, _, _, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.hasEstimatedDetails())

      case IncompleteCalculatedYTDAnswers(_, _, _, None, _, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.taxDue())

      case IncompleteCalculatedYTDAnswers(_, _, _, _, None, _, _, _) =>
        Redirect(routes.YearToDateLiabilityController.taxDue())

      case IncompleteCalculatedYTDAnswers(
          _,
          _,
          Some(_),
          Some(calculatedTaxDue),
          Some(taxDue),
          None,
          _,
          _
          ) if calculatedTaxDue.amountOfTaxDue =!= taxDue =>
        Redirect(routes.YearToDateLiabilityController.uploadMandatoryEvidence())

      case IncompleteCalculatedYTDAnswers(_, _, Some(h), Some(c), Some(t), m, _, _) if isATrust(fillingOutReturn) =>
        fromIncompleteToCompleteCalculatedYTDAnswers(
          fillingOutReturn,
          draftReturn,
          AmountInPence(0),
          Some(AmountInPence(0)),
          h,
          c,
          t,
          m,
          disposalDate
        )
      case IncompleteCalculatedYTDAnswers(Some(e), p, Some(h), Some(c), Some(t), m, _, _) =>
        fromIncompleteToCompleteCalculatedYTDAnswers(fillingOutReturn, draftReturn, e, p, h, c, t, m, disposalDate)
    }

  private def fromIncompleteToCompleteCalculatedYTDAnswers(
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftSingleDisposalReturn,
    e: AmountInPence,
    p: Option[AmountInPence],
    h: Boolean,
    c: CalculatedTaxDue,
    t: AmountInPence,
    m: Option[MandatoryEvidence],
    disposalDate: DisposalDate
  )(implicit request: RequestWithSessionData[_]): Future[Result] = {
    val completeAnswers = CompleteCalculatedYTDAnswers(e, p, h, c, t, m)
    val newDraftReturn =
      draftReturn.copy(yearToDateLiabilityAnswers = Some(completeAnswers))

    val result = for {
      _ <- returnsService.storeDraftReturn(
            newDraftReturn,
            fillingOutReturn.subscribedDetails.cgtReference,
            fillingOutReturn.agentReferenceNumber
          )
      _ <- EitherT(
            updateSession(sessionStore, request)(
              _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = newDraftReturn)))
            )
          )
    } yield ()

    result.fold({ e =>
      logger.warn("Could not update session", e)
      errorHandler.errorResult()
    }, _ => Ok(calculatedCheckYouAnswersPage(completeAnswers, disposalDate)))
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndYTDLiabilityAnswers(request) {
      case _ =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }
  }

}

object YearToDateLiabilityController {

  val estimatedIncomeForm: Form[BigDecimal] =
    Form(
      mapping(
        "estimatedIncome" -> of(MoneyUtils.amountInPoundsFormatter(_ < 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

  def personalAllowanceForm(disposalDate: DisposalDate): Form[BigDecimal] =
    Form(
      mapping(
        "personalAllowance" -> of(
          MoneyUtils.amountInPoundsFormatter(
            _ < 0,
            _ > disposalDate.taxYear.maxPersonalAllowance.inPounds(),
            List(MoneyUtils.formatAmountOfMoneyWithPoundSign(disposalDate.taxYear.maxPersonalAllowance.inPounds())),
            List(
              MoneyUtils.formatAmountOfMoneyWithPoundSign(disposalDate.taxYear.maxPersonalAllowance.inPounds()),
              disposalDate.taxYear.startDateInclusive.getYear.toString,
              disposalDate.taxYear.endDateExclusive.getYear.toString
            )
          )
        )
      )(identity)(Some(_))
    )

  val hasEstimatedDetailsForm: Form[Boolean] =
    Form(
      mapping(
        "hasEstimatedDetails" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

  val taxDueForm: Form[BigDecimal] =
    Form(
      mapping(
        "taxDue" -> of(MoneyUtils.amountInPoundsFormatter(_ < 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

  val mandatoryEvidenceForm: Form[FileUpload] =
    Form(
      mapping(
        "mandatoryEvidence" -> nonEmptyText,
        "reference"         -> nonEmptyText
      )(FileUpload.apply)(FileUpload.unapply)
    )

  val taxableGainOrLossForm: Form[BigDecimal] = {
    val (outerId, gainId, lossId) = ("taxableGainOrLoss", "taxableGain", "netLoss")

    def innerOption(id: String): InnerOption[BigDecimal] =
      InnerOption { data =>
        FormUtils
          .readValue(id, data, identity)
          .flatMap(
            validateAmountOfMoney(
              id,
              _ <= 0,
              _ > MoneyUtils.maxAmountOfPounds
            )(_)
          )
          .leftMap(Seq(_))
      }

    val formatter = ConditionalRadioUtils.formatter("taxableGainOrLoss")(
      List(
        Left(innerOption(gainId)),
        Left(innerOption(lossId).map(_ * -1)),
        Right(BigDecimal(0))
      )
    ) { d =>
      if (d > 0)
        Map(outerId -> "0", gainId -> MoneyUtils.formatAmountOfMoneyWithoutPoundSign(d))
      else if (d < 0)
        Map(outerId -> "1", lossId -> MoneyUtils.formatAmountOfMoneyWithoutPoundSign((d * -1)))
      else
        Map(outerId -> "2")
    }

    Form(
      mapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

  val nonCalculatedTaxDueForm: Form[BigDecimal] =
    Form(
      mapping(
        "nonCalculatedTaxDue" -> of(MoneyUtils.amountInPoundsFormatter(_ < 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

}
