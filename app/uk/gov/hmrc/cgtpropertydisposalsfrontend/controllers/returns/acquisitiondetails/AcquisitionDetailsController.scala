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
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.http.Writeable
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, RebasingCutoffDates, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.AcquisitionDetailsController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{acquisitiondetails => pages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class AcquisitionDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  acquisitionMethodPage: pages.acquisition_method,
  acquisitionDatePage: pages.acquisition_date,
  acquisitionPricePage: pages.acquisition_price,
  improvementCostsPage: pages.improvement_costs,
  acquisitionFeesPage: pages.acquisition_fees,
  rebasedAcquisitionPricePage: pages.rebased_acquisition_price,
  checkYouAnswersPage: pages.check_your_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withFillingOutReturnAndAcquisitionDetailsAnswers(
    request: RequestWithSessionData[_]
  )(
    f: (
      SessionData,
      FillingOutReturn,
      AcquisitionDetailsAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r: FillingOutReturn)) =>
        r.draftReturn.acquisitionDetailsAnswers.fold[Future[Result]](
          f(s, r, IncompleteAcquisitionDetailsAnswers.empty)
        )(f(s, r, _))
      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def withAssetTypeAndResidentialStatus(
    fillingOutReturn: FillingOutReturn,
    answers: AcquisitionDetailsAnswers
  )(f: (AssetType, Boolean) => Future[Result]): Future[Result] =
    fillingOutReturn.draftReturn.triageAnswers.fold(
      i => i.assetType       -> i.wasAUKResident,
      c => Some(c.assetType) -> Some(c.wasAUKResident)
    ) match {
      case (Some(a), Some(w)) => f(a, w)
      case _                  => Redirect(controllers.returns.routes.TaskListController.taskList())
    }

  private def withDisposalDate(
    fillingOutReturn: FillingOutReturn
  )(f: DisposalDate => Future[Result]): Future[Result] =
    fillingOutReturn.draftReturn.triageAnswers
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
        Redirect(routes.AcquisitionDetailsController.acquisitionDate())
      )(f)

  private def commonDisplayBehaviour[A, P: Writeable, R](
    currentAnswers: AcquisitionDetailsAnswers
  )(form: AcquisitionDetailsAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: AcquisitionDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => routes.AcquisitionDetailsController.checkYourAnswers()
      )

      Ok(page(form(currentAnswers), backLink))
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  private def commonSubmitBehaviour[A, P: Writeable, R](
    currentFillingOutReturn: FillingOutReturn,
    currentAnswers: AcquisitionDetailsAnswers
  )(form: Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: AcquisitionDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(
    updateAnswers: (A, AcquisitionDetailsAnswers) => AcquisitionDetailsAnswers
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      lazy val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => routes.AcquisitionDetailsController.checkYourAnswers()
      )
      form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink)), { value =>
            val newAnswers     = updateAnswers(value, currentAnswers)
            val newDraftReturn = currentFillingOutReturn.draftReturn.copy(acquisitionDetailsAnswers = Some(newAnswers))

            val result = for {
              _ <- if (newDraftReturn === currentFillingOutReturn.draftReturn) EitherT.pure(())
                  else returnsService.storeDraftReturn(newDraftReturn)
              _ <- EitherT(
                    updateSession(sessionStore, request)(
                      _.copy(journeyStatus = Some(currentFillingOutReturn.copy(draftReturn = newDraftReturn)))
                    )
                  )
            } yield ()

            result.fold({ e =>
              logger.warn("Could not update draft return", e)
              errorHandler.errorResult()
            }, _ => Redirect(routes.AcquisitionDetailsController.checkYourAnswers()))
          }
        )
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  def acquisitionMethod(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
      case (_, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.acquisitionMethod.fold(acquisitionMethodForm)(acquisitionMethodForm.fill),
            c => acquisitionMethodForm.fill(c.acquisitionMethod)
          )
        )(
          page = acquisitionMethodPage(_, _)
        )(
          requiredPreviousAnswer = _ => Some(()),
          controllers.returns.routes.TaskListController.taskList()
        )
    }
  }

  def acquisitionMethodSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        commonSubmitBehaviour(
          fillingOutReturn,
          answers
        )(
          acquisitionMethodForm
        )(acquisitionMethodPage(_, _))(
          requiredPreviousAnswer = _ => Some(()),
          controllers.returns.routes.TaskListController.taskList()
        )(
          updateAnswers = {
            case (m, answers) =>
              answers.fold(
                _.copy(acquisitionMethod = Some(m)),
                _.copy(acquisitionMethod = m)
              )
          }
        )
    }
  }

  def acquisitionDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withDisposalDate(fillingOutReturn) { disposalDate =>
          val form = acquisitionDateForm(disposalDate.value)

          commonDisplayBehaviour(answers)(
            form = _.fold(
              _.acquisitionDate.fold(form)(form.fill),
              c => form.fill(c.acquisitionDate)
            )
          )(
            page = acquisitionDatePage(_, _)
          )(
            requiredPreviousAnswer = _.fold(
              _.acquisitionMethod,
              c => Some(c.acquisitionMethod)
            ),
            redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.acquisitionMethod()
          )
        }
    }
  }

  def acquisitionDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withDisposalDate(fillingOutReturn) { disposalDate =>
          withAssetTypeAndResidentialStatus(fillingOutReturn, answers) {
            case (assetType, wasUkResident) =>
              commonSubmitBehaviour(
                fillingOutReturn,
                answers
              )(
                form = acquisitionDateForm(disposalDate.value)
              )(acquisitionDatePage(_, _))(
                requiredPreviousAnswer = _.fold(
                  _.acquisitionMethod,
                  c => Some(c.acquisitionMethod)
                ),
                redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.acquisitionMethod()
              )(
                updateAnswers = {
                  case (d, answers) =>
                    val didMeetRebasingCriteria =
                      answers
                        .fold(_.acquisitionDate, c => Some(c.acquisitionDate))
                        .exists(date => rebasingCutOffDate(date, assetType, wasUkResident).isDefined)

                    val nowMeetsRebasingCriteria =
                      rebasingCutOffDate(d, assetType, wasUkResident).isDefined

                    if (didMeetRebasingCriteria && !nowMeetsRebasingCriteria) {
                      answers.fold(
                        _.copy(acquisitionDate = Some(d), rebasedAcquisitionPrice = None),
                        _.copy(acquisitionDate = d, rebasedAcquisitionPrice       = None)
                      )
                    } else if (!didMeetRebasingCriteria && nowMeetsRebasingCriteria) {
                      answers.fold(
                        _.copy(acquisitionDate = Some(d), acquisitionPrice = None, rebasedAcquisitionPrice = None),
                        c =>
                          IncompleteAcquisitionDetailsAnswers(
                            Some(c.acquisitionMethod),
                            Some(d),
                            None,
                            None,
                            Some(c.improvementCosts),
                            Some(c.acquisitionFees)
                          )
                      )
                    } else {
                      answers.fold(
                        _.copy(acquisitionDate = Some(d)),
                        _.copy(acquisitionDate = d)
                      )
                    }
                }
              )
          }
        }
    }
  }

  def acquisitionPrice(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
      case (_, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.acquisitionPrice.fold(acquisitionPriceForm)(p => acquisitionPriceForm.fill(p.inPounds)),
            c => acquisitionPriceForm.fill(c.acquisitionPrice.inPounds())
          )
        )(
          page = acquisitionPricePage(_, _)
        )(
          requiredPreviousAnswer = _.fold(
            _.acquisitionDate,
            c => Some(c.acquisitionDate)
          ),
          redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.acquisitionDate()
        )
    }
  }

  def acquisitionPriceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        commonSubmitBehaviour(
          fillingOutReturn,
          answers
        )(
          acquisitionPriceForm
        )(page = acquisitionPricePage(_, _))(
          requiredPreviousAnswer = _.fold(
            _.acquisitionDate,
            c => Some(c.acquisitionDate)
          ),
          redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.acquisitionDate()
        )(
          updateAnswers = {
            case (p, answers) =>
              answers.fold(
                _.copy(acquisitionPrice = Some(AmountInPence.fromPounds(p))),
                _.copy(acquisitionPrice = AmountInPence.fromPounds(p))
              )
          }
        )
    }
  }

  def rebasedAcquisitionPrice(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withAssetTypeAndResidentialStatus(fillingOutReturn, answers) {
          case (assetType, wasUkResident) =>
            withAcquisitionDate(answers) { acquisitionDate =>
              rebasingCutOffDate(acquisitionDate, assetType, wasUkResident).fold[Future[Result]](
                Redirect(routes.AcquisitionDetailsController.checkYourAnswers())
              ) { rebaseDate =>
                commonDisplayBehaviour(answers)(
                  form = _.fold(
                    _.rebasedAcquisitionPrice.fold(rebasedAcquisitionPriceForm)(a =>
                      rebasedAcquisitionPriceForm.fill(a.inPounds())
                    ),
                    _.rebasedAcquisitionPrice.fold(rebasedAcquisitionPriceForm)(a =>
                      rebasedAcquisitionPriceForm.fill(a.inPounds())
                    )
                  )
                )(
                  page = rebasedAcquisitionPricePage(_, _, rebaseDate)
                )(
                  requiredPreviousAnswer = _.fold(
                    _.acquisitionPrice,
                    c => Some(c.acquisitionPrice)
                  ),
                  redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.acquisitionPrice()
                )
              }
            }
        }
    }
  }

  def rebasedAcquisitionPriceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
        case (_, fillingOutReturn, answers) =>
          withAssetTypeAndResidentialStatus(fillingOutReturn, answers) {
            case (assetType, wasUkResident) =>
              withAcquisitionDate(answers) { acquisitionDate =>
                rebasingCutOffDate(acquisitionDate, assetType, wasUkResident).fold[Future[Result]](
                  Redirect(routes.AcquisitionDetailsController.checkYourAnswers())
                ) { rebaseDate =>
                  commonSubmitBehaviour(fillingOutReturn, answers)(
                    form = rebasedAcquisitionPriceForm
                  )(
                    page = {
                      case (form, backLink) =>
                        val p = form.copy(errors =
                          form.errors.map(_.copy(args = Seq(LocalDateUtils.govDisplayFormat(rebaseDate))))
                        )
                        rebasedAcquisitionPricePage(p, backLink, rebaseDate)
                    }
                  )(
                    requiredPreviousAnswer = _.fold(
                      _.acquisitionPrice,
                      c => Some(c.acquisitionPrice)
                    ),
                    redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.acquisitionPrice()
                  )(
                    updateAnswers = {
                      case (p, answers) =>
                        answers.fold(
                          _.copy(rebasedAcquisitionPrice = Some(AmountInPence.fromPounds(p))),
                          _.copy(rebasedAcquisitionPrice = Some(AmountInPence.fromPounds(p)))
                        )
                    }
                  )
                }
              }
          }
      }
  }

  def improvementCosts(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withAssetTypeAndResidentialStatus(fillingOutReturn, answers) {
          case (assetType, wasUkResident) =>
            withAcquisitionDate(answers) { acquisitionDate =>
              val rebaseDate = rebasingCutOffDate(acquisitionDate, assetType, wasUkResident)
              commonDisplayBehaviour(answers)(
                form = _.fold(
                  _.improvementCosts.fold(improvementCostsForm)(p => improvementCostsForm.fill(p.inPounds())),
                  c => improvementCostsForm.fill(c.improvementCosts.inPounds())
                )
              )(
                page = improvementCostsPage(_, _)
              )(
                requiredPreviousAnswer = { a =>
                  if (rebaseDate.isDefined)
                    a.fold(_.rebasedAcquisitionPrice, _.rebasedAcquisitionPrice)
                  else
                    a.fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))
                },
                redirectToIfNoRequiredPreviousAnswer = {
                  if (rebaseDate.isDefined)
                    routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
                  else
                    routes.AcquisitionDetailsController.acquisitionPrice()
                }
              )
            }
        }
    }
  }

  def improvementCostsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withAssetTypeAndResidentialStatus(fillingOutReturn, answers) {
          case (assetType, wasUkResident) =>
            withAcquisitionDate(answers) { acquisitionDate =>
              val rebaseDate = rebasingCutOffDate(acquisitionDate, assetType, wasUkResident)

              commonSubmitBehaviour(
                fillingOutReturn,
                answers
              )(
                improvementCostsForm
              )(page = improvementCostsPage(_, _))(
                requiredPreviousAnswer = { answers =>
                  if (rebaseDate.isDefined)
                    answers.fold(_.rebasedAcquisitionPrice, _.rebasedAcquisitionPrice)
                  else
                    answers.fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))
                },
                redirectToIfNoRequiredPreviousAnswer = {
                  if (rebaseDate.isDefined)
                    routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
                  else
                    routes.AcquisitionDetailsController.acquisitionPrice()
                }
              )(
                updateAnswers = {
                  case (p, answers) =>
                    answers.fold(
                      _.copy(improvementCosts = Some(AmountInPence.fromPounds(p))),
                      _.copy(improvementCosts = AmountInPence.fromPounds(p))
                    )
                }
              )
            }
        }
    }
  }

  def acquisitionFees(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
      case (_, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.acquisitionFees.fold(acquisitionFeesForm)(p => acquisitionFeesForm.fill(p.inPounds)),
            c => acquisitionFeesForm.fill(c.acquisitionFees.inPounds())
          )
        )(
          page = acquisitionFeesPage(_, _)
        )(
          requiredPreviousAnswer = _.fold(
            _.improvementCosts,
            c => Some(c.improvementCosts)
          ),
          redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.improvementCosts()
        )
    }
  }

  def acquisitionFeesSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        commonSubmitBehaviour(
          fillingOutReturn,
          answers
        )(
          acquisitionFeesForm
        )(page = acquisitionFeesPage(_, _))(
          requiredPreviousAnswer = _.fold(
            _.improvementCosts,
            c => Some(c.improvementCosts)
          ),
          redirectToIfNoRequiredPreviousAnswer = routes.AcquisitionDetailsController.improvementCosts()
        )(
          updateAnswers = {
            case (p, answers) =>
              answers.fold(
                _.copy(acquisitionFees = Some(AmountInPence.fromPounds(p))),
                _.copy(acquisitionFees = AmountInPence.fromPounds(p))
              )
          }
        )
    }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndAcquisitionDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withAssetTypeAndResidentialStatus(fillingOutReturn, answers) {
          case (assetType, wasAUkResident) =>
            answers match {
              case c: CompleteAcquisitionDetailsAnswers =>
                Ok(checkYouAnswersPage(c))

              case IncompleteAcquisitionDetailsAnswers(None, _, _, _, _, _) =>
                Redirect(routes.AcquisitionDetailsController.acquisitionMethod())

              case IncompleteAcquisitionDetailsAnswers(_, None, _, _, _, _) =>
                Redirect(routes.AcquisitionDetailsController.acquisitionDate())

              case IncompleteAcquisitionDetailsAnswers(_, _, None, _, _, _) =>
                Redirect(routes.AcquisitionDetailsController.acquisitionPrice())

              case IncompleteAcquisitionDetailsAnswers(_, Some(date), _, None, _, _)
                  if (rebasingCutOffDate(date, assetType, wasAUkResident).isDefined) =>
                Redirect(routes.AcquisitionDetailsController.rebasedAcquisitionPrice())

              case IncompleteAcquisitionDetailsAnswers(_, _, _, _, None, _) =>
                Redirect(routes.AcquisitionDetailsController.improvementCosts())

              case IncompleteAcquisitionDetailsAnswers(_, _, _, _, _, None) =>
                Redirect(routes.AcquisitionDetailsController.acquisitionFees())

              case IncompleteAcquisitionDetailsAnswers(Some(m), Some(d), Some(p), r, Some(i), Some(f)) =>
                val completeAnswers = CompleteAcquisitionDetailsAnswers(m, d, p, r, i, f)
                val newDraftReturn =
                  fillingOutReturn.draftReturn.copy(acquisitionDetailsAnswers = Some(completeAnswers))

                val result = for {
                  _ <- returnsService.storeDraftReturn(newDraftReturn)
                  _ <- EitherT(
                        updateSession(sessionStore, request)(
                          _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = newDraftReturn)))
                        )
                      )
                } yield ()

                result.fold({ e =>
                  logger.warn("Could not update session", e)
                  errorHandler.errorResult()
                }, _ => Ok(checkYouAnswersPage(completeAnswers)))
            }
        }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Redirect(controllers.returns.routes.TaskListController.taskList())
  }

  private def rebasingCutOffDate(
    acquisitionDate: AcquisitionDate,
    assetType: AssetType,
    wasUkResident: Boolean
  ): Option[LocalDate] = {
    val cutoffDate =
      if (wasUkResident)
        RebasingCutoffDates.ukResidents
      else if (assetType === AssetType.Residential)
        RebasingCutoffDates.nonUkResidentsResidentialProperty
      else RebasingCutoffDates.nonUkResidentsNonResidentialProperty

    Some(cutoffDate).filter(acquisitionDate.value.isBefore)
  }

}

object AcquisitionDetailsController {

  val acquisitionMethodForm: Form[AcquisitionMethod] = {
    val formatter: Formatter[AcquisitionMethod] = {
      val (acquisitionMethodKey, otherAcquisitionMethodKey) = "acquisitionMethod" -> "otherAcquisitionMethod"
      val otherAcquisitionMethodPredicate                   = "^[a-zA-Z0-9 ]{1,35}$".r.pattern.asPredicate()

      def validateOtherAcquisitionMethod(s: String): Either[FormError, AcquisitionMethod] =
        if (s.length > 35) Left(FormError(otherAcquisitionMethodKey, "error.tooLong"))
        else if (!otherAcquisitionMethodPredicate.test(s)) Left(FormError(otherAcquisitionMethodKey, "error.invalid"))
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
        case AcquisitionMethod.Bought    => Map(acquisitionMethodKey -> "0")
        case AcquisitionMethod.Inherited => Map(acquisitionMethodKey -> "1")
        case AcquisitionMethod.Gifted    => Map(acquisitionMethodKey -> "2")
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

  def acquisitionDateForm(today: LocalDate): Form[AcquisitionDate] = Form(
    mapping(
      "" -> of(
        LocalDateUtils.dateFormatter(
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

  val acquisitionPriceForm: Form[Double] =
    Form(
      mapping(
        "acquisitionPrice" -> of(MoneyUtils.amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

  val rebasedAcquisitionPriceForm: Form[Double] =
    MoneyUtils.amountInPoundsYesNoForm("rebaseAcquisitionPrice", "rebaseAcquisitionPriceValue")

  val improvementCostsForm: Form[Double] =
    MoneyUtils.amountInPoundsYesNoForm("improvementCosts", "improvementCostsValue")

  val acquisitionFeesForm: Form[Double] =
    MoneyUtils.amountInPoundsYesNoForm("acquisitionFees", "acquisitionFeesValue")

}
