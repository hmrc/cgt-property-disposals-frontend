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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails

import cats.data.{EitherT, NonEmptyList, Validated, ValidatedNel}
import cats.instances.future._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.http.Writeable
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails.ReliefDetailsController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.IncompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{ReliefDetailsAnswers, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{reliefdetails => pages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ReliefDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  privateResidentsReliefPage: pages.private_residents_relief,
  lettingsReliefPage: pages.lettings_relief,
  otherReliefsPage: pages.other_reliefs,
  checkYouAnswersPage: pages.check_your_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withFillingOutReturnAndReliefDetailsAnswers(
    request: RequestWithSessionData[_]
  )(
    f: (
      SessionData,
      FillingOutReturn,
      SingleDisposalDraftReturn,
      ReliefDetailsAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r @ FillingOutReturn(_, _, _, d: SingleDisposalDraftReturn))) =>
        d.reliefDetailsAnswers.fold[Future[Result]](
          f(s, r, d, IncompleteReliefDetailsAnswers.empty)
        )(f(s, r, d, _))
      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def commonDisplayBehaviour[A, P: Writeable, R](
    currentAnswers: ReliefDetailsAnswers
  )(form: ReliefDetailsAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: ReliefDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => routes.ReliefDetailsController.checkYourAnswers()
      )
      Ok(page(form(currentAnswers), backLink))
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  private def commonSubmitBehaviour[A, P: Writeable, R](
    currentFillingOutReturn: FillingOutReturn,
    currentDraftReturn: SingleDisposalDraftReturn,
    currentAnswers: ReliefDetailsAnswers
  )(form: Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: ReliefDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(
    updateAnswers: (A, SingleDisposalDraftReturn) => SingleDisposalDraftReturn
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(currentAnswers).isDefined) {
      lazy val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
      )
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
            result.fold({ e =>
              logger.warn("Could not update draft return", e)
              errorHandler.errorResult()
            }, _ => Redirect(routes.ReliefDetailsController.checkYourAnswers()))

          }
        )
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  def privateResidentsRelief(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, _, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.privateResidentsRelief.fold(privateResidentsReliefForm)(a => privateResidentsReliefForm.fill(a.inPounds())
            ),
            c => privateResidentsReliefForm.fill(c.privateResidentsRelief.inPounds())
          )
        )(
          page = privateResidentsReliefPage(_, _)
        )(
          requiredPreviousAnswer = _ => Some(()),
          controllers.returns.routes.TaskListController.taskList()
        )
    }
  }

  def privateResidentsReliefSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withFillingOutReturnAndReliefDetailsAnswers(request) {
        case (_, fillingOutReturn, draftReturn, answers) =>
          commonSubmitBehaviour(fillingOutReturn, draftReturn, answers)(
            form = privateResidentsReliefForm
          )(
            page = {
              case (form, backLink) =>
                privateResidentsReliefPage(form, backLink)
            }
          )(
            requiredPreviousAnswer               = _ => Some(()),
            redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
          )(
            updateAnswers = {
              case (p, draftReturn) =>
                draftReturn.copy(
                  reliefDetailsAnswers = Some(
                    answers.fold(
                      _.copy(privateResidentsRelief = Some(AmountInPence.fromPounds(p))),
                      _.copy(privateResidentsRelief = AmountInPence.fromPounds(p))
                    )
                  )
                )
            }
          )
      }
  }

  def lettingsRelief(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, _, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.lettingsRelief.fold(lettingsReliefForm)(a => lettingsReliefForm.fill(a.inPounds())),
            c => lettingsReliefForm.fill(c.lettingsRelief.inPounds())
          )
        )(
          page = lettingsReliefPage(_, _)
        )(
          requiredPreviousAnswer = _.fold(
            _.privateResidentsRelief,
            c => Some(c.privateResidentsRelief)
          ),
          routes.ReliefDetailsController.privateResidentsRelief()
        )
    }
  }

  def lettingsReliefSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, fillingOutReturn, draftReturn, answers) =>
        commonSubmitBehaviour(fillingOutReturn, draftReturn, answers)(
          form = lettingsReliefForm
        )(page = lettingsReliefPage(_, _))(
          requiredPreviousAnswer = _.fold(
            _.privateResidentsRelief,
            c => Some(c.privateResidentsRelief)
          ),
          redirectToIfNoRequiredPreviousAnswer = routes.ReliefDetailsController.privateResidentsRelief()
        )(
          updateAnswers = {
            case (p, draftReturn) =>
              draftReturn.copy(
                reliefDetailsAnswers = Some(
                  answers.fold(
                    _.copy(lettingsRelief = Some(AmountInPence.fromPounds(p))),
                    _.copy(lettingsRelief = AmountInPence.fromPounds(p))
                  )
                )
              )
          }
        )
    }
  }

  def otherReliefs(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, _, _, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.otherReliefs.fold(otherReliefsForm)(
              _.fold(
                otherReliefs => otherReliefsForm.fill(Left(otherReliefs.name -> otherReliefs.amount.inPounds)),
                () => otherReliefsForm.fill(Right(()))
              )
            ),
            c =>
              c.otherReliefs.fold(otherReliefsForm)(
                _.fold(
                  otherReliefs => otherReliefsForm.fill(Left(otherReliefs.name -> otherReliefs.amount.inPounds)),
                  () => otherReliefsForm.fill(Right(()))
                )
              )
          )
        )(
          page = otherReliefsPage(_, _)
        )(
          requiredPreviousAnswer = _.fold(
            _.lettingsRelief,
            c => Some(c.lettingsRelief)
          ),
          routes.ReliefDetailsController.lettingsRelief()
        )
    }
  }

  def otherReliefsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, fillingOutReturn, draftReturn, answers) =>
        commonSubmitBehaviour(fillingOutReturn, draftReturn, answers)(
          form = otherReliefsForm
        )(page = otherReliefsPage(_, _))(
          requiredPreviousAnswer = _.fold(
            _.lettingsRelief,
            c => Some(c.lettingsRelief)
          ),
          redirectToIfNoRequiredPreviousAnswer = routes.ReliefDetailsController.lettingsRelief()
        )(
          updateAnswers = {
            case (maybeOtherReliefs, draftReturn) =>
              val otherReliefs = maybeOtherReliefs
                .bimap({
                  case (name, amount) =>
                    OtherReliefsOption.OtherReliefs(name, AmountInPence.fromPounds(amount))
                }, _ => OtherReliefsOption.NoOtherReliefs)
                .merge

              val updatedExemptionAndLossesAnswers =
                changeExemptionsAndLossesAnswersState(
                  draftReturn.exemptionAndLossesAnswers,
                  answers,
                  otherReliefs
                )

              val updatedReliefDetailsAnswers =
                answers.fold(_.copy(otherReliefs = Some(otherReliefs)), _.copy(otherReliefs = Some(otherReliefs)))

              draftReturn.copy(
                reliefDetailsAnswers      = Some(updatedReliefDetailsAnswers),
                exemptionAndLossesAnswers = updatedExemptionAndLossesAnswers
              )
          }
        )
    }
  }

  private def changeExemptionsAndLossesAnswersState(
    existingExemptionAndLossesAnswers: Option[ExemptionAndLossesAnswers],
    existingReliefDetailsAnswers: ReliefDetailsAnswers,
    newOtherReliefsOption: OtherReliefsOption
  ): Option[ExemptionAndLossesAnswers] = {
    def hasEnteredOtherReliefs(option: Option[OtherReliefsOption]): Boolean =
      option match {
        case Some(_: OtherReliefsOption.OtherReliefs) => true
        case _                                        => false
      }

    val (hadEnteredOtherReliefs, hasNowEnteredOtherReliefs) =
      hasEnteredOtherReliefs(existingReliefDetailsAnswers.fold(_.otherReliefs, _.otherReliefs)) ->
        hasEnteredOtherReliefs(Some(newOtherReliefsOption))

    if (hadEnteredOtherReliefs && !hasNowEnteredOtherReliefs)
      existingExemptionAndLossesAnswers.map(
        _.fold(
          _.copy(taxableGainOrLoss = None),
          _.copy(taxableGainOrLoss = None)
        )
      )
    else if (!hadEnteredOtherReliefs && hasNowEnteredOtherReliefs)
      existingExemptionAndLossesAnswers.map(
        _.fold(
          _.copy(taxableGainOrLoss = None),
          complete =>
            IncompleteExemptionAndLossesAnswers(
              Some(complete.inYearLosses),
              Some(complete.previousYearsLosses),
              Some(complete.annualExemptAmount),
              None
            )
        )
      )
    else
      existingExemptionAndLossesAnswers
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, fillingOutReturn, draftReturn, answers) =>
        answers match {
          case c: CompleteReliefDetailsAnswers =>
            Ok(checkYouAnswersPage(c))

          case IncompleteReliefDetailsAnswers(None, _, _) =>
            Redirect(routes.ReliefDetailsController.privateResidentsRelief())

          case IncompleteReliefDetailsAnswers(_, None, _) =>
            Redirect(routes.ReliefDetailsController.lettingsRelief())

          case IncompleteReliefDetailsAnswers(_, _, None) =>
            Redirect(routes.ReliefDetailsController.otherReliefs())

          case IncompleteReliefDetailsAnswers(Some(prr), Some(lr), or) =>
            val completeAnswers = CompleteReliefDetailsAnswers(prr, lr, or)
            val newDraftReturn =
              draftReturn.copy(reliefDetailsAnswers = Some(completeAnswers))

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
            }, _ => Ok(checkYouAnswersPage(completeAnswers)))
        }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case _ =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }
  }

}

object ReliefDetailsController {

  val privateResidentsReliefForm: Form[BigDecimal] =
    MoneyUtils.amountInPoundsYesNoForm("privateResidentsRelief", "privateResidentsReliefValue")

  val lettingsReliefForm: Form[BigDecimal] =
    MoneyUtils.amountInPoundsYesNoForm("lettingsRelief", "lettingsReliefValue")

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val otherReliefsForm: Form[Either[(String, BigDecimal), Unit]] = {
    val formatter: Formatter[Either[(String, BigDecimal), Unit]] = {
      val (otherReliefKey, otherReliefsNameKey, otherReliefsAmountKey) =
        ("otherReliefs", "otherReliefsName", "otherReliefsAmount")

      val otherReliefsNamePredicate = "^[A-Za-z0-9 ]{1,105}$".r.pattern.asPredicate()

      def validateReliefsName(s: String): Either[FormError, String] =
        if (s.length > 105) Left(FormError(otherReliefsNameKey, "error.tooLong"))
        else if (!otherReliefsNamePredicate.test(s)) Left(FormError(otherReliefsNameKey, "error.invalid"))
        else Right(s)

      val innerOption = InnerOption { data =>
        val nameResult: ValidatedNel[FormError, String] =
          Validated
            .fromEither(
              FormUtils
                .readValue(otherReliefsNameKey, data, identity)
                .flatMap(validateReliefsName)
            )
            .leftMap(NonEmptyList.one(_))

        val amountResult: ValidatedNel[FormError, BigDecimal] =
          Validated
            .fromEither(
              FormUtils
                .readValue(otherReliefsAmountKey, data, identity)
                .flatMap(
                  MoneyUtils.validateAmountOfMoney(
                    otherReliefsAmountKey,
                    _ <= 0,
                    _ > MoneyUtils.maxAmountOfPounds
                  )(_)
                )
            )
            .leftMap(NonEmptyList.one(_))

        (nameResult, amountResult).mapN(_ -> _).toEither.leftMap(_.toList)
      }

      ConditionalRadioUtils.formatter[Either[(String, BigDecimal), Unit]](otherReliefKey)(
        List(
          Left(innerOption.map(Left(_))),
          Right(Right(()))
        )
      ) {
        case Left((s, d)) =>
          Map(
            otherReliefKey        -> "0",
            otherReliefsNameKey   -> s,
            otherReliefsAmountKey -> MoneyUtils.formatAmountOfMoneyWithoutPoundSign(d)
          )
        case Right(()) => Map(otherReliefKey -> "1")
      }
    }

    Form(
      mapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

}
