/*
 * Copyright 2022 HM Revenue & Customs
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
import cats.instances.boolean._
import cats.instances.future._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.order._
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.StartingToAmendToFillingOutReturnBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails.ReliefDetailsController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{ReliefDetailsAnswers, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{reliefdetails => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ReliefDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  uuidGenerator: UUIDGenerator,
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
    with SessionUpdates
    with StartingToAmendToFillingOutReturnBehaviour {

  private def withFillingOutReturnAndReliefDetailsAnswers(
    f: (
      SessionData,
      FillingOutReturn,
      DraftSingleDisposalReturn,
      ReliefDetailsAnswers
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((_, s: StartingToAmendReturn)) =>
        convertFromStartingAmendToFillingOutReturn(s, sessionStore, errorHandler, uuidGenerator)

      case Some(
            (s, r @ FillingOutReturn(_, _, _, d: DraftSingleDisposalReturn, _, _))
          ) if d.triageAnswers.isPeriodOfAdmin() =>
        d.reliefDetailsAnswers.fold[Future[Result]](
          f(s, r, d, IncompleteReliefDetailsAnswers.empty)
        )(relief =>
          f(
            s,
            r,
            d,
            relief.fold(
              _.copy(lettingsRelief = Some(AmountInPence.zero)),
              _.copy(lettingsRelief = AmountInPence.zero)
            )
          )
        )

      case Some(
            (s, r @ FillingOutReturn(_, _, _, d: DraftSingleDisposalReturn, _, _))
          ) =>
        d.reliefDetailsAnswers.fold[Future[Result]](
          f(s, r, d, IncompleteReliefDetailsAnswers.empty)
        )(f(s, r, d, _))

      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def commonDisplayBehaviour[A, P : Writeable, R](
    currentAnswers: ReliefDetailsAnswers
  )(form: ReliefDetailsAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    hasRequiredPreviousAnswer: ReliefDetailsAnswers => Boolean,
    redirectToIfNoRequiredPreviousAnswer: ReliefDetailsAnswers => Call
  ): Future[Result] =
    if (hasRequiredPreviousAnswer(currentAnswers)) {
      val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer(currentAnswers),
        _ => routes.ReliefDetailsController.checkYourAnswers()
      )
      Ok(page(form(currentAnswers), backLink))
    } else
      Redirect(redirectToIfNoRequiredPreviousAnswer(currentAnswers))

  private def commonSubmitBehaviour[A, P : Writeable, R](
    currentFillingOutReturn: FillingOutReturn,
    currentDraftReturn: DraftSingleDisposalReturn,
    currentAnswers: ReliefDetailsAnswers
  )(form: Form[A])(
    page: (Form[A], Call) => P
  )(
    hasRequiredPreviousAnswer: ReliefDetailsAnswers => Boolean,
    redirectToIfNoRequiredPreviousAnswer: ReliefDetailsAnswers => Call
  )(
    updateDraftReturn: (
      A,
      DraftSingleDisposalReturn
    ) => DraftSingleDisposalReturn
  )(implicit
    request: RequestWithSessionData[_]
  ): Future[Result] =
    if (hasRequiredPreviousAnswer(currentAnswers)) {
      lazy val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer(currentAnswers),
        _ =>
          controllers.returns.reliefdetails.routes.ReliefDetailsController
            .checkYourAnswers()
      )
      form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink)),
          { value =>
            val newDraftReturn = updateDraftReturn(value, currentDraftReturn)
            val newJourney     =
              currentFillingOutReturn.copy(draftReturn = newDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends

            val result = for {
              _ <- if (newDraftReturn === currentDraftReturn) EitherT.pure(())
                   else
                     returnsService.storeDraftReturn(newJourney)
              _ <- EitherT(
                     updateSession(sessionStore, request)(
                       _.copy(journeyStatus = Some(newJourney))
                     )
                   )
            } yield ()
            result.fold(
              { e =>
                logger.warn("Could not update draft return", e)
                errorHandler.errorResult()
              },
              _ => Redirect(routes.ReliefDetailsController.checkYourAnswers())
            )

          }
        )
    } else
      Redirect(redirectToIfNoRequiredPreviousAnswer(currentAnswers))

  def privateResidentsRelief(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndReliefDetailsAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.privateResidentsRelief.fold(privateResidentsReliefForm)(a =>
              privateResidentsReliefForm.fill(a.inPounds())
            ),
            c =>
              privateResidentsReliefForm.fill(
                c.privateResidentsRelief.inPounds()
              )
          )
        )(
          page = privateResidentsReliefPage(
            _,
            _,
            fillingOutReturn.subscribedDetails.isATrust,
            draftReturn.representativeType(),
            fillingOutReturn.isAmendReturn
          )
        )(
          hasRequiredPreviousAnswer = _ => true,
          _ => controllers.returns.routes.TaskListController.taskList()
        )
      }
    }

  def privateResidentsReliefSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndReliefDetailsAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        commonSubmitBehaviour(fillingOutReturn, draftReturn, answers)(
          form = privateResidentsReliefForm
        )(
          page = { (form, backLink) =>
            privateResidentsReliefPage(
              form,
              backLink,
              fillingOutReturn.subscribedDetails.isATrust,
              draftReturn.representativeType(),
              fillingOutReturn.isAmendReturn
            )
          }
        )(
          hasRequiredPreviousAnswer = _ => true,
          redirectToIfNoRequiredPreviousAnswer = _ => controllers.returns.routes.TaskListController.taskList()
        )(
          updateDraftReturn = { (p, draftReturn) =>
            if (
              answers
                .fold(
                  _.privateResidentsRelief,
                  c => Some(c.privateResidentsRelief)
                )
                .map(_.inPounds())
                .contains(p)
            )
              draftReturn
            else {
              val updatedAnswers =
                if (draftReturn.triageAnswers.isPeriodOfAdmin)
                  answers.fold(
                    _.copy(privateResidentsRelief = Some(AmountInPence.fromPounds(p))),
                    _.copy(privateResidentsRelief = AmountInPence.fromPounds(p))
                  )
                else
                  answers
                    .unset(_.lettingsRelief)
                    .copy(privateResidentsRelief = Some(AmountInPence.fromPounds(p)))

              draftReturn.copy(
                reliefDetailsAnswers = Some(updatedAnswers),
                yearToDateLiabilityAnswers = draftReturn.yearToDateLiabilityAnswers.flatMap(
                  _.unsetAllButIncomeDetails()
                ),
                gainOrLossAfterReliefs = None,
                exemptionAndLossesAnswers =
                  if (fillingOutReturn.isFurtherOrAmendReturn.contains(true)) None
                  else draftReturn.exemptionAndLossesAnswers
              )
            }
          }
        )
      }
    }

  def lettingsRelief(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndReliefDetailsAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        withTaxYear(draftReturn) { taxYear =>
          commonDisplayBehaviour(answers)(
            form = _.fold(
              _.lettingsRelief.fold(
                lettingsReliefForm(answers, taxYear.maxLettingsReliefAmount)
              )(a =>
                lettingsReliefForm(answers, taxYear.maxLettingsReliefAmount)
                  .fill(a.inPounds())
              ),
              c =>
                lettingsReliefForm(answers, taxYear.maxLettingsReliefAmount)
                  .fill(c.lettingsRelief.inPounds())
            )
          )(
            page = lettingsReliefPage(
              _,
              _,
              fillingOutReturn.subscribedDetails.isATrust,
              draftReturn.representativeType(),
              fillingOutReturn.isAmendReturn
            )
          )(
            hasRequiredPreviousAnswer = hasRequiredPreviousAnswerForLettingsReliefs,
            redirectToIfNoRequiredPreviousAnswer = _ => routes.ReliefDetailsController.privateResidentsRelief()
          )
        }
      }
    }

  def lettingsReliefSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndReliefDetailsAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        withTaxYear(draftReturn) { taxYear =>
          commonSubmitBehaviour(fillingOutReturn, draftReturn, answers)(
            form = lettingsReliefForm(answers, taxYear.maxLettingsReliefAmount)
          )(page =
            lettingsReliefPage(
              _,
              _,
              fillingOutReturn.subscribedDetails.isATrust,
              draftReturn.representativeType(),
              fillingOutReturn.isAmendReturn
            )
          )(
            hasRequiredPreviousAnswer = hasRequiredPreviousAnswerForLettingsReliefs,
            redirectToIfNoRequiredPreviousAnswer = _ => routes.ReliefDetailsController.privateResidentsRelief()
          )(
            updateDraftReturn = { (p, draftReturn) =>
              if (
                answers
                  .fold(_.lettingsRelief, c => Some(c.lettingsRelief))
                  .map(_.inPounds())
                  .contains(p)
              )
                draftReturn
              else
                draftReturn.copy(
                  reliefDetailsAnswers = Some(
                    answers.fold(
                      _.copy(lettingsRelief = Some(AmountInPence.fromPounds(p))),
                      _.copy(lettingsRelief = AmountInPence.fromPounds(p))
                    )
                  ),
                  exemptionAndLossesAnswers =
                    if (fillingOutReturn.isFurtherOrAmendReturn.contains(true)) None
                    else draftReturn.exemptionAndLossesAnswers,
                  yearToDateLiabilityAnswers = draftReturn.yearToDateLiabilityAnswers.flatMap(
                    _.unsetAllButIncomeDetails()
                  )
                )
            }
          )
        }
      }
    }

  private def withTaxYear(
    draftReturn: DraftSingleDisposalReturn
  )(f: TaxYear => Future[Result]): Future[Result] =
    draftReturn.triageAnswers.fold(
      _.disposalDate.fold[Future[Result]](
        Redirect(controllers.returns.routes.TaskListController.taskList())
      )(disposalDate => f(disposalDate.taxYear)),
      complete => f(complete.disposalDate.taxYear)
    )

  private def otherReliefsBackLink(answers: SingleDisposalTriageAnswers): Call =
    if (answers.isPeriodOfAdmin())
      routes.ReliefDetailsController.privateResidentsRelief()
    else routes.ReliefDetailsController.lettingsRelief()

  def otherReliefs(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndReliefDetailsAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        commonDisplayBehaviour(answers)(
          form = _.fold(
            _.otherReliefs.fold(otherReliefsForm)(
              _.fold(
                otherReliefs =>
                  otherReliefsForm.fill(
                    Left(otherReliefs.name -> otherReliefs.amount.inPounds)
                  ),
                () => otherReliefsForm.fill(Right(()))
              )
            ),
            c =>
              c.otherReliefs.fold(otherReliefsForm)(
                _.fold(
                  otherReliefs =>
                    otherReliefsForm.fill(
                      Left(otherReliefs.name -> otherReliefs.amount.inPounds)
                    ),
                  () => otherReliefsForm.fill(Right(()))
                )
              )
          )
        )(
          page = otherReliefsPage(
            _,
            _,
            fillingOutReturn.subscribedDetails.isATrust,
            draftReturn.representativeType(),
            fillingOutReturn.isAmendReturn
          )
        )(
          hasRequiredPreviousAnswer = hasRequiredPreviousAnswerForOtherReliefs,
          redirectToIfNoRequiredPreviousAnswer = _ => otherReliefsBackLink(draftReturn.triageAnswers)
        )
      }
    }

  def otherReliefsSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndReliefDetailsAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        commonSubmitBehaviour(fillingOutReturn, draftReturn, answers)(
          form = otherReliefsForm
        )(page =
          otherReliefsPage(
            _,
            _,
            fillingOutReturn.subscribedDetails.isATrust,
            draftReturn.representativeType(),
            fillingOutReturn.isAmendReturn
          )
        )(
          hasRequiredPreviousAnswer = hasRequiredPreviousAnswerForOtherReliefs,
          redirectToIfNoRequiredPreviousAnswer = _ => otherReliefsBackLink(draftReturn.triageAnswers)
        )(
          updateDraftReturn = { (maybeOtherReliefs, draftReturn) =>
            val existingOtherReliefs =
              answers.fold(_.otherReliefs, _.otherReliefs)
            val otherReliefs         = maybeOtherReliefs
              .bimap(
                { case (name, amount) =>
                  OtherReliefsOption
                    .OtherReliefs(name, AmountInPence.fromPounds(amount))
                },
                _ => OtherReliefsOption.NoOtherReliefs
              )
              .merge

            if (existingOtherReliefs.contains(otherReliefs))
              draftReturn
            else {
              val updatedReliefDetailsAnswers =
                answers.fold(
                  _.copy(otherReliefs = Some(otherReliefs)),
                  _.copy(otherReliefs = Some(otherReliefs))
                )

              val (hadSelectedOtherReliefs, hasSelectedOtherReliefs) =
                existingOtherReliefs.exists(
                  selectedOtherReliefs
                ) -> selectedOtherReliefs(otherReliefs)

              if (hadSelectedOtherReliefs =!= hasSelectedOtherReliefs)
                draftReturn.copy(
                  reliefDetailsAnswers = Some(updatedReliefDetailsAnswers),
                  exemptionAndLossesAnswers = None,
                  yearToDateLiabilityAnswers = None,
                  supportingEvidenceAnswers = None
                )
              else
                draftReturn.copy(
                  reliefDetailsAnswers = Some(updatedReliefDetailsAnswers),
                  exemptionAndLossesAnswers =
                    if (fillingOutReturn.isFurtherOrAmendReturn.contains(true)) None
                    else draftReturn.exemptionAndLossesAnswers,
                  yearToDateLiabilityAnswers = draftReturn.yearToDateLiabilityAnswers.flatMap(
                    _.unsetAllButIncomeDetails()
                  )
                )
            }

          }
        )
      }
    }

  private def selectedOtherReliefs(option: OtherReliefsOption): Boolean =
    option match {
      case _: OtherReliefsOption.OtherReliefs => true
      case _                                  => false
    }

  private def hasRequiredPreviousAnswerForOtherReliefs(
    answers: ReliefDetailsAnswers
  ): Boolean = {
    val privateResidentsRelief = answers
      .fold(_.privateResidentsRelief, c => Some(c.privateResidentsRelief))
    val lettingRelief          =
      answers.fold(_.lettingsRelief, c => Some(c.lettingsRelief))

    privateResidentsRelief.exists(_.isZero) || (privateResidentsRelief.exists(
      _.isPositive
    ) && lettingRelief.isDefined)
  }

  private def hasRequiredPreviousAnswerForLettingsReliefs(
    answers: ReliefDetailsAnswers
  ): Boolean = {
    val privateResidentsRelief = answers
      .fold(_.privateResidentsRelief, c => Some(c.privateResidentsRelief))
    privateResidentsRelief.exists(_.isPositive)
  }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndReliefDetailsAnswers { (_, fillingOutReturn, draftReturn, answers) =>
        answers match {
          case c: CompleteReliefDetailsAnswers =>
            Ok(
              checkYouAnswersPage(
                c,
                fillingOutReturn.subscribedDetails.isATrust,
                draftReturn.representativeType()
              )
            )

          case IncompleteReliefDetailsAnswers(None, _, _) =>
            Redirect(routes.ReliefDetailsController.privateResidentsRelief())

          case IncompleteReliefDetailsAnswers(
                Some(AmountInPence(0)),
                None,
                None
              ) =>
            Redirect(routes.ReliefDetailsController.otherReliefs())

          case IncompleteReliefDetailsAnswers(Some(prr), None, _) if prr > AmountInPence.zero =>
            Redirect(routes.ReliefDetailsController.lettingsRelief())

          case IncompleteReliefDetailsAnswers(_, _, None) =>
            Redirect(routes.ReliefDetailsController.otherReliefs())

          case IncompleteReliefDetailsAnswers(Some(prr), None, or) =>
            completeRelief(
              fillingOutReturn,
              draftReturn,
              prr,
              AmountInPence(0),
              or
            )

          case IncompleteReliefDetailsAnswers(Some(prr), Some(lr), or) =>
            completeRelief(fillingOutReturn, draftReturn, prr, lr, or)
        }
      }

    }

  private def completeRelief(
    fillingOutReturn: FillingOutReturn,
    draftReturn: DraftSingleDisposalReturn,
    prr: AmountInPence,
    lr: AmountInPence,
    or: Option[OtherReliefsOption]
  )(implicit request: RequestWithSessionData[_]): Future[Result] = {
    val completeAnswers = CompleteReliefDetailsAnswers(prr, lr, or)
    val newDraftReturn  = draftReturn.copy(reliefDetailsAnswers = Some(completeAnswers))
    val newJourney      = fillingOutReturn.copy(draftReturn = newDraftReturn)

    val result = for {
      _ <- returnsService.storeDraftReturn(newJourney)
      _ <- EitherT(
             updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newJourney)))
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
            fillingOutReturn.subscribedDetails.isATrust,
            draftReturn.representativeType()
          )
        )
    )
  }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndReliefDetailsAnswers { (_, _, _, _) =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
      }
    }

}

object ReliefDetailsController {

  val privateResidentsReliefForm: Form[BigDecimal] =
    MoneyUtils.amountInPoundsYesNoForm(
      "privateResidentsRelief",
      "privateResidentsReliefValue"
    )

  def innerOption(
    privateResidencyRelief: AmountInPence,
    lettingsReliefLimit: AmountInPence
  ): InnerOption[BigDecimal] =
    InnerOption { data =>
      FormUtils
        .readValue("lettingsReliefValue", data, identity)
        .flatMap(
          MoneyUtils.validateAmountOfMoney(
            "lettingsReliefValue",
            _ <= 0,
            _ > MoneyUtils.maxAmountOfPounds
          )(_)
        )
        .flatMap(value =>
          MoneyUtils.validateValueIsLessThan(
            "lettingsReliefValue",
            lettingsReliefLimit,
            "error.amountOverLimit"
          )(value)
        )
        .flatMap { value =>
          MoneyUtils.validateValueIsLessThan(
            "lettingsReliefValue",
            privateResidencyRelief,
            "error.amountOverPrivateResidenceRelief"
          )(value)
        }
        .leftMap(Seq(_))
    }

  def lettingsReliefForm(
    answers: ReliefDetailsAnswers,
    lettingsReliefLimit: AmountInPence
  ): Form[BigDecimal] = {
    val reliefAmount =
      answers.fold[AmountInPence](
        _.privateResidentsRelief.fold(AmountInPence(0L))(identity),
        _.privateResidentsRelief
      )
    MoneyUtils
      .amountInPoundsYesNoForm(
        "lettingsRelief",
        "lettingsReliefValue",
        Some(innerOption(reliefAmount, lettingsReliefLimit))
      )
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val otherReliefsForm: Form[Either[(String, BigDecimal), Unit]] = {
    val formatter: Formatter[Either[(String, BigDecimal), Unit]] = {
      val (otherReliefKey, otherReliefsNameKey, otherReliefsAmountKey) =
        ("otherReliefs", "otherReliefsName", "otherReliefsAmount")

      val otherReliefsNamePredicate =
        "^[A-Za-z0-9 ]{1,105}$".r.pattern.asPredicate()

      def validateReliefsName(s: String): Either[FormError, String] =
        if (s.length > 105)
          Left(FormError(otherReliefsNameKey, "error.tooLong"))
        else if (!otherReliefsNamePredicate.test(s))
          Left(FormError(otherReliefsNameKey, "error.invalid"))
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

      ConditionalRadioUtils
        .formatter[Either[(String, BigDecimal), Unit]](otherReliefKey)(
          List(
            Left(innerOption.map(Left(_))),
            Right(Right(()))
          )
        ) {
          case Left((s, d)) =>
            Map(
              otherReliefKey        -> "0",
              otherReliefsNameKey   -> s,
              otherReliefsAmountKey -> MoneyUtils
                .formatAmountOfMoneyWithoutPoundSign(d)
            )
          case Right(())    => Map(otherReliefKey -> "1")
        }
    }

    Form(
      mapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

}
