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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
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
    hasRequiredPreviousAnswer: ReliefDetailsAnswers => Boolean,
    redirectToIfNoRequiredPreviousAnswer: ReliefDetailsAnswers => Call
  ): Future[Result] =
    if (hasRequiredPreviousAnswer(currentAnswers)) {
      val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer(currentAnswers),
        _ => routes.ReliefDetailsController.checkYourAnswers()
      )
      Ok(page(form(currentAnswers), backLink))
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer(currentAnswers))
    }

  private def commonSubmitBehaviour[A, P: Writeable, R](
    currentFillingOutReturn: FillingOutReturn,
    currentDraftReturn: SingleDisposalDraftReturn,
    currentAnswers: ReliefDetailsAnswers
  )(form: Form[A])(
    page: (Form[A], Call) => P
  )(
    hasRequiredPreviousAnswer: ReliefDetailsAnswers => Boolean,
    redirectToIfNoRequiredPreviousAnswer: ReliefDetailsAnswers => Call
  )(
    updateDraftReturn: (A, SingleDisposalDraftReturn) => SingleDisposalDraftReturn
  )(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    if (hasRequiredPreviousAnswer(currentAnswers)) {
      lazy val backLink = currentAnswers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer(currentAnswers),
        _ => controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
      )
      form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink)), { value =>
            val newDraftReturn = updateDraftReturn(value, currentDraftReturn)

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
      Redirect(redirectToIfNoRequiredPreviousAnswer(currentAnswers))
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
          hasRequiredPreviousAnswer = _ => true,
          _ => controllers.returns.routes.TaskListController.taskList()
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
            hasRequiredPreviousAnswer            = _ => true,
            redirectToIfNoRequiredPreviousAnswer = _ => controllers.returns.routes.TaskListController.taskList()
          )(
            updateDraftReturn = {
              case (p, draftReturn) =>
                fromMaybeReliefsToPrivateResidentsReliefAmount(draftReturn) match {
                  case Some(value) if (value === AmountInPence.fromPounds(p)) => {
                    draftReturn
                  }
                  case _ => {
                    draftReturn.copy(
                      reliefDetailsAnswers = Some(
                        answers.fold(
                          _.copy(privateResidentsRelief = Some(AmountInPence.fromPounds(p)), lettingsRelief = None),
                          c =>
                            IncompleteReliefDetailsAnswers(
                              Some(AmountInPence.fromPounds(p)),
                              None,
                              c.otherReliefs
                            )
                        )
                      )
                    )
                  }
                }
            }
          )
      }
  }

  private def fromMaybeReliefsToPrivateResidentsReliefAmount(
    draftReturn: SingleDisposalDraftReturn
  ): Option[AmountInPence] = {
    val privateResidentsRelief = draftReturn.reliefDetailsAnswers
      .flatMap(answers =>
        answers.fold[Option[AmountInPence]](
          incomplete => incomplete.privateResidentsRelief,
          p => Some(p.privateResidentsRelief)
        )
      )
    privateResidentsRelief
  }

  def lettingsRelief(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) { (_, fillingOutReturn, draftReturn, reliefDetailsAnswers) =>
      withTaxYear(fillingOutReturn, draftReturn, reliefDetailsAnswers) {
        lettingsDisplayBehaviour
      }
    }
  }

  def lettingsReliefSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) { (_, fillingOutReturn, draftReturn, reliefDetailsAnswers) =>
      withTaxYear(fillingOutReturn, draftReturn, reliefDetailsAnswers) {
        lettingsSubmitBehaviour
      }
    }
  }

  def lettingsDisplayBehaviour(
    fillingOutReturn: FillingOutReturn,
    draftReturn: SingleDisposalDraftReturn,
    answers: ReliefDetailsAnswers,
    taxYear: TaxYear
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    commonDisplayBehaviour(answers)(
      form = _.fold(
        _.lettingsRelief.fold(
          lettingsReliefForm(answers, taxYear.maxLettingsReliefAmount)
        )(a => lettingsReliefForm(answers, taxYear.maxLettingsReliefAmount).fill(a.inPounds())),
        c => lettingsReliefForm(answers, taxYear.maxLettingsReliefAmount).fill(c.lettingsRelief.inPounds())
      )
    )(
      page = lettingsReliefPage(_, _)
    )(
      hasRequiredPreviousAnswer            = hasRequiredPreviousAnswerForLettingsReliefs,
      redirectToIfNoRequiredPreviousAnswer = getRedirectForLettingsReliefs
    )

  def withTaxYear(
    fillingOutReturn: FillingOutReturn,
    draftReturn: SingleDisposalDraftReturn,
    reliefDetailsAnswers: ReliefDetailsAnswers
  )(
    f: (FillingOutReturn, SingleDisposalDraftReturn, ReliefDetailsAnswers, TaxYear) => Future[Result]
  ): Future[Result] = {
    val fallBackRoute: Future[Result] = Redirect(controllers.returns.routes.TaskListController.taskList())
    draftReturn.triageAnswers.fold[Future[Result]](
      incomplete =>
        incomplete.disposalDate.fold(fallBackRoute)(disposalDate =>
          f(fillingOutReturn, draftReturn, reliefDetailsAnswers, disposalDate.taxYear)
        ),
      complete => f(fillingOutReturn, draftReturn, reliefDetailsAnswers, complete.disposalDate.taxYear)
    )
  }

  private def lettingsSubmitBehaviour(
    fillingOutReturn: FillingOutReturn,
    draftReturn: SingleDisposalDraftReturn,
    answers: ReliefDetailsAnswers,
    taxYear: TaxYear
  )(implicit request: RequestWithSessionData[_]) =
    commonSubmitBehaviour(fillingOutReturn, draftReturn, answers)(
      form = lettingsReliefForm(answers, taxYear.maxLettingsReliefAmount)
    )(page = lettingsReliefPage(_, _))(
      hasRequiredPreviousAnswer            = hasRequiredPreviousAnswerForLettingsReliefs,
      redirectToIfNoRequiredPreviousAnswer = getRedirectForLettingsReliefs
    )(
      updateDraftReturn = {
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
          hasRequiredPreviousAnswer            = hasRequiredPreviousAnswerForOtherReliefs,
          redirectToIfNoRequiredPreviousAnswer = getRedirectForOtherReliefs
        )
    }
  }

  def otherReliefsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, fillingOutReturn, draftReturn, answers) =>
        commonSubmitBehaviour(fillingOutReturn, draftReturn, answers)(
          form = otherReliefsForm
        )(page = otherReliefsPage(_, _))(
          hasRequiredPreviousAnswer            = hasRequiredPreviousAnswerForOtherReliefs,
          redirectToIfNoRequiredPreviousAnswer = getRedirectForOtherReliefs
        )(
          updateDraftReturn = {
            case (maybeOtherReliefs, draftReturn) =>
              val otherReliefs = maybeOtherReliefs
                .bimap({
                  case (name, amount) =>
                    OtherReliefsOption.OtherReliefs(name, AmountInPence.fromPounds(amount))
                }, _ => OtherReliefsOption.NoOtherReliefs)
                .merge

              val updatedReliefDetailsAnswers =
                answers.fold(
                  _.copy(otherReliefs = Some(otherReliefs)),
                  _.copy(otherReliefs = Some(otherReliefs))
                )

              if (answers === updatedReliefDetailsAnswers) {
                draftReturn
              } else {
                draftReturn.copy(
                  reliefDetailsAnswers       = Some(updatedReliefDetailsAnswers),
                  yearToDateLiabilityAnswers = None
                )
              }
          }
        )
    }
  }

  private def hasRequiredPreviousAnswerForOtherReliefs(answers: ReliefDetailsAnswers): Boolean =
    answers match {
      case IncompleteReliefDetailsAnswers(None, _, _)                                                  => false
      case IncompleteReliefDetailsAnswers(Some(AmountInPence(0)), _, _)                                => true
      case IncompleteReliefDetailsAnswers(Some(AmountInPence(_)), lettingsRelief, _)                   => lettingsRelief.isDefined
      case CompleteReliefDetailsAnswers(AmountInPence(0), lettingsRelief, _) if !lettingsRelief.isZero => false
      case CompleteReliefDetailsAnswers(AmountInPence(_), _, _)                                        => true
    }

  private def getRedirectForOtherReliefs(answers: ReliefDetailsAnswers): Call =
    answers match {
      case IncompleteReliefDetailsAnswers(None, _, _) => routes.ReliefDetailsController.privateResidentsRelief()
      case IncompleteReliefDetailsAnswers(Some(AmountInPence(0)), _, _) =>
        routes.ReliefDetailsController.privateResidentsRelief()
      case IncompleteReliefDetailsAnswers(Some(AmountInPence(_)), lettingsRelief, _) =>
        routes.ReliefDetailsController.lettingsRelief()
      case CompleteReliefDetailsAnswers(AmountInPence(0), lettingsRelief, _) if !lettingsRelief.isZero =>
        routes.ReliefDetailsController.privateResidentsRelief()
      case CompleteReliefDetailsAnswers(AmountInPence(_), _, _) => routes.ReliefDetailsController.lettingsRelief()
    }

  private def hasRequiredPreviousAnswerForLettingsReliefs(answers: ReliefDetailsAnswers): Boolean =
    answers match {
      case IncompleteReliefDetailsAnswers(None, _, _)                                        => false
      case IncompleteReliefDetailsAnswers(Some(AmountInPence(0)), None, _)                   => false
      case IncompleteReliefDetailsAnswers(Some(AmountInPence(0)), Some(AmountInPence(0)), _) => false
      case IncompleteReliefDetailsAnswers(Some(AmountInPence(0)), Some(_), _)                => false
      case IncompleteReliefDetailsAnswers(Some(AmountInPence(_)), _, _)                      => true
      case CompleteReliefDetailsAnswers(AmountInPence(0), AmountInPence(0), _)               => false
      case CompleteReliefDetailsAnswers(_, _, _)                                             => true
    }

  private def getRedirectForLettingsReliefs(answers: ReliefDetailsAnswers): Call =
    answers match {
      case IncompleteReliefDetailsAnswers(None, _, _) => routes.ReliefDetailsController.privateResidentsRelief()
      case IncompleteReliefDetailsAnswers(Some(AmountInPence(0)), None, _) =>
        routes.ReliefDetailsController.otherReliefs()
      case IncompleteReliefDetailsAnswers(Some(AmountInPence(0)), Some(AmountInPence(0)), _) =>
        routes.ReliefDetailsController.otherReliefs()
      case IncompleteReliefDetailsAnswers(Some(AmountInPence(0)), Some(_), _) =>
        routes.ReliefDetailsController.privateResidentsRelief()
      case IncompleteReliefDetailsAnswers(Some(AmountInPence(value)), _, _) =>
        routes.ReliefDetailsController.privateResidentsRelief()
      case CompleteReliefDetailsAnswers(AmountInPence(0), AmountInPence(0), _) =>
        routes.ReliefDetailsController.otherReliefs()
      case CompleteReliefDetailsAnswers(_, _, _) => routes.ReliefDetailsController.privateResidentsRelief()
    }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndReliefDetailsAnswers(request) {
      case (_, fillingOutReturn, draftReturn, answers) =>
        answers match {
          case c: CompleteReliefDetailsAnswers =>
            Ok(checkYouAnswersPage(c))

          case IncompleteReliefDetailsAnswers(None, _, _) =>
            Redirect(routes.ReliefDetailsController.privateResidentsRelief())

          case IncompleteReliefDetailsAnswers(Some(AmountInPence(0)), None, None) =>
            Redirect(routes.ReliefDetailsController.otherReliefs())

          case IncompleteReliefDetailsAnswers(_, None, None) =>
            Redirect(routes.ReliefDetailsController.lettingsRelief())

          case IncompleteReliefDetailsAnswers(_, _, None) =>
            Redirect(routes.ReliefDetailsController.otherReliefs())

          case IncompleteReliefDetailsAnswers(Some(prr), None, or) =>
            completeRelief(fillingOutReturn, draftReturn, prr, AmountInPence(0), or)

          case IncompleteReliefDetailsAnswers(Some(prr), Some(lr), or) =>
            completeRelief(fillingOutReturn, draftReturn, prr, lr, or)
        }
    }

  }

  private def completeRelief(
    fillingOutReturn: FillingOutReturn,
    draftReturn: SingleDisposalDraftReturn,
    prr: AmountInPence,
    lr: AmountInPence,
    or: Option[OtherReliefsOption]
  )(implicit request: RequestWithSessionData[_]): Future[Result] = {
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

  def innerOption(privateResidencyRelief: AmountInPence, lettingsReliefLimit: AmountInPence): InnerOption[BigDecimal] =
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

  def lettingsReliefForm(answers: ReliefDetailsAnswers, lettingsReliefLimit: AmountInPence): Form[BigDecimal] = {
    val reliefAmount =
      answers.fold[AmountInPence](_.privateResidentsRelief.fold(AmountInPence(0L))(identity), _.privateResidentsRelief)
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
