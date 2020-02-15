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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails

import cats.Eq
import cats.data.EitherT
import cats.instances.bigDecimal._
import cats.instances.future._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Forms.{of, mapping => formMapping}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.http.Writeable
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails.DisposalDetailsController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.AmountInPence._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDetailsAnswers, DisposalMethod, ShareOfProperty}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{ConditionalRadioUtils, FormUtils, MoneyUtils, NumberUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class DisposalDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  howMuchDidYouOwnPage: views.html.returns.disposaldetails.how_much_did_you_own,
  disposalPricePage: views.html.returns.disposaldetails.disposal_price,
  disposalFeesPage: views.html.returns.disposaldetails.disposal_fees,
  checkYouAnswers: views.html.returns.disposaldetails.check_your_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  private def withFillingOutReturnAndDisposalDetailsAnswers(
    request: RequestWithSessionData[_]
  )(
    f: (
      SessionData,
      FillingOutReturn,
      DisposalDetailsAnswers
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r: FillingOutReturn)) =>
        r.draftReturn.disposalDetailsAnswers
          .fold[Future[Result]](f(s, r, IncompleteDisposalDetailsAnswers.empty))(f(s, r, _))

      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def withDisposalMethod(fillingOutReturn: FillingOutReturn)(
    f: DisposalMethod => Future[Result]
  ): Future[Result] =
    fillingOutReturn.draftReturn.triageAnswers.fold(_.disposalMethod, c => Some(c.disposalMethod)) match {
      case Some(method) => f(method)
      case _            => Redirect(controllers.routes.StartController.start())
    }

  private def withDisposalMethodAndShareOfProperty(fillingOutReturn: FillingOutReturn, answers: DisposalDetailsAnswers)(
    f: (DisposalMethod, ShareOfProperty) => Future[Result]
  ): Future[Result] =
    withDisposalMethod(fillingOutReturn) { method =>
      answers.fold(_.shareOfProperty, c => Some(c.shareOfProperty)) match {
        case Some(share) => f(method, share)
        case _           => Redirect(routes.DisposalDetailsController.howMuchDidYouOwn())
      }
    }

  private def displayPage[A, P: Writeable, R](answers: DisposalDetailsAnswers)(form: DisposalDetailsAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: DisposalDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(answers).isDefined) {
      val backLink = answers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
      )

      Ok(page(form(answers), backLink))
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  private def submitBehaviour[A, P: Writeable, R](fillingOutReturn: FillingOutReturn, answers: DisposalDetailsAnswers)(
    form: Form[A]
  )(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: DisposalDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(updateAnswers: (A, DisposalDetailsAnswers) => DisposalDetailsAnswers, nextPage: DisposalDetailsAnswers => Call)(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(answers).isDefined) {
      lazy val backLink = answers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
      )
      form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink)), { value =>
            val newAnswers     = updateAnswers(value, answers)
            val newDraftReturn = fillingOutReturn.draftReturn.copy(disposalDetailsAnswers = Some(newAnswers))

            val result = for {
              _ <- if (newDraftReturn === fillingOutReturn.draftReturn) EitherT.pure(())
                  else returnsService.storeDraftReturn(newDraftReturn)
              _ <- EitherT(
                    updateSession(sessionStore, request)(
                      _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = newDraftReturn)))
                    )
                  )
            } yield ()

            result.fold({ e =>
              logger.warn("Could not update draft return", e)
              errorHandler.errorResult()
            }, _ => Redirect(nextPage(newAnswers)))
          }
        )
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  def howMuchDidYouOwn(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, _, answers) =>
        displayPage(answers)(
          form = _.fold(
            _.shareOfProperty.fold(shareOfPropertyForm)(shareOfPropertyForm.fill),
            c => shareOfPropertyForm.fill(c.shareOfProperty)
          )
        )(
          page = howMuchDidYouOwnPage(_, _)
        )(
          requiredPreviousAnswer               = _ => Some(()),
          redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
        )
    }
  }

  def howMuchDidYouOwnSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        submitBehaviour(fillingOutReturn, answers)(
          form = shareOfPropertyForm
        )(
          page = howMuchDidYouOwnPage(_, _)
        )(
          requiredPreviousAnswer               = _ => Some(()),
          redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
        )(
          updateAnswers = {
            case (percentage, d) =>
              d.fold(
                _.copy(shareOfProperty = Some(percentage)),
                _.copy(shareOfProperty = percentage)
              )
          },
          nextPage = _.fold(
            _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice(),
            _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )
        )
    }
  }

  def whatWasDisposalPrice(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withDisposalMethodAndShareOfProperty(fillingOutReturn, answers) {
          case (disposalMethod, shareOfProperty) =>
            displayPage(answers)(
              form = _.fold(
                _.disposalPrice.fold(disposalPriceForm)(a => disposalPriceForm.fill(a.inPounds())),
                c => disposalPriceForm.fill(c.disposalPrice.inPounds())
              )
            )(
              page = disposalPricePage(_, _, disposalMethod, shareOfProperty)
            )(
              requiredPreviousAnswer = _.fold(_.shareOfProperty, c => Some(c.shareOfProperty)),
              redirectToIfNoRequiredPreviousAnswer =
                controllers.returns.disposaldetails.routes.DisposalDetailsController.howMuchDidYouOwn()
            )
        }
    }
  }

  def whatWasDisposalPriceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withDisposalMethodAndShareOfProperty(fillingOutReturn, answers) {
          case (disposalMethod, shareOfProperty) =>
            submitBehaviour(fillingOutReturn, answers)(
              form = disposalPriceForm
            )(
              page = disposalPricePage(_, _, disposalMethod, shareOfProperty)
            )(
              requiredPreviousAnswer = _.fold(_.shareOfProperty, c => Some(c.shareOfProperty)),
              redirectToIfNoRequiredPreviousAnswer =
                controllers.returns.disposaldetails.routes.DisposalDetailsController.howMuchDidYouOwn()
            )(
              updateAnswers = {
                case (price, d) =>
                  d.fold(
                    _.copy(disposalPrice = Some(fromPounds(price))),
                    _.copy(disposalPrice = fromPounds(price))
                  )
              },
              nextPage = _.fold(
                _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWereDisposalFees(),
                _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
              )
            )
        }
    }
  }

  def whatWereDisposalFees(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withDisposalMethodAndShareOfProperty(fillingOutReturn, answers) {
          case (disposalMethod, shareOfProperty) =>
            displayPage(answers)(
              form = _.fold(
                _.disposalFees.fold(disposalFeesForm)(a => disposalFeesForm.fill(a.inPounds())),
                c => disposalFeesForm.fill(c.disposalFees.inPounds())
              )
            )(
              page = disposalFeesPage(_, _, disposalMethod, shareOfProperty)
            )(
              requiredPreviousAnswer = _.fold(_.disposalPrice, c => Some(c.disposalPrice)),
              redirectToIfNoRequiredPreviousAnswer =
                controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
            )
        }
    }
  }

  def whatWereDisposalFeesSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withDisposalMethodAndShareOfProperty(fillingOutReturn, answers) {
          case (disposalMethod, shareOfProperty) =>
            submitBehaviour(fillingOutReturn, answers)(
              form = disposalFeesForm
            )(
              page = disposalFeesPage(_, _, disposalMethod, shareOfProperty)
            )(
              requiredPreviousAnswer = _.fold(_.disposalPrice, c => Some(c.disposalPrice)),
              redirectToIfNoRequiredPreviousAnswer =
                controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
            )(
              updateAnswers = {
                case (price, d) =>
                  d.fold(
                    _.copy(disposalFees = Some(fromPounds(price))),
                    _.copy(disposalFees = fromPounds(price))
                  )
              },
              nextPage = _.fold(
                _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers(),
                _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
              )
            )
        }
    }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, fillingOutReturn, answers) =>
        withDisposalMethod(fillingOutReturn) {
          case (disposalMethod) =>
            answers match {
              case IncompleteDisposalDetailsAnswers(None, _, _) =>
                Redirect(routes.DisposalDetailsController.howMuchDidYouOwn())

              case IncompleteDisposalDetailsAnswers(_, None, _) =>
                Redirect(routes.DisposalDetailsController.whatWasDisposalPrice())

              case IncompleteDisposalDetailsAnswers(_, _, None) =>
                Redirect(routes.DisposalDetailsController.whatWereDisposalFees())

              case IncompleteDisposalDetailsAnswers(Some(share), Some(price), Some(fees)) =>
                val completeAnswers = CompleteDisposalDetailsAnswers(share, price, fees)
                val updatedDraftReturn =
                  fillingOutReturn.draftReturn.copy(disposalDetailsAnswers = Some(completeAnswers))

                val result = for {
                  _ <- returnsService.storeDraftReturn(updatedDraftReturn)
                  _ <- EitherT(
                        updateSession(sessionStore, request)(
                          _.copy(journeyStatus = Some(fillingOutReturn.copy(draftReturn = updatedDraftReturn)))
                        )
                      )
                } yield ()

                result.fold({ e =>
                  logger.warn("Could not update session", e)
                  errorHandler.errorResult
                }, _ => Ok(checkYouAnswers(completeAnswers, disposalMethod)))

              case answers: CompleteDisposalDetailsAnswers =>
                Ok(checkYouAnswers(answers, disposalMethod))
            }

        }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case _ =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }
  }

}

object DisposalDetailsController {

  val shareOfPropertyForm: Form[ShareOfProperty] = {

    val formatter: Formatter[ShareOfProperty] = {
      val (shareOfPropertyKey, percentageKey) = "shareOfProperty" -> "percentageShare"

      def validatePercentage(s: String): Either[FormError, ShareOfProperty] =
        Try(BigDecimal(s)).toEither
          .leftMap(_ => FormError(percentageKey, "error.invalid"))
          .flatMap { d =>
            if (d > 100) Left(FormError(percentageKey, "error.tooLarge"))
            else if (d < 0) Left(FormError(percentageKey, "error.tooSmall"))
            else if (NumberUtils.numberHasMoreThanNDecimalPlaces(d, 2))
              Left(FormError(percentageKey, "error.tooManyDecimals"))
            else if (d === BigDecimal(100)) Right(ShareOfProperty.Full)
            else if (d === BigDecimal(50)) Right(ShareOfProperty.Half)
            else Right(ShareOfProperty.Other(d))
          }

      ConditionalRadioUtils.formatter(shareOfPropertyKey)(
        List(
          Right(ShareOfProperty.Full),
          Right(ShareOfProperty.Half),
          Left(
            ConditionalRadioUtils.InnerOption(data =>
              FormUtils
                .readValue(percentageKey, data, identity)
                .flatMap(validatePercentage)
                .leftMap(Seq(_))
            )
          )
        )
      ) {
        case ShareOfProperty.Full => Map(shareOfPropertyKey -> "0")
        case ShareOfProperty.Half => Map(shareOfPropertyKey -> "1")
        case ShareOfProperty.Other(percentageValue) =>
          Map(shareOfPropertyKey -> "2", percentageKey -> percentageValue.toString)
      }
    }

    Form(
      formMapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

  implicit val fillingOutReturnEq: Eq[FillingOutReturn] = Eq.fromUniversalEquals

  val disposalPriceForm: Form[BigDecimal] =
    Form(
      formMapping(
        "disposalPrice" -> of(MoneyUtils.amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

  val disposalFeesForm: Form[BigDecimal] =
    Form(
      formMapping(
        "disposalFees" -> of(MoneyUtils.amountInPoundsFormatter(_ < 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

}
