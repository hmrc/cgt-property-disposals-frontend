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
import cats.instances.int._
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ShareOfProperty.{Full, Half}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDetailsAnswers, DisposalMethod, ShareOfProperty}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{MoneyUtils, NumberUtils, SessionData}
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
      DisposalDetailsAnswers,
      DisposalMethod,
      Option[ShareOfProperty]
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r: FillingOutReturn)) =>
        val shareOfProperty = r.draftReturn.disposalDetailsAnswers.flatMap(
          _.fold(_.shareOfProperty, c => Some(c.shareOfProperty))
        )

        r.draftReturn.triageAnswers.fold(_.disposalMethod, c => Some(c.disposalMethod)) match {
          case Some(d) =>
            r.draftReturn.disposalDetailsAnswers.fold[Future[Result]](
              f(s, r, IncompleteDisposalDetailsAnswers.empty, d, shareOfProperty)
            )(f(s, r, _, d, shareOfProperty))

          case None =>
            logger.warn("Could not find disposal method in disposal details section")
            Redirect(controllers.routes.StartController.start())
        }

      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def commonDisplayBehaviour[A, P: Writeable, R](form: DisposalDetailsAnswers => Form[A])(
    page: Call => P
  )(
    requiredPreviousAnswer: DisposalDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(answers: DisposalDetailsAnswers): Future[Result] =
    if (requiredPreviousAnswer(answers).isDefined) {
      val backLink = answers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
      )

      Ok(page(backLink))
    } else {
      Redirect(redirectToIfNoRequiredPreviousAnswer)
    }

  private def displayPageWithShareOfProperty[A, P: Writeable, R](form: DisposalDetailsAnswers => Form[A])(
    page: (Form[A], Call, DisposalMethod, ShareOfProperty) => P
  )(
    requiredPreviousAnswer: DisposalDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, _, answers, disposalMethod, Some(shareOfProperty)) =>
        commonDisplayBehaviour(form)(page(form(answers), _, disposalMethod, shareOfProperty))(
          requiredPreviousAnswer,
          redirectToIfNoRequiredPreviousAnswer
        )(answers)

      case (_, _, _, _, None) =>
        logger.warn("Could not find share of property value")
        Redirect(routes.DisposalDetailsController.howMuchDidYouOwn())

    }

  private def displayPage[A, P: Writeable, R](form: DisposalDetailsAnswers => Form[A])(
    page: (Form[A], Call, DisposalMethod) => P
  )(
    requiredPreviousAnswer: DisposalDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, _, answers, disposalMethod, _) =>
        commonDisplayBehaviour(form)(page(form(answers), _, disposalMethod))(
          requiredPreviousAnswer,
          redirectToIfNoRequiredPreviousAnswer
        )(answers)
    }

  private def commonSubmitBehaviour[A, P: Writeable, R](form: DisposalDetailsAnswers => Form[A])(
    page: (Form[A], Call, DisposalMethod) => P
  )(
    requiredPreviousAnswer: DisposalDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(
    updateAnswers: (A, DisposalDetailsAnswers) => DisposalDetailsAnswers,
    nextPage: DisposalDetailsAnswers => Call
  )(answers: DisposalDetailsAnswers, disposalMethod: DisposalMethod, fillingOurReturn: FillingOutReturn)(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(answers).isDefined) {
      lazy val backLink = answers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
      )
      form(answers)
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink, disposalMethod)), { value =>
            val newAnswers     = updateAnswers(value, answers)
            val newDraftReturn = fillingOurReturn.draftReturn.copy(disposalDetailsAnswers = Some(newAnswers))

            val result = for {
              _ <- if (newDraftReturn === fillingOurReturn.draftReturn) EitherT.pure(())
                  else returnsService.storeDraftReturn(newDraftReturn)
              _ <- EitherT(
                    updateSession(sessionStore, request)(
                      _.copy(journeyStatus = Some(fillingOurReturn.copy(draftReturn = newDraftReturn)))
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

  private def submitBehaviourWithShareOfProperty[A, P: Writeable, R](form: DisposalDetailsAnswers => Form[A])(
    page: (Form[A], Call, DisposalMethod, ShareOfProperty) => P
  )(
    requiredPreviousAnswer: DisposalDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(updateAnswers: (A, DisposalDetailsAnswers) => DisposalDetailsAnswers, nextPage: DisposalDetailsAnswers => Call)(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, r, answers, disposalMethod, Some(shareOfProperty)) =>
        commonSubmitBehaviour(form)(page(_, _, _, shareOfProperty))(
          requiredPreviousAnswer,
          redirectToIfNoRequiredPreviousAnswer
        )(updateAnswers, nextPage)(answers, disposalMethod, r)

      case (_, _, _, _, None) =>
        logger.warn("Could not find share of property value")
        Redirect(routes.DisposalDetailsController.howMuchDidYouOwn())
    }

  private def submitBehaviour[A, P: Writeable, R](form: DisposalDetailsAnswers => Form[A])(
    page: (Form[A], Call, DisposalMethod) => P
  )(
    requiredPreviousAnswer: DisposalDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(updateAnswers: (A, DisposalDetailsAnswers) => DisposalDetailsAnswers, nextPage: DisposalDetailsAnswers => Call)(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, r, answers, disposalMethod, _) =>
        commonSubmitBehaviour(form)(page(_, _, _))(
          requiredPreviousAnswer,
          redirectToIfNoRequiredPreviousAnswer
        )(updateAnswers, nextPage)(answers, disposalMethod, r)
    }

  def howMuchDidYouOwn(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayPage(
      form = _.fold(
        _.shareOfProperty.fold(shareOfPropertyForm)(shareOfPropertyForm.fill),
        c => shareOfPropertyForm.fill(c.shareOfProperty)
      )
    )(
      page = {
        case (form, backlink, _) => howMuchDidYouOwnPage(form, backlink)
      }
    )(
      requiredPreviousAnswer               = _ => Some(()),
      redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
    )
  }

  def howMuchDidYouOwnSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    submitBehaviour(
      form = _ => shareOfPropertyForm
    )(
      page = {
        case (form, backlink, _) => howMuchDidYouOwnPage(form, backlink)
      }
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

  def whatWasDisposalPrice(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayPageWithShareOfProperty(
      form = _.fold(
        _.disposalPrice.fold(disposalPriceForm)(a => disposalPriceForm.fill(a.inPounds())),
        c => disposalPriceForm.fill(c.disposalPrice.inPounds())
      )
    )(
      page = disposalPricePage(_, _, _, _)
    )(
      requiredPreviousAnswer = _.fold(_.shareOfProperty, c => Some(c.shareOfProperty)),
      redirectToIfNoRequiredPreviousAnswer =
        controllers.returns.disposaldetails.routes.DisposalDetailsController.howMuchDidYouOwn()
    )
  }

  def whatWasDisposalPriceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    submitBehaviourWithShareOfProperty(
      form = _ => disposalPriceForm
    )(
      page = disposalPricePage(_, _, _, _)
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

  def whatWereDisposalFees(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayPageWithShareOfProperty(
      form = _.fold(
        _.disposalFees.fold(disposalFeesForm)(a => disposalFeesForm.fill(a.inPounds())),
        c => disposalFeesForm.fill(c.disposalFees.inPounds())
      )
    )(
      page = disposalFeesPage(_, _, _, _)
    )(
      requiredPreviousAnswer = _.fold(_.disposalPrice, c => Some(c.disposalPrice)),
      redirectToIfNoRequiredPreviousAnswer =
        controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice()
    )
  }

  def whatWereDisposalFeesSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    submitBehaviourWithShareOfProperty(
      form = _ => disposalFeesForm
    )(
      page = disposalFeesPage(_, _, _, _)
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

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, fillingOutReturn, disposalDetailsAnswers, disposalMethod, _) =>
        disposalDetailsAnswers match {
          case IncompleteDisposalDetailsAnswers(None, _, _) =>
            Redirect(routes.DisposalDetailsController.howMuchDidYouOwn())

          case IncompleteDisposalDetailsAnswers(_, None, _) =>
            Redirect(routes.DisposalDetailsController.whatWasDisposalPrice())

          case IncompleteDisposalDetailsAnswers(_, _, None) =>
            Redirect(routes.DisposalDetailsController.whatWereDisposalFees())

          case IncompleteDisposalDetailsAnswers(Some(share), Some(price), Some(fees)) =>
            val completeAnswers    = CompleteDisposalDetailsAnswers(share, price, fees)
            val updatedDraftReturn = fillingOutReturn.draftReturn.copy(disposalDetailsAnswers = Some(completeAnswers))

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

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case _ =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
    }
  }

}

object DisposalDetailsController {

  val shareOfPropertyForm: Form[ShareOfProperty] = {

    val formatter: Formatter[ShareOfProperty] = new Formatter[ShareOfProperty] {
      def validatePercentage(d: BigDecimal, key: String): Either[FormError, ShareOfProperty] =
        if (d > 100) Left(FormError(key, "error.tooLarge"))
        else if (d < 0) Left(FormError(key, "error.tooSmall"))
        else if (NumberUtils.numberHasMoreThanNDecimalPlaces(d, 2)) Left(FormError(key, "error.tooManyDecimals"))
        else if (d === BigDecimal(100)) Right(ShareOfProperty.Full)
        else if (d === BigDecimal(50)) Right(ShareOfProperty.Half)
        else Right(ShareOfProperty.Other(d.toDouble))

      def readValue[A](key: String, data: Map[String, String], f: String => A): Either[FormError, A] =
        data
          .get(key)
          .map(_.trim())
          .filter(_.nonEmpty)
          .fold[Either[FormError, A]](Left(FormError(key, "error.required"))) { stringValue =>
            Either
              .fromTry(Try(f(stringValue)))
              .leftMap(_ => FormError(key, "error.invalid"))
          }

      val (shareOfPropertyKey, percentageKey) = "shareOfProperty" -> "percentageShare"

      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], ShareOfProperty] = {
        val result = readValue(shareOfPropertyKey, data, _.toInt)
          .flatMap { i =>
            if (i === 0)
              Right(Full)
            else if (i === 1)
              Right(Half)
            else if (i === 2) {
              readValue(percentageKey, data, BigDecimal(_))
                .flatMap(validatePercentage(_, percentageKey))
            } else {
              Left(FormError(shareOfPropertyKey, "error.invalid"))
            }
          }

        result.leftMap(Seq(_))
      }

      override def unbind(key: String, value: ShareOfProperty): Map[String, String] =
        value match {
          case ShareOfProperty.Full => Map(shareOfPropertyKey -> "0")
          case ShareOfProperty.Half => Map(shareOfPropertyKey -> "1")
          case ShareOfProperty.Other(percentageValue) =>
            Map(shareOfPropertyKey -> "2", percentageKey -> percentageValue.toString.stripSuffix(".0"))
        }
    }

    Form(
      formMapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

  implicit val fillingOutReturnEq: Eq[FillingOutReturn] = Eq.fromUniversalEquals

  val disposalPriceForm: Form[Double] =
    Form(
      formMapping(
        "disposalPrice" -> of(MoneyUtils.amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

  val disposalFeesForm: Form[Double] =
    Form(
      formMapping(
        "disposalFees" -> of(MoneyUtils.amountInPoundsFormatter(_ < 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

}
