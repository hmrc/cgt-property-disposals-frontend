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
import cats.instances.future._
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.{Form, FormError}
import play.api.data.Forms.{bigDecimal, mapping => formMapping, of}
import play.api.data.format.Formatter
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.http.Writeable
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails.DisposalDetailsController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.AmountInPence._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDetailsAnswers, ShareOfProperty}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.IncompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ShareOfProperty.{Full, Half}
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
  )(f: (SessionData, FillingOutReturn, DisposalDetailsAnswers) => Future[Result]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r: FillingOutReturn)) =>
        r.draftReturn.disposalDetailsAnswers.fold[Future[Result]](
          f(s, r, IncompleteDisposalDetailsAnswers.empty)
        )(f(s, r, _))

      case _ => Redirect(controllers.routes.StartController.start())
    }

  def displayPage[A, P: Writeable, R](form: DisposalDetailsAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: DisposalDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, _, d) =>
        if (requiredPreviousAnswer(d).isDefined) {
          val backLink = d.fold(
            _ => redirectToIfNoRequiredPreviousAnswer,
            _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )

          Ok(page(form(d), backLink))
        } else {
          Redirect(redirectToIfNoRequiredPreviousAnswer)
        }

    }

  def s[A, P: Writeable, R](form: DisposalDetailsAnswers => Form[A])(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: DisposalDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(updateAnswers: (A, DisposalDetailsAnswers) => DisposalDetailsAnswers, nextPage: DisposalDetailsAnswers => Call)(
    implicit request: RequestWithSessionData[_]
  ): Future[Result] =
    withFillingOutReturnAndDisposalDetailsAnswers(request) {
      case (_, r, d) =>
        lazy val backLink = d.fold(
          _ => redirectToIfNoRequiredPreviousAnswer,
          _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
        )
        form(d)
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(page(formWithErrors, backLink)), { value =>
              val newAnswers     = updateAnswers(value, d)
              val newDraftReturn = r.draftReturn.copy(disposalDetailsAnswers = Some(newAnswers))

              val result = for {
                _ <- if (newDraftReturn === r.draftReturn) EitherT.pure(())
                    else returnsService.storeDraftReturn(newDraftReturn)
                _ <- EitherT(
                      updateSession(sessionStore, request)(
                        _.copy(journeyStatus = Some(r.copy(draftReturn = newDraftReturn)))
                      )
                    )
              } yield ()

              result.fold({ e =>
                logger.warn("Could not update draft return", e)
                errorHandler.errorResult()
              }, _ => Redirect(nextPage(newAnswers)))
            }
          )
    }

  def howMuchDidYouOwn(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayPage(
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

  def howMuchDidYouOwnSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    s(
      form = _.fold(
        _.shareOfProperty.fold(shareOfPropertyForm)(shareOfPropertyForm.fill),
        c => shareOfPropertyForm.fill(c.shareOfProperty)
      )
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

  def whatWasDisposalPrice(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    displayPage(
      form = _.fold(
        _.disposalPrice.fold(propertyPriceForm)(a => propertyPriceForm.fill(a.inPounds())),
        c => propertyPriceForm.fill(c.disposalPrice.inPounds())
      )
    )(
      page = disposalPricePage(_, _)
    )(
      requiredPreviousAnswer = _.fold(_.shareOfProperty, c => Some(c.shareOfProperty)),
      redirectToIfNoRequiredPreviousAnswer =
        controllers.returns.disposaldetails.routes.DisposalDetailsController.howMuchDidYouOwn()
    )
  }

  def whatWasDisposalPriceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    s(
      form = _.fold(
        _.disposalPrice.fold(propertyPriceForm)(a => propertyPriceForm.fill(a.inPounds())),
        c => propertyPriceForm.fill(c.disposalPrice.inPounds())
      )
    )(
      page = disposalPricePage(_, _)
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

  def whatWereDisposalFees(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok("")
  }

  def whatWereDisposalFeesSubmit(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok("")
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok("")
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok("")
  }
}

object DisposalDetailsController {

  def numberHasMoreThanNDecimalPlaces(d: BigDecimal, n: Int): Boolean =
    d.toString.split('.').toList match {
      case _ :: decimals :: _ => decimals.length() > n
      case _                  => false
    }

  val shareOfPropertyForm: Form[ShareOfProperty] = {

    val formatter: Formatter[ShareOfProperty] = new Formatter[ShareOfProperty] {
      def validatePercentage(d: BigDecimal, key: String): Either[FormError, ShareOfProperty.Other] =
        if (d > 100) Left(FormError(key, "error.tooLarge"))
        else if (d < 0) Left(FormError(key, "error.tooSmall"))
        else if (numberHasMoreThanNDecimalPlaces(d, 2)) Left(FormError(key, "error.tooManyDecimals"))
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
              Left(FormError(key, "error.invalid"))
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

  val propertyPriceForm: Form[Double] = {

    def validatePercentage(d: BigDecimal): ValidationResult =
      // TODO: find out actual max value
      if (d > 100) Invalid("error.tooLarge")
      // TODO: is 0 valid?
      else if (d < 0) Invalid("error.tooSmall")
      else if (numberHasMoreThanNDecimalPlaces(d, 2)) Invalid("error.tooManyDecimals")
      else Valid

    Form(
      formMapping(
        "propertyPrice" ->
          bigDecimal
            .verifying(Constraint[BigDecimal](validatePercentage(_)))
            .transform[Double](_.toDouble, BigDecimal(_))
      )(identity)(Some(_))
    )
  }

}
