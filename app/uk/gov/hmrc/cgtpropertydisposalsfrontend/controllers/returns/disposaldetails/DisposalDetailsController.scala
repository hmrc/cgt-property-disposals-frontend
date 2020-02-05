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
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{bigDecimal, mapping => formMapping}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationResult}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails.DisposalDetailsController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.IncompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

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

  def withFillingOutReturn(
    request: RequestWithSessionData[_]
  )(f: (SessionData, FillingOutReturn) => Future[Result]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s, r: FillingOutReturn)) => f(s, r)
      case _                              => Redirect(controllers.routes.StartController.start())
    }

  def howMuchDidYouOwn(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturn(request) {
      case (_, r) =>
        val (backLink, form) = r.draftReturn.disposalDetailsAnswers.fold(
          controllers.returns.routes.TaskListController.taskList() -> propertySharePercentageForm
        )(
          _.fold(
            incomplete =>
              controllers.returns.routes.TaskListController.taskList() ->
                incomplete.percentageOwned.fold(propertySharePercentageForm)(propertySharePercentageForm.fill),
            complete =>
              controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers() ->
                propertySharePercentageForm.fill(complete.percentageOwned)
          )
        )

        Ok(howMuchDidYouOwnPage(form, backLink))

    }
  }

  def howMuchDidYouOwnSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withFillingOutReturn(request) {
      case (_, r) =>
        lazy val backLink = r.draftReturn.disposalDetailsAnswers.fold(
          controllers.returns.routes.TaskListController.taskList()
        )(
          _.fold(
            _ => controllers.returns.routes.TaskListController.taskList(),
            _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
          )
        )
        propertySharePercentageForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(howMuchDidYouOwnPage(formWithErrors, backLink)), { percentage =>
              val newAnswers =
                r.draftReturn.disposalDetailsAnswers.fold[DisposalDetailsAnswers](
                  IncompleteDisposalDetailsAnswers(Some(percentage), None, None)
                )(
                  _.fold(
                    _.copy(percentageOwned = Some(percentage)),
                    _.copy(percentageOwned = percentage)
                  )
                )

              val newDraftReturn = r.draftReturn.copy(disposalDetailsAnswers = Some(newAnswers))

              lazy val redirectTo = newAnswers.fold(
                _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.whatWasDisposalPrice(),
                _ => controllers.returns.disposaldetails.routes.DisposalDetailsController.checkYourAnswers()
              )

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
              }, _ => Redirect(redirectTo))
            }
          )
    }
  }

  def whatWasDisposalPrice(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok("")
  }

  def whatWasDisposalPriceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok("")
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

  val propertySharePercentageForm: Form[Double] = {
    def numberHasMoreThanNDecimalPlaces(d: BigDecimal, n: Int): Boolean =
      d.toString.split('.').toList match {
        case _ :: decimals :: _ => decimals.length() > n
        case _                  => false
      }

    def validatePercentage(d: BigDecimal): ValidationResult =
      if (d > 100) Invalid("error.tooLarge")
      else if (d < 0) Invalid("error.tooSmall")
      else if (numberHasMoreThanNDecimalPlaces(d, 2)) Invalid("error.tooManyDecimals")
      else Valid

    Form(
      formMapping(
        "propertySharePercentage" ->
          bigDecimal
            .verifying(Constraint(validatePercentage(_)))
            .transform[Double](_.toDouble, BigDecimal(_))
      )(identity)(Some(_))
    )
  }

  implicit val fillingOutReturnEq: Eq[FillingOutReturn] = Eq.fromUniversalEquals

}
