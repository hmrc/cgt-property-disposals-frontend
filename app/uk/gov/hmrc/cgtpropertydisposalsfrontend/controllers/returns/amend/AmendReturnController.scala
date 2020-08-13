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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.amend

import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.RebasingEligibilityUtil
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.amend.AmendReturnController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.BooleanFormatter
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.StartingToAmendReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{amend => pages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.Future

class AmendReturnController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  rebasingEligibilityUtil: RebasingEligibilityUtil,
  youNeedToCalculatePage: pages.you_need_to_calculate,
  confirmCancelPage: pages.confirm_cancel,
  checkYourAnswersPage: pages.check_your_answers
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def youNeedToCalculate(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withStartingToAmendReturn(request) { _ =>
        Ok(youNeedToCalculatePage(controllers.returns.routes.ViewReturnController.displayReturn()))
      }
    }

  def confirmCancel(back: String): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      confirmCancelBackLinkMappings.get(back) match {
        case None           =>
          logger.warn(s"Could not get back link location for '$back'")
          errorHandler.errorResult()

        case Some(backLink) =>
          Ok(
            confirmCancelPage(
              confirmCancelForm,
              backLink,
              routes.AmendReturnController.confirmCancelSubmit(back)
            )
          )
      }
    }

  def confirmCancelSubmit(back: String): Action[AnyContent] =
    authenticatedActionWithSessionData { implicit request =>
      confirmCancelBackLinkMappings.get(back) match {
        case None           =>
          logger.warn(s"Could not get back link location for '$back'")
          errorHandler.errorResult()

        case Some(backLink) =>
          confirmCancelForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  confirmCancelPage(formWithErrors, backLink, routes.AmendReturnController.confirmCancelSubmit(back))
                ),
              { cancel =>
                val redirectTo =
                  if (cancel) controllers.returns.routes.ViewReturnController.displayReturn()
                  else backLink

                Redirect(redirectTo)
              }
            )
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withStartingToAmendReturn(request) { journey =>
        Ok(
          checkYourAnswersPage(
            journey.originalReturn.completeReturn,
            rebasingEligibilityUtil,
            journey.subscribedDetails,
            journey.originalReturn.completeReturn.representativeType(),
            journey.originalReturn.completeReturn.isIndirectDisposal(),
            Some(!journey.originalReturn.isFirstReturn),
            routes.AmendReturnController.youNeedToCalculate()
          )
        )
      }
    }

  private def withStartingToAmendReturn(
    request: RequestWithSessionData[_]
  )(f: StartingToAmendReturn => Future[Result]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(s: StartingToAmendReturn) => f(s)
      case _                              => Redirect(controllers.routes.StartController.start())
    }

}

object AmendReturnController {

  object ConfirmCancelBackLocations {
    val calculateAmounts: String = "calculateAmounts"
    val checkAnswers: String     = "checkAnswers"
  }

  val confirmCancelBackLinkMappings: Map[String, Call] =
    Map(
      ConfirmCancelBackLocations.calculateAmounts -> routes.AmendReturnController.youNeedToCalculate(),
      ConfirmCancelBackLocations.checkAnswers     -> routes.AmendReturnController.checkYourAnswers()
    )

  val confirmCancelForm: Form[Boolean] =
    Form(
      mapping(
        "confirmCancelAmendReturn" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

}
