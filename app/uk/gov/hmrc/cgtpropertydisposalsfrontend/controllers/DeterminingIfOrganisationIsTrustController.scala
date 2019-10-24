/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.DeterminingIfOrganisationIsTrustController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.BooleanFormatter
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.DeterminingIfOrganisationIsTrust
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class DeterminingIfOrganisationIsTrustController @Inject()(
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  errorHandler: ErrorHandler,
  sessionStore: SessionStore,
  doYouWantToReportForATrustPage: views.html.subscription.do_you_want_to_report_for_a_trust,
  reportWithCorporateTaxPage: views.html.subscription.report_with_corporate_tax,
  registerYourTrustPage: views.html.register_your_trust,
  cc: MessagesControllerComponents
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def withValidUser(
    request: RequestWithSessionData[_]
  )(f: DeterminingIfOrganisationIsTrust => Future[Result]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(d: DeterminingIfOrganisationIsTrust) => f(d)
      case _                                         => Redirect(routes.StartController.start())
    }

  def doYouWantToReportForATrust(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { determiningIfOrganisationIsTrust =>
      val form =
        determiningIfOrganisationIsTrust.isReportingForTrust.fold(doYouWantToReportForATrustForm)(
          doYouWantToReportForATrustForm.fill
        )
      Ok(doYouWantToReportForATrustPage(form))
    }
  }

  def doYouWantToReportForATrustSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withValidUser(request) { _ =>
        doYouWantToReportForATrustForm
          .bindFromRequest()
          .fold(
            formWithError => BadRequest(doYouWantToReportForATrustPage(formWithError)), { isReportingForTrust =>
              updateSession(sessionStore, request)(
                _.copy(journeyStatus = Some(DeterminingIfOrganisationIsTrust(Some(isReportingForTrust))))
              ).map {
                case Left(e) =>
                  logger.warn("Could not update session data with reporting for trust answer", e)
                  errorHandler.errorResult()

                case Right(_) =>
                  if (isReportingForTrust)
                    Redirect(routes.DeterminingIfOrganisationIsTrustController.doYouHaveATrn())
                  else
                    Redirect(routes.DeterminingIfOrganisationIsTrustController.reportWithCorporateTax())
              }
            }
          )
      }
  }

  def reportWithCorporateTax(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { determiningIfOrganisationIsTrust =>
      if (determiningIfOrganisationIsTrust.isReportingForTrust.contains(false)) {
        Ok(reportWithCorporateTaxPage())
      } else {
        Redirect(routes.StartController.start())
      }
    }
  }

  def doYouHaveATrn(): Action[AnyContent] = Action { implicit request =>
    Ok("Do you have a TRN?")
  }

  def registerYourTrust(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { _ =>
      Ok(registerYourTrustPage())
    }
  }

}

object DeterminingIfOrganisationIsTrustController {

  val doYouWantToReportForATrustForm: Form[Boolean] =
    Form(
      mapping(
        "isReportingForATrust" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

}
