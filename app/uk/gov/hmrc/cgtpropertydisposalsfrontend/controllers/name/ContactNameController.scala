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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.name

import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ContactNameController @Inject()(
  cc: MessagesControllerComponents,
  errorHandler: ErrorHandler,
  sessionStore: SessionStore,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  enterContactNamePage: views.html.contactname.contact_name
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withSubscriptionReady(
    request: RequestWithSessionData[_]
  )(f: SubscriptionReady => Future[Result]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(s: SubscriptionReady) => f(s)
      case _                          => Redirect(controllers.routes.StartController.start())
    }

  private def enterContactNamePageWithForm(form: Form[ContactName])(implicit request: RequestWithSessionData[_]) =
    enterContactNamePage(
      form,
      controllers.routes.SubscriptionController.checkYourDetails(),
      routes.ContactNameController.enterContactNameSubmit()
    )

  def enterContactName(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSubscriptionReady(request) { subscriptionReady =>
      Ok(
        enterContactNamePageWithForm(
          ContactName.form.fill(subscriptionReady.subscriptionDetails.contactName)
        )
      )
    }
  }

  def enterContactNameSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withSubscriptionReady(request) {
      case SubscriptionReady(subscriptionDetails) =>
        ContactName.form
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(enterContactNamePageWithForm(formWithErrors)), { contactName =>
              updateSession(sessionStore, request)(
                _.copy(journeyStatus = Some(SubscriptionReady(subscriptionDetails.copy(contactName = contactName))))
              ).map {
                case Left(e) =>
                  logger.warn("Could not update contact name in session", e)
                  errorHandler.errorResult()

                case Right(_) =>
                  Redirect(controllers.routes.SubscriptionController.checkYourDetails())
              }
            }
          )
    }
  }

}
