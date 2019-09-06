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
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.IndividualWithInsufficientConfidenceLevel
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{HasSAUTR, SubscriptionStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

@Singleton
class RegistrationController @Inject()(
                                        val authenticatedAction: AuthenticatedAction,
                                        val sessionDataAction: SessionDataAction,
                                        val sessionStore: SessionStore,
                                        val errorHandler: ErrorHandler,
                                        startRegistrationPage: views.html.registration.registration_start,
                                        cc: MessagesControllerComponents
                                      )(implicit viewConfig: ViewConfig)
  extends FrontendController(cc) with WithAuthAndSessionDataAction {

  def startRegistration(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.sessionData.flatMap(_.subscriptionStatus) match {
      case Some(IndividualWithInsufficientConfidenceLevel(Some(false), Some(HasSAUTR(None)))) =>
        Ok(startRegistrationPage(routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr()))
      case _ =>
        SeeOther(routes.StartController.start().url)
    }
  }

}
