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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.OrganisationUnregisteredTrust
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class RegisterTrustController @Inject()(
                                         val authenticatedAction: AuthenticatedAction,
                                         val sessionDataAction: SessionDataAction,
                                         registerYourTrustPage: views.html.register_your_trust,
                                         cc: MessagesControllerComponents)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
  extends FrontendController(cc) with WithAuthAndSessionDataAction with DefaultRedirects {


  def registerYourTrust(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.sessionData.flatMap(_.subscriptionStatus) match {
      case Some(OrganisationUnregisteredTrust) =>  Ok(registerYourTrustPage())
      case other                               => defaultRedirect(other)
    }
  }

}
