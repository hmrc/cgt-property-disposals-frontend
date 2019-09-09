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

import cats.syntax.eq._
import cats.instances.int._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText, number}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.IndividualWithInsufficientConfidenceLevel
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.HasSAUTR
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class RegistrationController @Inject()(
                                        val authenticatedAction: AuthenticatedAction,
                                        val sessionDataAction: SessionDataAction,
                                        val sessionStore: SessionStore,
                                        val errorHandler: ErrorHandler,
                                        startRegistrationPage: views.html.registration.registration_start,
                                        selectEntityTypePage: views.html.registration.select_entity_type,
                                        wrongGGAccountForTrustPage: views.html.wrong_gg_account_for_trust,
                                        cc: MessagesControllerComponents
                                      )(
                                        implicit viewConfig: ViewConfig,
                                        ec: ExecutionContext
                                      )
  extends FrontendController(cc) with WithAuthAndSessionDataAction {
  import RegistrationController._
  def startRegistration(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.sessionData.flatMap(_.subscriptionStatus) match {
      case Some(IndividualWithInsufficientConfidenceLevel(Some(false), Some(HasSAUTR(None)), _, _)) =>
        Ok(startRegistrationPage(routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr()))
      case _ =>
        SeeOther(routes.StartController.start().url)
    }
  }

  def selectEntityType():  Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok(selectEntityTypePage(RegistrationController.selectUserTypeForm, routes.RegistrationController.startRegistration()))
  }

  def selectEntityTypeSubmit(): Action[AnyContent] =  authenticatedActionWithSessionData { implicit request =>
    RegistrationController.selectUserTypeForm.bindFromRequest().fold(
      e => BadRequest(selectEntityTypePage(e, routes.RegistrationController.startRegistration())),
      {
        case EntityType.Individual => Ok("You can register as an individual")
        case EntityType.Trust => Redirect(routes.RegistrationController.wrongGGAccountForTrusts())
      }
    )
  }

  def wrongGGAccountForTrusts(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    Ok(wrongGGAccountForTrustPage(routes.RegistrationController.selectEntityType()))
  }

}

object RegistrationController {

  sealed trait EntityType

  object EntityType {
    final case object Individual extends EntityType
    final case object Trust extends EntityType
  }

  val selectUserTypeForm: Form[EntityType] =
  Form(
    mapping(
    "entityType" -> number
        .verifying("invalid", a => a === 0 || a === 1)
        .transform[EntityType](value => if(value === 0) EntityType.Individual else EntityType.Trust, {
          case EntityType.Individual => 0
          case EntityType.Trust => 1
        })
    )(identity)(Some(_))
  )
}
