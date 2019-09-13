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
import play.api.data.Forms.{mapping, number}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.{IndividualWithInsufficientConfidenceLevel, SubscriptionMissingData, SubscriptionReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{HasSAUTR, Name, RegistrationStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RegistrationController @Inject()(
                                        val authenticatedAction: AuthenticatedAction,
                                        val sessionDataAction: SessionDataAction,
                                        val sessionStore: SessionStore,
                                        val errorHandler: ErrorHandler,
                                        startRegistrationPage: views.html.registration.registration_start,
                                        selectEntityTypePage: views.html.registration.select_entity_type,
                                        wrongGGAccountForTrustPage: views.html.wrong_gg_account_for_trust,
                                        enterNamePage: views.html.registration.enter_name,
                                        cc: MessagesControllerComponents
                                      )(
                                        implicit viewConfig: ViewConfig,
                                        ec: ExecutionContext
                                      )
  extends FrontendController(cc) with WithAuthAndSessionDataAction with SessionUpdates with Logging {
  import RegistrationController._

  private def carryOnIfIndividualWithInsufficientConfidenceLevel(request: RequestWithSessionData[_])(
    f: (IndividualWithInsufficientConfidenceLevel, Option[RegistrationStatus]) => Future[Result]): Future[Result] =
    request.sessionData.flatMap(_.subscriptionStatus) match {
      case Some(u @ IndividualWithInsufficientConfidenceLevel(Some(false), Some(HasSAUTR(None)), _, _))
      => f(u, request.sessionData.flatMap(_.registrationStatus))
      case _
      =>  SeeOther(routes.StartController.start().url)
    }

  private def carryOnIfIndividualSupplyingDetails(request: RequestWithSessionData[_])(
    f: (IndividualWithInsufficientConfidenceLevel, RegistrationStatus.IndividualSupplyingInformation) => Future[Result]): Future[Result] =
    carryOnIfIndividualWithInsufficientConfidenceLevel(request){ case (i, r) =>
      r match {
        case Some(supplyingInfo: RegistrationStatus.IndividualSupplyingInformation) =>
          f(i, supplyingInfo)

        case Some(RegistrationStatus.IndividualWantsToRegisterTrust) =>
          Redirect(routes.RegistrationController.wrongGGAccountForTrusts())

        case None =>
          Redirect(routes.RegistrationController.startRegistration())
      }

    }

  def startRegistration(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    carryOnIfIndividualWithInsufficientConfidenceLevel(request) { case _ =>
      Ok(startRegistrationPage(routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr()))
    }
  }

  def selectEntityType():  Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    carryOnIfIndividualWithInsufficientConfidenceLevel(request) { case (_, registrationStatus) =>
      val form = {
        val blankForm = RegistrationController.selectEntityTypeForm
        registrationStatus.fold(blankForm){
          case _: RegistrationStatus.IndividualSupplyingInformation =>
            blankForm.fill(EntityType.Individual)

          case RegistrationStatus.IndividualWantsToRegisterTrust =>
            blankForm.fill(EntityType.Trust)
        }
      }

      Ok(selectEntityTypePage(form, routes.RegistrationController.startRegistration()))
    }
  }

  def selectEntityTypeSubmit(): Action[AnyContent] =  authenticatedActionWithSessionData.async { implicit request =>
    carryOnIfIndividualWithInsufficientConfidenceLevel(request) { case (_, registrationStatus) =>
      RegistrationController.selectEntityTypeForm.bindFromRequest().fold(
        e => BadRequest(selectEntityTypePage(e, routes.RegistrationController.startRegistration())),
        { entityType =>
          val (newRegistrationStatus, redirectTo): (RegistrationStatus, Call) = entityType match {
            case EntityType.Individual =>
              RegistrationStatus.IndividualSupplyingInformation(None) -> routes.RegistrationController.enterName()
            case EntityType.Trust =>
              RegistrationStatus.IndividualWantsToRegisterTrust -> routes.RegistrationController.wrongGGAccountForTrusts()
          }

          (registrationStatus, newRegistrationStatus) match {
            case (Some(_: RegistrationStatus.IndividualSupplyingInformation), _: RegistrationStatus.IndividualSupplyingInformation) |
              (Some(RegistrationStatus.IndividualWantsToRegisterTrust), RegistrationStatus.IndividualWantsToRegisterTrust) =>
              Redirect(redirectTo)

            case _ =>
              updateSession(sessionStore, request)(_.copy(registrationStatus = Some(newRegistrationStatus))).map {
                case Left(e) =>
                  logger.warn("Could not update registration status", e)
                  errorHandler.errorResult()
                case Right(_) =>
                  Redirect(redirectTo)
              }
          }
        }
      )
    }
  }

  def wrongGGAccountForTrusts(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    carryOnIfIndividualWithInsufficientConfidenceLevel(request) { case (_, registrationStatus) =>
      registrationStatus match {
        case Some(RegistrationStatus.IndividualWantsToRegisterTrust) =>
          Ok(wrongGGAccountForTrustPage(routes.RegistrationController.selectEntityType()))

        case _ =>
          Redirect(routes.RegistrationController.startRegistration())
      }
    }
  }

  def enterName(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    carryOnIfIndividualSupplyingDetails(request) { case (_, supplyingInformation ) =>
      val form = {
        supplyingInformation.name.fold(Name.form)(Name.form.fill)
      }

      Ok(enterNamePage(form, routes.RegistrationController.selectEntityType()))
    }
  }

  def enterNameSubmit(): Action[AnyContent] =  authenticatedActionWithSessionData.async { implicit request =>
    carryOnIfIndividualSupplyingDetails(request) { case (_, supplyingInformation) =>
      Name.form.bindFromRequest().fold(
        e => BadRequest(enterNamePage(e, routes.RegistrationController.selectEntityType())),
        name =>
        updateSession(sessionStore, request)(
          _.copy(registrationStatus = Some(supplyingInformation.copy(name = Some(name))))
        ).map{
          case Left(e) =>
          logger.warn("Could not update registration status with name", e)
          errorHandler.errorResult()

          case Right(_) =>
            Ok(s"your name is ${name.firstName} ${name.lastName}")
        }
      )
    }
  }
}

object RegistrationController {

  sealed trait EntityType extends Product with Serializable

  object EntityType {
    final case object Individual extends EntityType
    final case object Trust extends EntityType
  }

  val selectEntityTypeForm: Form[EntityType] =
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
