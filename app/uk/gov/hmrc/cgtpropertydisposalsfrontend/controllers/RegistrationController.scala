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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Name
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
  extends FrontendController(cc) with WithAuthAndSessionDataAction with SessionUpdates with DefaultRedirects with Logging {
  import RegistrationController._


  private def withValidUser(request: RequestWithSessionData[_])(
    f: Either[IndividualWithInsufficientConfidenceLevel,RegistrationStatus] => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(u @ IndividualWithInsufficientConfidenceLevel(Some(false), Some(false), _, _)) =>
        f(Left(u))

      case Some(r: RegistrationStatus) =>
        f(Right(r))

      case _ =>
        SeeOther(routes.StartController.start().url)
    }

  private def carryOnIfIndividualSupplyingDetails(request: RequestWithSessionData[_])(
  f: RegistrationStatus.IndividualSupplyingInformation => Future[Result]): Future[Result] =
  withValidUser(request){
      case Right(supplyingInfo: RegistrationStatus.IndividualSupplyingInformation) =>
      f(supplyingInfo)

      case Right(RegistrationStatus.IndividualWantsToRegisterTrust) =>
      Redirect(routes.RegistrationController.wrongGGAccountForTrusts())

      case Right(_: RegistrationStatus.RegistrationReady) =>
        Redirect(routes.RegistrationController.checkYourAnswers())

      case Left(_) =>
      Redirect(routes.RegistrationController.startRegistration().url)
    }


  def startRegistration(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { case _ =>
      Ok(startRegistrationPage(routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr()))
    }
  }

  def selectEntityType():  Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { status =>
      val form = {
        val blankForm = RegistrationController.selectEntityTypeForm
        status.fold(_ => blankForm,
          {
            case _: RegistrationStatus.IndividualSupplyingInformation | _: RegistrationStatus.RegistrationReady =>
              blankForm.fill(EntityType.Individual)

            case RegistrationStatus.IndividualWantsToRegisterTrust =>
              blankForm.fill(EntityType.Trust)
          }
        )
      }

      Ok(selectEntityTypePage(form, routes.RegistrationController.startRegistration()))
    }
  }

  def selectEntityTypeSubmit(): Action[AnyContent] =  authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { status =>
      RegistrationController.selectEntityTypeForm.bindFromRequest().fold(
        e => BadRequest(selectEntityTypePage(e, routes.RegistrationController.startRegistration())),
        { entityType =>
          val (newRegistrationStatus, redirectTo): (RegistrationStatus, Call) = entityType match {
            case EntityType.Individual =>
              RegistrationStatus.IndividualSupplyingInformation(None, None) -> routes.RegistrationController.enterName()
            case EntityType.Trust =>
              RegistrationStatus.IndividualWantsToRegisterTrust -> routes.RegistrationController.wrongGGAccountForTrusts()
          }

          (status, newRegistrationStatus) match {
            case (Right(_: RegistrationStatus.IndividualSupplyingInformation), _: RegistrationStatus.IndividualSupplyingInformation) |
                 (Right(RegistrationStatus.IndividualWantsToRegisterTrust), RegistrationStatus.IndividualWantsToRegisterTrust) =>
              Redirect(redirectTo)

            case _ =>
              updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newRegistrationStatus))).map {
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
    withValidUser(request) {
        case Right(RegistrationStatus.IndividualWantsToRegisterTrust) =>
          Ok(wrongGGAccountForTrustPage(routes.RegistrationController.selectEntityType()))

        case _ =>
          Redirect(routes.RegistrationController.startRegistration())
    }
  }

  def enterName(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    carryOnIfIndividualSupplyingDetails(request) { supplyingInformation =>
      val form = {
        supplyingInformation.name.fold(Name.form)(Name.form.fill)
      }

      Ok(enterNamePage(form, routes.RegistrationController.selectEntityType()))
    }
  }

  def enterNameSubmit(): Action[AnyContent] =  authenticatedActionWithSessionData.async { implicit request =>
    carryOnIfIndividualSupplyingDetails(request) { supplyingInformation =>
      Name.form.bindFromRequest().fold(
        e => BadRequest(enterNamePage(e, routes.RegistrationController.selectEntityType())),
        name =>
          updateSession(sessionStore, request)(
            _.copy(journeyStatus = Some(supplyingInformation.copy(name = Some(name))))
          ).map{
            case Left(e) =>
              logger.warn("Could not update registration status with name", e)
              errorHandler.errorResult()

            case Right(_) =>
              Redirect(address.routes.RegistrationAddressController.isUk())
          }
      )
    }
  }


  def checkYourAnswers(): Action[AnyContent] = Action { implicit request =>
    Ok("placeholder for registration check your answers")
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
