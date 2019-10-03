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

import cats.instances.int._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
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
                                        enterNamePage: views.html.name.enter_name,
                                        checkYourDetailsPage: views.html.registration.check_your_details,
                                        cc: MessagesControllerComponents
                                      )(
                                        implicit viewConfig: ViewConfig,
                                        ec: ExecutionContext
                                      )
  extends FrontendController(cc) with WithAuthAndSessionDataAction with SessionUpdates with Logging {
  import RegistrationController._


  private def withValidUser(request: RequestWithSessionData[_])(
    f: Either[TryingToGetIndividualsFootprint,RegistrationStatus] => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(u @ TryingToGetIndividualsFootprint(Some(false), Some(false), _, _)) =>
        f(Left(u))

      case Some(r: RegistrationStatus) =>
        f(Right(r))

      case _ =>
        SeeOther(routes.StartController.start().url)
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
            case RegistrationStatus.IndividualWantsToRegisterTrust =>
              blankForm.fill(EntityType.Trust)

            case _ =>
              blankForm.fill(EntityType.Individual)
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
              RegistrationStatus.IndividualSupplyingInformation(
                None,
                None,
                status.fold(_.email, _ => None)
              ) -> name.routes.RegistrationEnterIndividualNameController.enterIndividualName()
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


  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(r: RegistrationStatus.RegistrationReady) =>
        Ok(checkYourDetailsPage(r))

      case Some(RegistrationStatus.IndividualSupplyingInformation(None, _, _)) =>
        Redirect(name.routes.RegistrationEnterIndividualNameController.enterIndividualName())

      case Some(RegistrationStatus.IndividualSupplyingInformation(_, None, _)) =>
        Redirect(address.routes.RegistrationEnterAddressController.isUk())

      case Some(RegistrationStatus.IndividualSupplyingInformation(Some(name), Some(address), None)) =>
        updateSession(sessionStore, request)(_.copy(journeyStatus =
          Some(RegistrationStatus.IndividualMissingEmail(name,address)))
        ).map {
          case Left(error) =>
            logger.warn("Could not update session for enter registration email journey", error)
            errorHandler.errorResult()

          case Right(_) =>
            Redirect(email.routes.RegistrationEnterEmailController.enterEmail())
        }

          case Some(RegistrationStatus.IndividualSupplyingInformation(Some(name), Some(address), Some(email))) =>
            val r = RegistrationStatus.RegistrationReady(name,address,email)
        updateSession(sessionStore, request)(_.copy(journeyStatus =
          Some(r))
        ).map{
          case Left(error) =>
          logger.warn("Could not update session for registration ready", error)
          errorHandler.errorResult()

          case Right(_) =>
            Ok(checkYourDetailsPage(r))
        }

      case _ =>
        Redirect(routes.StartController.start())

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
