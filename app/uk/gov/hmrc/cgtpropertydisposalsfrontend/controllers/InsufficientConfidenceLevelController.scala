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

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Forms.{mapping, nonEmptyText, of}
import play.api.data.Form
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.BusinessPartnerRecordRequest.IndividualBusinessPartnerRecordRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormat, Name, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.referencechecker.SelfAssessmentReferenceChecker

import scala.concurrent.{ExecutionContext, Future}

class InsufficientConfidenceLevelController @Inject()(
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  val config: Configuration,
  businessPartnerRecordService: BusinessPartnerRecordService,
  doYouHaveANinoPage: views.html.do_you_have_a_nino,
  doYouHaveAnSaUtrPage: views.html.do_you_have_an_sa_utr,
  enterSautrAndNamePage: views.html.enter_sa_utr_and_name,
  startRegistrationPage: views.html.registration.registration_start,
  cc: MessagesControllerComponents
)(
  implicit viewConfig: ViewConfig,
  ec: ExecutionContext
) extends FrontendController(cc)
    with IvBehaviour
    with Logging
    with WithAuthAndSessionDataAction
    with DefaultRedirects
    with SessionUpdates {
  import InsufficientConfidenceLevelController._
  import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.toFuture

  private def withInsufficientConfidenceLevelUser(
    f: IndividualWithInsufficientConfidenceLevel => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(i: IndividualWithInsufficientConfidenceLevel) => f(i)
      case other                                              => defaultRedirect(other)
    }

  def doYouHaveNINO(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser {
      case IndividualWithInsufficientConfidenceLevel(hasNino, _, _) =>
        val form = hasNino.fold(haveANinoForm)(haveANinoForm.fill)
        Ok(doYouHaveANinoPage(form))
    }
  }

  def doYouHaveNINOSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser { insufficientConfidenceLevel =>
      InsufficientConfidenceLevelController.haveANinoForm
        .bindFromRequest()
        .fold(
          e => BadRequest(doYouHaveANinoPage(e)),
          hasNino =>
            updateSession(sessionStore, request)(
              _.copy(journeyStatus = Some(insufficientConfidenceLevel.copy(hasNino = Some(hasNino))))
            ).map {
              case Left(e) =>
                logger.warn("Could not update session after has NINO page submit", e)
                errorHandler.errorResult()

              case Right(_) =>
                if (hasNino) {
                  redirectToIv
                } else {
                  Redirect(routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr())
                }
            }
        )
    }
  }

  def doYouHaveAnSaUtr(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser {
      case IndividualWithInsufficientConfidenceLevel(hasNino, hasSaUtr, _) =>
        hasNino.fold(
          SeeOther(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)
        ) { _ =>
          val form = hasSaUtr.fold(hasSaUtrForm)(hasSaUtrForm.fill)
          Ok(doYouHaveAnSaUtrPage(form, routes.InsufficientConfidenceLevelController.doYouHaveNINO()))
        }
    }
  }

  def doYouHaveSaUtrSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser { insufficientConfidenceLevel =>
      insufficientConfidenceLevel.hasNino.fold[Future[Result]](
        SeeOther(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)
      ) { _ =>
        hasSaUtrForm
          .bindFromRequest()
          .fold(
            e => BadRequest(doYouHaveAnSaUtrPage(e, routes.InsufficientConfidenceLevelController.doYouHaveNINO())),
            hasSautr =>
              updateSession(sessionStore, request)(
                _.copy(journeyStatus = Some(insufficientConfidenceLevel.copy(hasSautr = Some(hasSautr))))
              ).map {
                case Left(e) =>
                  logger.warn("Could not update session after has SAUTR page submit", e)
                  errorHandler.errorResult()

                case Right(_) =>
                  if (hasSautr)
                    Redirect(routes.InsufficientConfidenceLevelController.enterSautrAndName())
                  else
                    Redirect(routes.RegistrationController.startRegistration())
              }
          )
      }
    }
  }

  def enterSautrAndName(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser { insufficientConfidenceLevel =>
      (insufficientConfidenceLevel.hasNino, insufficientConfidenceLevel.hasSautr) match {
        case (Some(false), Some(true)) =>
          Ok(enterSautrAndNamePage(InsufficientConfidenceLevelController.sautrAndNameForm))
        case _ =>
          Redirect(routes.InsufficientConfidenceLevelController.doYouHaveNINO())
      }
    }
  }

  def enterSautrAndNameSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser { insufficientConfidenceLevel =>
      (insufficientConfidenceLevel.hasNino, insufficientConfidenceLevel.hasSautr) match {
        case (Some(false), Some(true)) =>
          InsufficientConfidenceLevelController.sautrAndNameForm
            .bindFromRequest()
            .fold(
              e => BadRequest(enterSautrAndNamePage(e)), {
                case (sautr, name) =>
                  val bprRequest = IndividualBusinessPartnerRecordRequest(Left(sautr), Some(name))

                  val result = for {
                    bpr <- businessPartnerRecordService.getBusinessPartnerRecord(bprRequest)
                    _ <- EitherT(
                          updateSession(sessionStore, request)(
                            _.copy(journeyStatus = Some(SubscriptionStatus.SubscriptionMissingData(bpr)))
                          )
                        )
                  } yield ()

                  result
                    .fold({ e =>
                      logger.warn("Could not get BPR with entered SA UTR", e)
                      errorHandler.errorResult()
                    }, { _ =>
                      Redirect(routes.StartController.start())
                    })
              }
            )

        case _ =>
          Redirect(routes.InsufficientConfidenceLevelController.doYouHaveNINO())
      }
    }
  }

}

object InsufficientConfidenceLevelController {

  val haveANinoForm: Form[Boolean] =
    Form(
      mapping(
        "hasNino" -> of(BooleanFormat.formatter)
      )(identity)(Some(_))
    )

  val hasSaUtrForm: Form[Boolean] =
    Form(
      mapping(
        "hasSaUtr" -> of(BooleanFormat.formatter)
      )(identity)(Some(_))
    )

  val sautrAndNameForm: Form[(SAUTR, Name)] =
    Form(
      mapping(
        "saUtr" -> nonEmptyText
          .transform[String](_.trim, identity)
          .verifying("error.pattern", SelfAssessmentReferenceChecker.isValid(_)),
        "firstName" -> Name.mapping,
        "lastName"  -> Name.mapping
      ) {
        case (sautr, firstName, lastName) => SAUTR(sautr) -> Name(firstName, lastName)
      } {
        case (sautr, name) => Some((sautr.value, name.firstName, name.lastName))
      }
    )

}
