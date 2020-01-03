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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.agents

import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions, Enrolment, InsufficientEnrolments}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{CgtEnrolment, ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.agents.AgentAccessController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.AgentStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.AgentStatus.AgentSupplyingClientDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

class AgentAccessController @Inject()(
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  authConnector: AuthConnector,
  sessionStore: SessionStore,
  errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  enterClientsCgtRefPage: views.html.agents.enter_client_cgt_ref
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging { self =>

  private val authorisedFunctions: AuthorisedFunctions = new AuthorisedFunctions {
    override def authConnector: AuthConnector = self.authConnector
  }

  def enterClientsCgtRef(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAgentSupplyingClientDetails { _ =>
      Ok(enterClientsCgtRefPage(cgtReferenceForm))
    }
  }

  def enterClientsCgtRefSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withAgentSupplyingClientDetails {
      case AgentSupplyingClientDetails(ggCredId, _) =>
        cgtReferenceForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(enterClientsCgtRefPage(formWithErrors)),
            cgtReference => handleSubmittedCgtReferenceNumber(cgtReference, ggCredId)
          )
    }
  }

  private def handleSubmittedCgtReferenceNumber(
    cgtReference: CgtReference,
    ggCredId: GGCredId
  )(implicit hc: HeaderCarrier, request: RequestWithSessionData[_]): Future[Result] =
    authorisedFunctions
      .authorised(
        Enrolment(CgtEnrolment.enrolmentKey)
          .withIdentifier(CgtEnrolment.enrolmentIdentifier, cgtReference.value)
          .withDelegatedAuthRule(CgtEnrolment.delegateAuthRule)
      ) {
        updateSession(sessionStore, request)(
          _.copy(
            journeyStatus = Some(AgentSupplyingClientDetails(ggCredId, Some(cgtReference)))
          )
        ).map {
          case Left(e) =>
            logger.warn("Could not update session", e)
            errorHandler.errorResult(Some(UserType.Agent))

          case Right(_) =>
            Ok(s"Got $cgtReference")
        }
      }
      .recover {
        case _: InsufficientEnrolments =>
          BadRequest(
            enterClientsCgtRefPage(
              cgtReferenceForm
                .fill(cgtReference)
                .withError(cgtReferenceKey, "error.notPermitted")
            )
          )

        case NonFatal(error) =>
        logger.warn(s"Could not do delegated auth rule check for agent: ${error.getMessage}")
        errorHandler.errorResult(Some(UserType.Agent))
      }

  private def withAgentSupplyingClientDetails(
    f: AgentSupplyingClientDetails => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(a: AgentStatus.AgentSupplyingClientDetails) => f(a)
      case _                                                => Redirect(controllers.routes.StartController.start())
    }
}

object AgentAccessController {

  val cgtReferenceKey = "cgtReference"

  val cgtReferenceForm: Form[CgtReference] = Form(
    mapping(
      cgtReferenceKey -> CgtReference.mapping
    )(identity)(Some(_))
  )

}
