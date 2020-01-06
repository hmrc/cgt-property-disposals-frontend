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

import cats.data.EitherT
import cats.instances.future._
import cats.instances.string._
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.http.Writeable
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions, Enrolment, InsufficientEnrolments}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{CgtEnrolment, ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.agents.AgentAccessController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.AgentStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.AgentStatus.{AgentSupplyingClientDetails, VerifierMatchingDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
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
  subscriptionService: SubscriptionService,
  cc: MessagesControllerComponents,
  enterClientsCgtRefPage: views.html.agents.enter_client_cgt_ref,
  enterClientsPostcodePage: views.html.agents.enter_postcode,
  enterClientsCountryPage: views.html.agents.enter_country
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

  def enterClientsPostcode(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withVerifierMatchingDetails {
      case (_, verifierMatchingDetails) =>
        verifierMatchingDetails.clientDetails.address match {
          case UkAddress(_, _, _, _, postcode) =>
            val form =
              if (verifierMatchingDetails.correctVerifierSupplied) postcodeForm.fill(postcode) else postcodeForm
            Ok(enterClientsPostcodePage(form))

          case _: NonUkAddress =>
            Redirect(routes.AgentAccessController.enterClientsCountry())
        }
    }
  }

  def enterClientsPostcodeSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withVerifierMatchingDetails {
      case (agentSupplyingClientDetails, verifierMatchingDetails) =>
        verifierMatchingDetails.clientDetails.address match {
          case UkAddress(_, _, _, _, clientPostcode) =>
            handleSubmittedVerifier(
              clientPostcode
            )(
              (p1, p2) => toUpperWithNoSpaces(p1.value) === toUpperWithNoSpaces(p2.value),
              postcodeForm,
              postcodeKey,
              enterClientsPostcodePage(_),
              agentSupplyingClientDetails,
              verifierMatchingDetails
            )
          case _: NonUkAddress =>
            Redirect(routes.AgentAccessController.enterClientsCountry())
        }
    }
  }

  def enterClientsCountry(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withVerifierMatchingDetails {
      case (_, verifierMatchingDetails) =>
        verifierMatchingDetails.clientDetails.address match {
          case NonUkAddress(_, _, _, _, _, country) =>
            val form =
              if (verifierMatchingDetails.correctVerifierSupplied) countryForm.fill(country) else countryForm
            Ok(enterClientsCountryPage(form))

          case _: UkAddress =>
            Redirect(routes.AgentAccessController.enterClientsPostcode())
        }
    }
  }

  def enterClientsCountrySubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withVerifierMatchingDetails {
      case (agentSupplyingClientDetails, verifierMatchingDetails) =>
        verifierMatchingDetails.clientDetails.address match {
          case NonUkAddress(_, _, _, _, _, clientCountry) =>
            handleSubmittedVerifier(
              clientCountry
            )(
              _ === _,
              countryForm,
              countryKey,
              enterClientsCountryPage(_),
              agentSupplyingClientDetails,
              verifierMatchingDetails
            )
          case _: UkAddress =>
            Redirect(routes.AgentAccessController.enterClientsPostcode())
        }
    }
  }
  private def toUpperWithNoSpaces(s: String): String = s.toUpperCase.replaceAllLiterally(" ", "")

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
        val result = for {
          details <- subscriptionService.getSubscribedDetails(cgtReference)
          _ <- EitherT(
                updateSession(sessionStore, request)(
                  _.copy(
                    journeyStatus =
                      Some(AgentSupplyingClientDetails(ggCredId, Some(VerifierMatchingDetails(details, false))))
                  )
                )
              )
        } yield details

        result.fold(
          { e =>
            logger.warn("Could not handle submitted cgt reference", e)
            errorHandler.errorResult(request.userType)
          }, { clientDetails =>
            val redirectTo = clientDetails.address match {
              case _: UkAddress    => routes.AgentAccessController.enterClientsPostcode()
              case _: NonUkAddress => routes.AgentAccessController.enterClientsCountry()
            }

            Redirect(redirectTo)
          }
        )
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
          errorHandler.errorResult(request.userType)
      }

  private def handleSubmittedVerifier[V, P: Writeable](
    actualVerifier: V
  )(
    matches: (V, V) => Boolean,
    form: Form[V],
    formKey: String,
    page: Form[V] => P,
    currentAgentSupplyingClientDetails: AgentSupplyingClientDetails,
    currentVerifierMatchingDetails: VerifierMatchingDetails
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    form
      .bindFromRequest()
      .fold(
        formWithErrors => BadRequest(page(formWithErrors)),
        submittedVerifier =>
          if (matches(submittedVerifier, actualVerifier)) {
            updateSession(sessionStore, request)(
              _.copy(
                journeyStatus = Some(
                  currentAgentSupplyingClientDetails.copy(
                    verifierMatchingDetails = Some(
                      currentVerifierMatchingDetails.copy(
                        correctVerifierSupplied = true
                      )
                    )
                  )
                )
              )
            ).map {
              case Left(e) =>
                logger.warn("Could not update session", e)
                errorHandler.errorResult(request.userType)

              case Right(_) =>
                Ok("verifier matched")
            }
          } else
            BadRequest(page(form.fill(submittedVerifier).withError(formKey, "error.noMatch")))
      )

  private def withAgentSupplyingClientDetails(
    f: AgentSupplyingClientDetails => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(a: AgentStatus.AgentSupplyingClientDetails) => f(a)
      case _                                                => Redirect(controllers.routes.StartController.start())
    }

  private def withVerifierMatchingDetails(
    f: (AgentSupplyingClientDetails, VerifierMatchingDetails) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(a @ AgentStatus.AgentSupplyingClientDetails(_, Some(v))) => f(a, v)
      case _                                                             => Redirect(controllers.routes.StartController.start())
    }
}

object AgentAccessController {

  val cgtReferenceKey = "cgtReference"

  val postcodeKey = "postcode"

  val countryKey = "countryCode"

  val cgtReferenceForm: Form[CgtReference] = Form(
    mapping(
      cgtReferenceKey -> CgtReference.mapping
    )(identity)(Some(_))
  )

  val postcodeForm: Form[Postcode] = Form(
    mapping(
      postcodeKey -> Postcode.mapping
    )(identity)(Some(_))
  )

  val countryForm: Form[Country] = Form(
    mapping(
      countryKey -> of(Country.formatter)
    )(identity)(Some(_))
  )

}
