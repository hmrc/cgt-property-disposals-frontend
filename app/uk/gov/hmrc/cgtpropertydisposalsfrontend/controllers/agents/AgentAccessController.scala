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
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.http.Writeable
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions, Enrolment, InsufficientEnrolments}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.EnrolmentConfig._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.agents.AgentAccessController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.AgentStatus.{AgentSupplyingClientDetails, VerifierMatchingDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{AgentStatus, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.agents.UnsuccessfulVerifierAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.agents.audit.{AgentAccessAttempt, AgentVerifierMatchAttempt}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.agents.AgentVerifierMatchRetryStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

class AgentAccessController @Inject() (
  config: Configuration,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  authConnector: AuthConnector,
  sessionStore: SessionStore,
  errorHandler: ErrorHandler,
  agentAccessAuditService: AuditService,
  subscriptionService: SubscriptionService,
  agentVerifierMatchRetryStore: AgentVerifierMatchRetryStore,
  cc: MessagesControllerComponents,
  enterClientsCgtRefPage: views.html.agents.enter_client_cgt_ref,
  enterClientsPostcodePage: views.html.agents.enter_postcode,
  enterClientsCountryPage: views.html.agents.enter_country,
  confirmClientPage: views.html.agents.confirm_client,
  tooManyAttemptsPage: views.html.agents.too_many_attempts
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging { self =>

  private val maxVerifierNameMatchAttempts: Int =
    config.underlying.getInt("agent-verifier-match.max-retries")

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
      case AgentSupplyingClientDetails(arn, ggCredId, _) =>
        cgtReferenceForm
          .bindFromRequest()
          .fold(
            formWithErrors => BadRequest(enterClientsCgtRefPage(formWithErrors)),
            cgtReference => handleSubmittedCgtReferenceNumber(cgtReference, arn, ggCredId)
          )
    }
  }

  def enterClientsPostcode(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withVerifierMatchingDetails {
      case (_, verifierMatchingDetails, _) =>
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
      case (agentSupplyingClientDetails, verifierMatchingDetails, currentUnsuccessfulAttempts) =>
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
              verifierMatchingDetails,
              currentUnsuccessfulAttempts
            )
          case _: NonUkAddress =>
            Redirect(routes.AgentAccessController.enterClientsCountry())
        }
    }
  }

  def enterClientsCountry(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withVerifierMatchingDetails {
      case (_, verifierMatchingDetails, _) =>
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
      case (agentSupplyingClientDetails, verifierMatchingDetails, currentUnsuccessfulAttempts) =>
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
              verifierMatchingDetails,
              currentUnsuccessfulAttempts
            )
          case _: UkAddress =>
            Redirect(routes.AgentAccessController.enterClientsPostcode())
        }
    }
  }

  def tooManyVerifierMatchAttempts(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withAgentSupplyingClientDetails { _ =>
        Ok(tooManyAttemptsPage())
      }
  }

  def confirmClient(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withVerifierMatchingDetails {
      case (_, verifierMatchingDetails, _) =>
        val backLink = enterVerifierCall(verifierMatchingDetails.clientDetails)

        if (verifierMatchingDetails.correctVerifierSupplied)
          Ok(confirmClientPage(verifierMatchingDetails.clientDetails, backLink))
        else
          Redirect(backLink)
    }
  }

  def confirmClientSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withVerifierMatchingDetails {
      case (agentSupplyingClientDetails, verifierMatchingDetails, _) =>
        if (verifierMatchingDetails.correctVerifierSupplied)
          updateSession(sessionStore, request)(
            _.copy(
              journeyStatus = Some(
                Subscribed(
                  verifierMatchingDetails.clientDetails,
                  agentSupplyingClientDetails.agentGGCredId,
                  Some(agentSupplyingClientDetails.agentReferenceNumber),
                  None
                )
              )
            )
          ).map {
            case Left(e) =>
              logger.warn("Could not update session", e)
              errorHandler.errorResult()
            case Right(_) =>
              Redirect(controllers.accounts.homepage.routes.HomePageController.homepage())
          }
        else
          Redirect(enterVerifierCall(verifierMatchingDetails.clientDetails))
    }
  }

  private def enterVerifierCall(clientDetails: SubscribedDetails) =
    clientDetails.address match {
      case _: UkAddress    => routes.AgentAccessController.enterClientsPostcode()
      case _: NonUkAddress => routes.AgentAccessController.enterClientsCountry()
    }

  private def toUpperWithNoSpaces(s: String): String = s.toUpperCase.replaceAllLiterally(" ", "")

  private def handleSubmittedCgtReferenceNumber(
    cgtReference: CgtReference,
    agentReferenceNumber: AgentReferenceNumber,
    ggCredId: GGCredId
  )(implicit hc: HeaderCarrier, request: RequestWithSessionData[_]): Future[Result] =
    authorisedFunctions
      .authorised(
        Enrolment(CgtEnrolment.key)
          .withIdentifier(CgtEnrolment.cgtReferenceIdentifier, cgtReference.value)
          .withDelegatedAuthRule(CgtEnrolment.delegateAuthRule)
      ) {
        agentAccessAuditService.sendEvent(
          "agentAccessAttempt",
          AgentAccessAttempt(agentReferenceNumber, cgtReference, success = true),
          "agent-access-attempt"
        )

        val result = for {
          details <- subscriptionService.getSubscribedDetails(cgtReference)
          _ <- EitherT(
                updateSession(sessionStore, request)(
                  _.copy(
                    journeyStatus = Some(
                      AgentSupplyingClientDetails(
                        agentReferenceNumber,
                        ggCredId,
                        Some(VerifierMatchingDetails(details, false))
                      )
                    )
                  )
                )
              )
        } yield details

        result.fold(
          { e =>
            logger.warn("Could not handle submitted cgt reference", e)
            errorHandler.errorResult()
          }, { clientDetails =>
            Redirect(enterVerifierCall(clientDetails))
          }
        )
      }
      .recover {
        case _: InsufficientEnrolments =>
          agentAccessAuditService.sendEvent(
            "agentAccessAttempt",
            AgentAccessAttempt(agentReferenceNumber, cgtReference, success = false),
            "agent-access-attempt"
          )
          BadRequest(
            enterClientsCgtRefPage(
              cgtReferenceForm
                .fill(cgtReference)
                .withError(cgtReferenceKey, "error.notPermitted")
            )
          )

        case NonFatal(error) =>
          logger.warn(s"Could not do delegated auth rule check for agent: ${error.getMessage}")
          errorHandler.errorResult()
      }

  private def handleSubmittedVerifier[V, P: Writeable](
    actualVerifier: V
  )(
    matches: (V, V) => Boolean,
    form: Form[V],
    formKey: String,
    page: Form[V] => P,
    currentAgentSupplyingClientDetails: AgentSupplyingClientDetails,
    currentVerifierMatchingDetails: VerifierMatchingDetails,
    currentUnsuccessfulVerifierMatchAttempts: Option[UnsuccessfulVerifierAttempts]
  )(
    implicit
    request: RequestWithSessionData[_],
    toEither: V => Either[Country, Postcode]
  ): Future[Result] = {
    lazy val handleMatchedVerifier: Future[Result] =
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
          errorHandler.errorResult()

        case Right(_) =>
          Redirect(routes.AgentAccessController.confirmClient())
      }

    def noMatchResult(submittedVerifier: V): Result =
      BadRequest(page(form.fill(submittedVerifier).withError(formKey, "error.noMatch")))

    def handleNewUnmatchedVerifier(submittedVerifier: V, updatedUnsuccessfulAttempts: Int): Future[Result] =
      agentVerifierMatchRetryStore
        .store(
          currentAgentSupplyingClientDetails.agentGGCredId,
          currentVerifierMatchingDetails.clientDetails.cgtReference,
          UnsuccessfulVerifierAttempts(updatedUnsuccessfulAttempts, toEither(submittedVerifier))
        )
        .map {
          case Left(e) =>
            logger.warn("Could not update agent verifier match retry store ", e)
            errorHandler.errorResult()

          case Right(_) =>
            if (updatedUnsuccessfulAttempts >= maxVerifierNameMatchAttempts)
              Redirect(routes.AgentAccessController.tooManyVerifierMatchAttempts())
            else
              noMatchResult(submittedVerifier)
        }

    def sendAuditEvent(submittedVerifier: V, success: Boolean, numberOfUnsuccessfulAttempts: Int): Unit =
      agentAccessAuditService.sendEvent(
        "agentVerifierMatchAttempt",
        AgentVerifierMatchAttempt(
          currentAgentSupplyingClientDetails.agentReferenceNumber,
          currentVerifierMatchingDetails.clientDetails.cgtReference,
          numberOfUnsuccessfulAttempts,
          maxVerifierNameMatchAttempts,
          toEither(submittedVerifier),
          success
        ),
        "agent-verifier-match-attempt"
      )

    form
      .bindFromRequest()
      .fold(
        formWithErrors => BadRequest(page(formWithErrors)),
        submittedVerifier => {
          val currentNumberOfUnsuccessfulAttempts =
            currentUnsuccessfulVerifierMatchAttempts.map(_.unsuccessfulAttempts).getOrElse(0)

          if (matches(submittedVerifier, actualVerifier)) {
            sendAuditEvent(submittedVerifier, success = true, currentNumberOfUnsuccessfulAttempts)
            handleMatchedVerifier
          }
          // don't increase the unsuccessful attempt count if the same incorrect value has been submitted
          else if (currentUnsuccessfulVerifierMatchAttempts
                     .map(_.lastDetailsTried)
                     .contains(toEither(submittedVerifier))) {
            sendAuditEvent(submittedVerifier, success = false, currentNumberOfUnsuccessfulAttempts)
            noMatchResult(submittedVerifier)
          } else {
            val updatedNumberOfUnsuccessfulAttempts = currentNumberOfUnsuccessfulAttempts + 1
            sendAuditEvent(submittedVerifier, success = false, updatedNumberOfUnsuccessfulAttempts)
            handleNewUnmatchedVerifier(submittedVerifier, updatedNumberOfUnsuccessfulAttempts)
          }
        }
      )

  }

  private def withAgentSupplyingClientDetails(
    f: AgentSupplyingClientDetails => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(a: AgentStatus.AgentSupplyingClientDetails) => f(a)
      case _                                                => Redirect(controllers.routes.StartController.start())
    }

  private def withVerifierMatchingDetails(
    f: (AgentSupplyingClientDetails, VerifierMatchingDetails, Option[UnsuccessfulVerifierAttempts]) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(a @ AgentStatus.AgentSupplyingClientDetails(_, _, Some(v))) =>
        agentVerifierMatchRetryStore
          .get(a.agentGGCredId, v.clientDetails.cgtReference)
          .flatMap {
            case Left(e) =>
              logger.warn("Could not get agent verifier match details", e)
              errorHandler.errorResult()

            case Right(Some(unsuccessfulVerifierAttempts))
                if unsuccessfulVerifierAttempts.unsuccessfulAttempts >= maxVerifierNameMatchAttempts =>
              Redirect(routes.AgentAccessController.tooManyVerifierMatchAttempts())

            case Right(maybeUnsuccessfulVerifierAttempts) =>
              f(a, v, maybeUnsuccessfulVerifierAttempts)
          }
      case _ => Redirect(controllers.routes.StartController.start())
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

  implicit def toLeft[A, B]: A => Either[A, B] = Left(_)

  implicit def toRight[A, B]: B => Either[A, B] = Right(_)

}
