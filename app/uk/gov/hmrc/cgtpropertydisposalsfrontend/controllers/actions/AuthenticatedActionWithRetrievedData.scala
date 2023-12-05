/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions

import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Credentials, ~}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.EnrolmentConfig._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Individual, NonGovernmentGatewayUser, Organisation}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{RetrievedUserType, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

final case class AuthenticatedRequestWithRetrievedData[A](
  journeyUserType: RetrievedUserType,
  userType: Option[UserType],
  request: MessagesRequest[A]
) extends WrappedRequest[A](request)

@Singleton
class AuthenticatedActionWithRetrievedData @Inject() (
  val subscriptionService: SubscriptionService,
  val authConnector: AuthConnector,
  val config: Configuration,
  val errorHandler: ErrorHandler,
  val sessionStore: SessionStore
)(implicit val executionContext: ExecutionContext)
    extends AuthenticatedActionBase[AuthenticatedRequestWithRetrievedData] {

  override def authorisedFunction[A](
    auth: AuthorisedFunctions,
    request: MessagesRequest[A]
  ): Future[Either[Result, AuthenticatedRequestWithRetrievedData[A]]] = {
    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)
    auth
      .authorised()
      .retrieve(
        Retrievals.confidenceLevel and
          Retrievals.affinityGroup and
          Retrievals.nino and
          Retrievals.saUtr and
          Retrievals.email and
          Retrievals.allEnrolments and
          Retrievals.credentials
      ) { case cl ~ affinityGroup ~ maybeNino ~ maybeSautr ~ maybeEmail ~ enrolments ~ creds =>
        withGGCredentials(creds, request) { ggCredId =>
          affinityGroup match {
            case Some(AffinityGroup.Agent) =>
              Future.successful(handleAgent(request, ggCredId, enrolments))

            case Some(ag) if ag == AffinityGroup.Individual || ag == AffinityGroup.Organisation =>
              val affinityGroup = ag match {
                case AffinityGroup.Individual   => Right(AffinityGroup.Individual)
                case AffinityGroup.Organisation => Left(AffinityGroup.Organisation)
              }
              handleIndividualOrOrganisation(
                affinityGroup,
                cl,
                maybeNino,
                maybeSautr,
                maybeEmail,
                enrolments,
                ggCredId,
                request
              )

            case other =>
              logger.warn(s"User has unsupported affinity group type $other")
              Future.successful(Left(errorHandler.errorResult(None)(request)))
          }
        }
      }
  }

  private def handleIndividualOrOrganisation[A](
    affinityGroup: Either[AffinityGroup.Organisation.type, AffinityGroup.Individual.type],
    confidenceLevel: ConfidenceLevel,
    maybeNino: Option[String],
    maybeSautr: Option[String],
    maybeEmail: Option[String],
    enrolments: Enrolments,
    ggCredId: GGCredId,
    request: MessagesRequest[A]
  )(implicit
    hc: HeaderCarrier
  ): Future[Either[Result, AuthenticatedRequestWithRetrievedData[A]]] =
    hasSubscribed(enrolments, request) map {
      case Left(errorResult) => Left(errorResult)

      case Right(Some(cgtReference)) =>
        handleSubscribedUser(cgtReference, ggCredId, affinityGroup, request)

      case Right(None) =>
        (affinityGroup, confidenceLevel, maybeNino) match {
          case (Right(AffinityGroup.Individual), cl, _) if cl < ConfidenceLevel.L200 =>
            Right(
              AuthenticatedRequestWithRetrievedData(
                RetrievedUserType.IndividualWithInsufficientConfidenceLevel(
                  maybeNino.map(NINO(_)),
                  maybeSautr.map(SAUTR(_)),
                  maybeEmail.filter(_.nonEmpty).map(Email(_)),
                  ggCredId
                ),
                Some(Individual),
                request
              )
            )

          case (Right(AffinityGroup.Individual), cl, Some(nino)) =>
            Right(
              AuthenticatedRequestWithRetrievedData(
                RetrievedUserType.Individual(
                  Right(NINO(nino)),
                  maybeEmail.filter(_.nonEmpty).map(Email(_)),
                  ggCredId
                ),
                Some(Individual),
                request
              )
            )

          case (Left(AffinityGroup.Organisation), _, _) =>
            handleOrganisation(request, enrolments, maybeEmail, ggCredId)

        }
    }

  private def hasSubscribed[A](
    enrolments: Enrolments,
    request: MessagesRequest[A]
  )(implicit
    hc: HeaderCarrier
  ): Future[Either[Result, Option[CgtReference]]] =
    enrolments.getEnrolment(CgtEnrolment.key) match {
      case Some(cgtEnrolment) =>
        cgtEnrolment.getIdentifier(CgtEnrolment.cgtReferenceIdentifier) match {
          case Some(cgtReference) =>
            Future.successful(Right(Some(CgtReference(cgtReference.value))))
          case None               =>
            logger.warn(s"CGT identifier value is missing from the enrolment")
            Future.successful(Left(errorHandler.errorResult(None)(request)))
        }
      case None               =>
        subscriptionService
          .hasFailedCgtEnrolment()
          .leftMap(_ => errorHandler.errorResult(None)(request))
          .value
    }

  private def handleSubscribedUser[A](
    cgtReference: CgtReference,
    ggCredId: GGCredId,
    affinityGroup: Either[AffinityGroup.Organisation.type, AffinityGroup.Individual.type],
    request: MessagesRequest[A]
  ): Either[Result, AuthenticatedRequestWithRetrievedData[A]] = {
    val userType = affinityGroup match {
      case Right(AffinityGroup.Individual)  => UserType.Individual
      case Left(AffinityGroup.Organisation) => UserType.Organisation
    }
    Right(
      AuthenticatedRequestWithRetrievedData(
        RetrievedUserType
          .Subscribed(CgtReference(cgtReference.value), ggCredId),
        Some(userType),
        request
      )
    )
  }

  private def withGGCredentials[A](
    credentials: Option[Credentials],
    request: MessagesRequest[A]
  )(
    f: GGCredId => Future[Either[Result, AuthenticatedRequestWithRetrievedData[A]]]
  ): Future[Either[Result, AuthenticatedRequestWithRetrievedData[A]]] =
    credentials match {
      case None =>
        logger.warn("No credentials were retrieved")
        Future.successful(Left(errorHandler.errorResult(None)(request)))

      case Some(Credentials(id, "GovernmentGateway")) =>
        f(GGCredId(id))

      case Some(Credentials(_, otherProvider)) =>
        Future.successful(
          Right(
            AuthenticatedRequestWithRetrievedData(
              RetrievedUserType.NonGovernmentGatewayRetrievedUser(
                otherProvider
              ),
              Some(NonGovernmentGatewayUser),
              request
            )
          )
        )
    }

  private def handleAgent[A](
    request: MessagesRequest[A],
    ggCredId: GGCredId,
    allEnrolments: Enrolments
  ): Either[Result, AuthenticatedRequestWithRetrievedData[A]] = {
    val arn = allEnrolments.getEnrolment(AgentsEnrolment.key) match {
      case None             => Right(None)
      case Some(enrolments) =>
        enrolments.getIdentifier(AgentsEnrolment.agentReferenceNumberIdentifier) match {
          case None    =>
            logger.warn(
              s"Agent has ${AgentsEnrolment.key} enrolment but does not have ${AgentsEnrolment.agentReferenceNumberIdentifier} identifier"
            )
            Left(errorHandler.errorResult(Some(UserType.Agent))(request))
          case Some(x) => Right(Some(AgentReferenceNumber(x.value)))
        }
    }
    arn.map(arn =>
      AuthenticatedRequestWithRetrievedData(
        RetrievedUserType.Agent(ggCredId, arn),
        Some(UserType.Agent),
        request
      )
    )
  }

  private def handleOrganisation[A](
    request: MessagesRequest[A],
    enrolments: Enrolments,
    email: Option[String],
    ggCredId: GGCredId
  ): Either[Result, AuthenticatedRequestWithRetrievedData[A]] = {
    // work out if it is an organisation or not
    val id = enrolments.getEnrolment(TrustsEnrolment.key) match {
      case None                 => Right(RetrievedUserType.OrganisationUnregisteredTrust(email.map(Email(_)), ggCredId))
      case Some(trustEnrolment) =>
        trustEnrolment.getIdentifier(TrustsEnrolment.sautrIdentifier) match {
          case None     =>
            logger.warn(
              s"Could not find SAUTR identifier for user with trust enrolment $trustEnrolment. " +
                s"Found identifier keys [${trustEnrolment.identifiers.map(_.key).mkString(",")}]"
            )
            Left(errorHandler.errorResult(Some(Organisation))(request))
          case Some(id) =>
            Right(RetrievedUserType.Trust(SAUTR(id.value), email.filter(_.nonEmpty).map(Email(_)), ggCredId))
        }
    }
    id.map(id => AuthenticatedRequestWithRetrievedData(id, Some(Organisation), request))
  }

}
