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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions

import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Credentials, ~}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Individual, NonGovernmentGatewayUser, Organisation}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{JourneyUserType, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

final case class AuthenticatedRequestWithRetrievedData[A](
  journeyUserType: JourneyUserType,
  userType: Option[UserType],
  request: MessagesRequest[A]
) extends WrappedRequest[A](request)

@Singleton
class AuthenticatedActionWithRetrievedData @Inject()(
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

    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

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
      ) {
        case retrievedData @ _ ~ _ ~ _ ~ _ ~ _ ~ allEnrolments ~ creds =>
          withGGCredentials(creds, request) { ggCredId =>
            hasSubscribed(allEnrolments, request) map {
              case Left(errorResult) => Left(errorResult)
              case Right(Some(cgtReference)) =>
                Right(
                  AuthenticatedRequestWithRetrievedData(
                    JourneyUserType.Subscribed(CgtReference(cgtReference.value), ggCredId),
                    None,
                    request
                  )
                )

              case Right(None) =>
                val result = retrievedData match {
                  case cl ~ Some(AffinityGroup.Individual) ~ maybeNino ~ maybeSautr ~ maybeEmail ~ _ ~ _
                      if cl < ConfidenceLevel.L200 =>
                    Right(
                      AuthenticatedRequestWithRetrievedData(
                        JourneyUserType.IndividualWithInsufficientConfidenceLevel(
                          maybeNino.map(NINO(_)),
                          maybeSautr.map(SAUTR(_)),
                          maybeEmail.filter(_.nonEmpty).map(Email(_)),
                          ggCredId
                        ),
                        Some(Individual),
                        request
                      )
                    )

                  case individual @ _ ~ Some(AffinityGroup.Individual) ~ _ ~ _ ~ _ ~ _ ~ _ =>
                    individual match {
                      case _ ~ _ ~ Some(nino) ~ _ ~ maybeEmail ~ _ ~ _ =>
                        Right(
                          AuthenticatedRequestWithRetrievedData(
                            JourneyUserType.Individual(
                              Right(NINO(nino)),
                              maybeEmail.filter(_.nonEmpty).map(Email(_)),
                              ggCredId
                            ),
                            Some(Individual),
                            request
                          )
                        )
                    }

                  case _ @_ ~ Some(AffinityGroup.Organisation) ~ _ ~ _ ~ maybeEmail ~ enrolments ~ _ =>
                    handleOrganisation(request, enrolments, maybeEmail, ggCredId)

                  case _ @_ ~ otherAffinityGroup ~ _ ~ _ ~ _ ~ _ ~ _ =>
                    logger.warn(s"Got request for unsupported affinity group $otherAffinityGroup")
                    Left(errorHandler.errorResult()(request))
                }
                result
            }
          }
      }
  }

  private def hasSubscribed[A](enrolments: Enrolments, request: MessagesRequest[A])(
    implicit hc: HeaderCarrier
  ): Future[Either[Result, Option[CgtReference]]] =
    enrolments.getEnrolment("HMRC-CGT-PD") match {
      case Some(cgtEnrolment) =>
        cgtEnrolment.getIdentifier("CGTPDRef") match {
          case Some(cgtReference) => Future.successful(Right(Some(CgtReference(cgtReference.value))))
          case None =>
            logger.warn(s"CGT identifier value is missing from the enrolment")
            Future.successful(Left(errorHandler.errorResult()(request)))
        }
      case None =>
        subscriptionService
          .hasSubscription()
          .leftMap(_ => errorHandler.errorResult()(request))
          .value
    }

  private def withGGCredentials[A](credentials: Option[Credentials], request: MessagesRequest[A])(
    f: GGCredId => Future[Either[Result, AuthenticatedRequestWithRetrievedData[A]]]
  ): Future[Either[Result, AuthenticatedRequestWithRetrievedData[A]]] =
    credentials match {
      case None =>
        logger.warn("No credentials were retrieved")
        Future.successful(Left(errorHandler.errorResult()(request)))

      case Some(Credentials(id, "GovernmentGateway")) =>
        f(GGCredId(id))

      case Some(Credentials(_, otherProvider)) =>
        Future.successful(
          Right(
            AuthenticatedRequestWithRetrievedData(
              JourneyUserType.NonGovernmentGatewayJourneyUser(otherProvider),
              Some(NonGovernmentGatewayUser),
              request
            )
          )
        )
    }

  private def handleOrganisation[A](
    request: MessagesRequest[A],
    enrolments: Enrolments,
    email: Option[String],
    ggCredId: GGCredId
  ): Either[Result, AuthenticatedRequestWithRetrievedData[A]] =
    // work out if it is an organisation or not
    enrolments.getEnrolment("HMRC-TERS-ORG") match {
      case None =>
        Right(
          AuthenticatedRequestWithRetrievedData(
            JourneyUserType.OrganisationUnregisteredTrust(email.map(Email(_)), ggCredId),
            Some(Organisation),
            request
          )
        )

      case Some(trustEnrolment) =>
        trustEnrolment
          .getIdentifier("SAUTR")
          .fold[Either[Result, AuthenticatedRequestWithRetrievedData[A]]] {
            logger.warn(
              s"Could not find SAUTR identifier for user with trust enrolment $trustEnrolment. " +
                s"Found identifier keys [${trustEnrolment.identifiers.map(_.key).mkString(",")}]"
            )
            Left(errorHandler.errorResult()(request))
          }(
            id =>
              Right(
                AuthenticatedRequestWithRetrievedData(
                  JourneyUserType.Trust(
                    SAUTR(id.value),
                    email.filter(_.nonEmpty).map(Email(_)),
                    ggCredId
                  ),
                  Some(Organisation),
                  request
                )
              )
          )
    }

}
