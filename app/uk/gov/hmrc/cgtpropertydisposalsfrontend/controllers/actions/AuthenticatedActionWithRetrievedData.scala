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

import java.time.LocalDate

import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc._
import play.api.mvc.Results.SeeOther
import uk.gov.hmrc.auth.core.retrieve.{~, Name => RetrievalName, _}
import uk.gov.hmrc.auth.core.{ConfidenceLevel, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{DateOfBirth, Email, NINO, Name, SAUTR, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.toFuture
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

final case class AuthenticatedRequestWithRetrievedData[A](userType: UserType,
                                                          request: MessagesRequest[A])
  extends WrappedRequest[A](request)

@Singleton
class AuthenticatedActionWithRetrievedData @Inject()(
  val authConnector: AuthConnector,
  val config: Configuration,
  val errorHandler: ErrorHandler,
  val sessionStore: SessionStore
)(implicit val executionContext: ExecutionContext)
    extends AuthenticatedActionBase[AuthenticatedRequestWithRetrievedData] {

  private def itmpNameToName(itmpName: ItmpName): Option[Name] =
    itmpName match {
      case ItmpName(Some(givenName), _, Some(familyName)) => Some(Name(givenName, familyName))
      case _                                              => None
    }

  private def ggNameToName(retrievalName: RetrievalName): Option[Name] =
    retrievalName match {
      case RetrievalName(Some(name), _) =>
        name.split(' ').toList.filter(_.nonEmpty) match {
          case Nil      => None // no name
          case firstName :: tl =>
            tl.lastOption match {
              case Some(lastName) => Some(Name(firstName, lastName))
              case None           => None // only one name
            }
        }
      case RetrievalName(None, _) => None
    }

  private def makeAuthenticatedRequestWithRetrieval[A, B](
    f: (A) => Option[Name],
    nino: String,
    name: A,
    dateOfBirth: LocalDate,
    maybeEmail: Option[String],
    request: MessagesRequest[B]
  ): Future[Either[Result, AuthenticatedRequestWithRetrievedData[B]]] =
    f(name) match {
      case Some(name) =>
        Right(AuthenticatedRequestWithRetrievedData(
          UserType.Individual(
          NINO(nino),
          Name(name.firstName, name.lastName),
          DateOfBirth(dateOfBirth),
          maybeEmail.filter(_.nonEmpty).map(Email(_))
          ),
          request
        ))
      case None => {
        logger.warn(s"Failed to retrieve name")
        Left(errorHandler.errorResult()(request))
      }
    }

  override def authorisedFunction[A](auth: AuthorisedFunctions,
                                     request: MessagesRequest[A]
                                    ): Future[Either[Result, AuthenticatedRequestWithRetrievedData[A]]] = {
    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

    auth
      .authorised()
      .retrieve(
        v2.Retrievals.confidenceLevel and
          v2.Retrievals.affinityGroup and
          v2.Retrievals.nino and
          v2.Retrievals.itmpName and
          v2.Retrievals.name and
          v2.Retrievals.itmpDateOfBirth and
          v2.Retrievals.email and
        v2.Retrievals.allEnrolments
      ) {
        case cl ~ Some(AffinityGroup.Individual) ~ maybeNino ~ _ ~ _ ~ _ ~ _ ~ _ if cl < ConfidenceLevel.L200 =>
          Right(AuthenticatedRequestWithRetrievedData(UserType.InsufficientConfidenceLevel(maybeNino.map(NINO)), request))

        case individual @ _ ~ Some(AffinityGroup.Individual) ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ =>
          individual match {
            case  _ ~ _ ~ Some(nino) ~ Some(itmpName) ~ _ ~ Some(dob) ~ maybeEmail ~ _ =>
              makeAuthenticatedRequestWithRetrieval(
                itmpNameToName,
                nino,
                itmpName,
                LocalDate.parse(dob.toString),
                maybeEmail,
                request
              )
            case _ ~ _ ~ Some(nino) ~ None ~ Some(name) ~ Some(dob) ~ maybeEmail ~ _ =>
              makeAuthenticatedRequestWithRetrieval(
                ggNameToName,
                nino,
                name,
                LocalDate.parse(dob.toString),
                maybeEmail,
                request
              )
            case other =>
              logger.warn(s"Failed to retrieve some information from the auth record: $other")
              Left(errorHandler.errorResult()(request))
          }

        case _ @  _ ~ Some(AffinityGroup.Organisation) ~ _ ~ _ ~ _ ~ _ ~ _ ~ enrolments =>
          handleOrganisation(request, enrolments)

        case _ @ _ ~ otherAffinityGroup ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ =>
          logger.warn(s"Got request for unsupported affinity group $otherAffinityGroup")
          Left(errorHandler.errorResult()(request))

      }
  }

  private def handleOrganisation[A](request: MessagesRequest[A],
                                    enrolments: Enrolments
                                   ): Either[Result, AuthenticatedRequestWithRetrievedData[A]] =
  // work out if it is an organisation or not
    enrolments.getEnrolment("HMRC-TERS-ORG") match {
      case None =>
        Left(SeeOther(routes.RegisterTrustController.registerYourTrust().url))

      case Some(trustEnrolment) =>
        trustEnrolment.getIdentifier("SAUTR")
          .fold[Either[Result, AuthenticatedRequestWithRetrievedData[A]]]{
            logger.warn(
              s"Could not find SAUTR identifier for user with trust enrolment $trustEnrolment. " +
                s"Found identifier keys [${trustEnrolment.identifiers.map(_.key).mkString(",")}]"
            )
            Left(errorHandler.errorResult()(request))
          }( id =>
            Right(AuthenticatedRequestWithRetrievedData(UserType.Trust(SAUTR(id.value)), request))
          )
    }


}
