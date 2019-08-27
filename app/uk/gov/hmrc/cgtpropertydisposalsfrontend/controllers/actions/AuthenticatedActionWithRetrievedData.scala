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
import uk.gov.hmrc.auth.core.retrieve.{~, Name => RetrievalName, _}
import uk.gov.hmrc.auth.core.{ConfidenceLevel, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{DateOfBirth, Email, NINO, Name}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.toFuture
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

final case class AuthenticatedRequestWithRetrievedData[A](nino: NINO,
                                                          name: Name,
                                                          dateOfBirth: DateOfBirth,
                                                          email: Option[Email],
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

  private def nonItmpNameToName(retrievalName: RetrievalName): Option[Name] =
    retrievalName match {
      case RetrievalName(Some(name), _) =>
        name.split(' ').toList.filter(_.nonEmpty) match {
          case Nil      => None // no name
          case _ :: Nil => None // only one name
          case ::(firstName, tl) =>
            tl.lastOption match {
              case Some(lastName) => Some(Name(firstName, lastName))
              case None           => None
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
          NINO(nino),
          Name(name.forename, name.surname),
          DateOfBirth(dateOfBirth),
          maybeEmail.map(Email(_)),
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
      .authorised(ConfidenceLevel.L200)
      .retrieve(
        v2.Retrievals.nino and
        v2.Retrievals.itmpName and
        v2.Retrievals.name and
        v2.Retrievals.itmpDateOfBirth and
        v2.Retrievals.email) {
        case Some(nino) ~ Some(itmpName) ~ _ ~ Some(dob) ~ maybeEmail =>
          makeAuthenticatedRequestWithRetrieval(
            itmpNameToName,
            nino,
            itmpName,
            LocalDate.parse(dob.toString),
            maybeEmail.filter(_.nonEmpty),
            request
          )
        case Some(nino) ~ None ~ Some(name) ~ Some(dob) ~ maybeEmail =>
          makeAuthenticatedRequestWithRetrieval(
            nonItmpNameToName,
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
  }

}
