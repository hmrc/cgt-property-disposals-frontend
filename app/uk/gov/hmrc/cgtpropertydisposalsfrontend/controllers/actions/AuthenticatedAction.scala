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

import cats.syntax.either._
import com.google.inject.{Inject, Singleton}
import org.joda.time.{LocalDate => JodaLocalDate}
import play.api.Configuration
import play.api.mvc.Results._
import play.api.mvc._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{~, _}
import uk.gov.hmrc.auth.core.{ConfidenceLevel, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{DateOfBirth, NINO, Name, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.auth.core.retrieve.{Name => RetrievalName}
import scala.concurrent.{ExecutionContext, Future}

final case class AuthenticatedRequest[A](nino: NINO, name: Name, dateOfBirth: DateOfBirth, request: MessagesRequest[A])
    extends WrappedRequest[A](request)

@Singleton
class AuthenticatedAction @Inject()(
  authConnector: AuthConnector,
  config: Configuration,
  errorHandler: ErrorHandler,
  sessionStore: SessionStore
)(implicit ec: ExecutionContext)
    extends ActionRefiner[MessagesRequest, AuthenticatedRequest]
    with Logging {
  self =>

  val authorisedFunctions: AuthorisedFunctions = new AuthorisedFunctions {
    override def authConnector: AuthConnector = self.authConnector
  }

  private def getString(key: String): String = config.underlying.getString(key)

  val signInUrl: String = getString("gg.url")

  val origin: String = getString("gg.origin")

  val selfBaseUrl: String = getString("self.url")

  val ivUrl: String = getString("iv.url")

  val ivOrigin: String = getString("iv.origin")

  val (ivSuccessUrl: String, ivFailureUrl: String) = {
    val useRelativeUrls = config.underlying.getBoolean("iv.use-relative-urls")
    val (successRelativeUrl, failureRelativeUrl) =
      getString("iv.success-relative-url") -> getString("iv.failure-relative-url")

    if (useRelativeUrls)
      successRelativeUrl -> failureRelativeUrl
    else
      (selfBaseUrl + successRelativeUrl) -> (selfBaseUrl + failureRelativeUrl)
  }

  private def makeAuthenticatedRequestWithItmpRetrieval[A](
    nino: String,
    name: ItmpName,
    dateOfBirth: LocalDate,
    request: MessagesRequest[A]
  ): Future[Either[Result, AuthenticatedRequest[A]]] =
    name match {
      case ItmpName(Some(givenName), _, Some(familyName)) =>
        Right(AuthenticatedRequest(NINO(nino), Name(givenName, familyName), DateOfBirth(dateOfBirth), request))
      case other =>
        logger.warn(s"Failed to retrieve the complete name for this user: $other")
        Left(errorHandler.errorResult()(request))
    }

  private def makeAuthenticatedRequestWithRetrieval[A](
    nino: String,
    name: RetrievalName,
    dateOfBirth: LocalDate,
    request: MessagesRequest[A]
  ): Future[Either[Result, AuthenticatedRequest[A]]] =
    name match {
      case RetrievalName(Some(forename), Some(surname)) =>
        Right(AuthenticatedRequest(NINO(nino), Name(forename, surname), DateOfBirth(dateOfBirth), request))
      case other =>
        logger.warn(s"Failed to retrieve the complete name for this user: $other")
        Left(errorHandler.errorResult()(request))
    }

  @SuppressWarnings(Array("org.wartremover.warts.Product", "org.wartremover.warts.Serializable"))
  override protected def refine[A](request: MessagesRequest[A]): Future[Either[Result, AuthenticatedRequest[A]]] = {
    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

    authorisedFunctions
      .authorised(ConfidenceLevel.L200)
      .retrieve(v2.Retrievals.nino and v2.Retrievals.itmpName and v2.Retrievals.name and Retrievals.itmpDateOfBirth) {
        case Some(nino) ~ Some(itmpName) ~ _ ~ Some(dob) =>
          makeAuthenticatedRequestWithItmpRetrieval(nino, itmpName, LocalDate.parse(dob.toString), request)
        case Some(nino) ~ None ~ Some(name) ~ Some(dob) =>
          makeAuthenticatedRequestWithRetrieval(nino, name, LocalDate.parse(dob.toString), request)
        case other =>
          logger.warn(s"Failed to retrieve some information from the auth record: $other")
          Left(errorHandler.errorResult()(request))
      }
      .recoverWith {
        case _: NoActiveSession =>
          Left(Redirect(signInUrl, Map("continue" -> Seq(selfBaseUrl + request.uri), "origin" -> Seq(origin))))
        case _: InsufficientConfidenceLevel =>
          handleInsufficientConfidenceLevel(request, errorHandler.errorResult()(request)).map(Left(_))
      }
  }

  override protected def executionContext: ExecutionContext = ec

  private def handleInsufficientConfidenceLevel(request: MessagesRequest[_], errorResponse: Result)(
    implicit hc: HeaderCarrier
  ): Future[Result] =
    sessionStore
      .store(SessionData.empty.copy(ivContinueUrl = Some(selfBaseUrl + request.uri)))
      .map {
        _.bimap(
          { e =>
            logger.warn("Could not store IV continue url", e)
            errorResponse
          },
          _ =>
            Redirect(
              s"$ivUrl/mdtp/uplift",
              Map(
                "origin"          -> Seq(ivOrigin),
                "confidenceLevel" -> Seq("200"),
                "completionURL"   -> Seq(ivSuccessUrl),
                "failureURL"      -> Seq(ivFailureUrl)
              )
          )
        ).merge
      }

}
