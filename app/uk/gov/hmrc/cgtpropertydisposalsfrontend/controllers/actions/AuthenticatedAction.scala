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

import cats.syntax.either._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.Results.{InternalServerError, _}
import play.api.mvc._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{NINO, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

final case class AuthenticatedRequest[A](nino: NINO, request: MessagesRequest[A]) extends WrappedRequest[A](request)

@Singleton
class AuthenticatedAction @Inject() (
    authConnector: AuthConnector,
    config: Configuration,
    errorHandler: ErrorHandler,
    sessionStore: SessionStore
)(implicit ec: ExecutionContext)
  extends ActionRefiner[MessagesRequest, AuthenticatedRequest] with Logging { self =>

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

  @SuppressWarnings(Array("org.wartremover.warts.Product", "org.wartremover.warts.Serializable"))
  override protected def refine[A](request: MessagesRequest[A]): Future[Either[Result, AuthenticatedRequest[A]]] = {
    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

    authorisedFunctions.authorised(ConfidenceLevel.L200).retrieve(Retrievals.nino) {
      case None =>
        logger.warn("Could not find NINO")
        Left(errorHandler.errorResult()(request))

      case Some(nino) =>
        Right(AuthenticatedRequest(NINO(nino), request))
    }.recoverWith {
      case _: NoActiveSession =>
        Left(Redirect(signInUrl, Map("continue" -> Seq(selfBaseUrl + request.uri), "origin" -> Seq(origin))))

      case _: InsufficientConfidenceLevel =>
        handleInsufficientConfidenceLevel(request, errorHandler.errorResult()(request)).map(Left(_))

    }
  }

  override protected def executionContext: ExecutionContext = ec

  private def handleInsufficientConfidenceLevel(request: MessagesRequest[_], errorResponse: Result)(implicit hc: HeaderCarrier): Future[Result] =
    sessionStore.store(SessionData.empty.copy(ivContinueUrl = Some(selfBaseUrl + request.uri)))
      .map {
        _.bimap(
          { e =>
            logger.warn("Could not store IV continue url", e)
            errorResponse
          }, _ =>
            Redirect(
              s"$ivUrl/mdtp/uplift",
              Map(
                "origin" -> Seq(ivOrigin),
                "confidenceLevel" -> Seq("200"),
                "completionURL" -> Seq(ivSuccessUrl),
                "failureURL" -> Seq(ivFailureUrl)
              )
            )
        ).merge
      }

}

