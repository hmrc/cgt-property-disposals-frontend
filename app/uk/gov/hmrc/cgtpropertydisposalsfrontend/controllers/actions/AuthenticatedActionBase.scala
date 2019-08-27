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
import play.api.Configuration
import play.api.mvc.Results.Redirect
import play.api.mvc.{ActionRefiner, MessagesRequest, Request, Result}
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions, InsufficientConfidenceLevel, NoActiveSession}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

trait AuthenticatedActionBase [P[_]]
  extends ActionRefiner[MessagesRequest,P] with Logging { self  =>

  val authConnector: AuthConnector
  val config: Configuration
  val errorHandler: ErrorHandler
  val sessionStore: SessionStore
  implicit val executionContext: ExecutionContext

  def authorisedFunction[A](auth: AuthorisedFunctions, request: MessagesRequest[A]): Future[Either[Result,P[A]]]

  private val authorisedFunctions: AuthorisedFunctions = new AuthorisedFunctions {
    override def authConnector: AuthConnector = self.authConnector
  }

  private def getString(key: String): String = config.underlying.getString(key)

  private val signInUrl: String = getString("gg.url")

  private val origin: String = getString("gg.origin")

  private val selfBaseUrl: String = getString("self.url")

  private val ivUrl: String = getString("iv.url")

  private val ivOrigin: String = getString("iv.origin")

  private val (ivSuccessUrl: String, ivFailureUrl: String) = {
    val useRelativeUrls = config.underlying.getBoolean("iv.use-relative-urls")
    val (successRelativeUrl, failureRelativeUrl) =
      getString("iv.success-relative-url") -> getString("iv.failure-relative-url")

    if (useRelativeUrls)
      successRelativeUrl -> failureRelativeUrl
    else
      (selfBaseUrl + successRelativeUrl) -> (selfBaseUrl + failureRelativeUrl)
  }

  override protected def refine[A](request: MessagesRequest[A]): Future[Either[Result, P[A]]] = {
    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

    authorisedFunction[A](authorisedFunctions, request).recoverWith{
      case _: NoActiveSession =>
        Future.successful(Left(Redirect(signInUrl, Map("continue" -> Seq(selfBaseUrl + request.uri), "origin" -> Seq(origin)))))

      case _: InsufficientConfidenceLevel =>
        handleInsufficientConfidenceLevel(request, errorHandler.errorResult()(request)).map(Left(_))

    }
  }

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



