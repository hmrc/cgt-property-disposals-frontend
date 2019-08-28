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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import cats.syntax.either._
import play.api.Configuration
import play.api.mvc.{Request, Result}
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.RequestWithSessionDataAndRetrievedData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait IvBehaviour { this: Logging =>

  val config: Configuration

  val sessionStore: SessionStore

  val errorHandler: ErrorHandler

  private def getString(key: String): String = config.underlying.getString(key)

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


  def updateSessionAndRedirectToIV[R[_] <: Request[_]](request: R[_])(
    implicit hc: HeaderCarrier, ec: ExecutionContext
  ): Future[Result] =
    sessionStore
      .store(SessionData.empty.copy(ivContinueUrl = Some(selfBaseUrl + request.uri)))
      .map {
        _.bimap(
          { e =>
            logger.warn("Could not store IV continue url", e)
            errorHandler.errorResult()(request)
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
