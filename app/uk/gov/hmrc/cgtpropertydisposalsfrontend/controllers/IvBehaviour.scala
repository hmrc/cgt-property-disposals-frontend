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

import play.api.Configuration
import play.api.mvc.Result
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler

trait IvBehaviour {

  val config: Configuration

  val errorHandler: ErrorHandler

  private def getString(key: String): String = config.underlying.getString(key)

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

  val redirectToIv: Result =
    Redirect(
      s"$ivUrl/mdtp/uplift",
      Map(
        "origin"          -> Seq(ivOrigin),
        "confidenceLevel" -> Seq("200"),
        "completionURL"   -> Seq(ivSuccessUrl),
        "failureURL"      -> Seq(ivFailureUrl)
      )
    )

}
