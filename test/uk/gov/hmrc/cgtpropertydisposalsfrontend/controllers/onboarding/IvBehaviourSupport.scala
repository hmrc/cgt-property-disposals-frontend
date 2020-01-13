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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding

import com.typesafe.config.ConfigFactory
import play.api.Configuration
import play.api.mvc.Result
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.ControllerSpec

import scala.concurrent.Future

trait IvBehaviourSupport { this: ControllerSpec =>

  val (selfBaseUrl, ivUrl, ivOrigin) = ("self-base-url", "ivUrl", "ivOrigin")

  val (ivSuccessRelativeUrl, ivFailureRelativeUrl) = "/success" -> "/failure"

  def ivConfig(useRelativeUrls: Boolean): Configuration =
    Configuration(
      ConfigFactory.parseString(
        s"""
           |self.url  = "$selfBaseUrl"
           |iv {
           |  url         = "$ivUrl"
           |  origin      = "$ivOrigin"
           |  success-relative-url = "$ivSuccessRelativeUrl"
           |  failure-relative-url = "$ivFailureRelativeUrl"
           |  use-relative-urls = $useRelativeUrls
           |}
    """.stripMargin
      )
    )

  def checkIsRedirectToIv(result: Future[Result], useRelativeUrls: Boolean): Unit = {
    val expectedCompletionUrl = if (useRelativeUrls) ivSuccessRelativeUrl else s"$selfBaseUrl$ivSuccessRelativeUrl"
    val expectedFailureUrl    = if (useRelativeUrls) ivFailureRelativeUrl else s"$selfBaseUrl$ivFailureRelativeUrl"

    checkIsRedirect(
      result,
      s"$ivUrl/mdtp/uplift?" +
        s"origin=$ivOrigin&" +
        s"confidenceLevel=200&" +
        s"completionURL=${urlEncode(expectedCompletionUrl)}&" +
        s"failureURL=${urlEncode(expectedFailureUrl)}"
    )
  }

}
