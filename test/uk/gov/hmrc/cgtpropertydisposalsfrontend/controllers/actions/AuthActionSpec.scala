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

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import play.api.Configuration
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.Retrieval
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.SubscriptionService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait AuthActionSpec { this: MockFactory =>

  val mockAuthConnector: AuthConnector             = mock[AuthConnector]
  val mockSubscriptionService: SubscriptionService = mock[SubscriptionService]

  val (signInUrl, origin, selfBaseUrl) = ("sign-in", "origin", "self-base-url")

  val config: Configuration =
    Configuration(
      ConfigFactory.parseString(
        s"""
           |gg.url    = "$signInUrl"
           |gg.origin = "$origin"
           |self.url  = "$selfBaseUrl"
    """.stripMargin
      )
    )

  def mockAuth[R](predicate: Predicate, retrieval: Retrieval[R])(result: Future[R]): Unit =
    (mockAuthConnector
      .authorise(_: Predicate, _: Retrieval[R])(_: HeaderCarrier, _: ExecutionContext))
      .expects(predicate, retrieval, *, *)
      .returning(result)

}
