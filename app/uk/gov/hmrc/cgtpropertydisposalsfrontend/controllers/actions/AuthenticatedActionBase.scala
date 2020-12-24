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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions

import play.api.Configuration
import play.api.mvc.Results.Redirect
import play.api.mvc.{ActionRefiner, MessagesRequest, Result}
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions, NoActiveSession}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging

import scala.concurrent.{ExecutionContext, Future}

trait AuthenticatedActionBase[P[_]] extends ActionRefiner[MessagesRequest, P] with Logging { self =>

  val authConnector: AuthConnector
  val config: Configuration
  val errorHandler: ErrorHandler
  val sessionStore: SessionStore
  implicit val executionContext: ExecutionContext

  def authorisedFunction[A](
    auth: AuthorisedFunctions,
    request: MessagesRequest[A]
  ): Future[Either[Result, P[A]]]

  private val authorisedFunctions: AuthorisedFunctions =
    new AuthorisedFunctions {
      override def authConnector: AuthConnector = self.authConnector
    }

  private def getString(key: String): String = config.underlying.getString(key)

  private val signInUrl: String = getString("bas-gateway.url")

  private val origin: String = getString("gg.origin")

  private val selfBaseUrl: String = getString("self.url")

  override protected def refine[A](
    request: MessagesRequest[A]
  ): Future[Either[Result, P[A]]] =
    authorisedFunction[A](authorisedFunctions, request).recoverWith { case _: NoActiveSession =>
      Future.successful(
        Left(
          Redirect(
            signInUrl,
            Map(
              "continue" -> Seq(selfBaseUrl + request.uri),
              "origin"   -> Seq(origin)
            )
          )
        )
      )
    }

}
