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
import play.api.mvc.{ActionRefiner, Request, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

trait SessionDataActionBase[R[_] <: Request[_], P[_] <: Request[_]] extends ActionRefiner[R, P] with Logging {

  val sessionStore: SessionStore

  val errorHandler: ErrorHandler

  implicit val executionContext: ExecutionContext

  def sessionDataAction[A](sessionData: Option[SessionData], request: R[A]): P[A]

  override protected def refine[A](request: R[A]): Future[Either[Result, P[A]]] = {
    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

    sessionStore
      .get()
      .map(_.bimap({ e =>
        logger.warn("Could not get session data", e)
        errorHandler.errorResult()(request)
      }, sessionDataAction(_, request)))

  }

}
