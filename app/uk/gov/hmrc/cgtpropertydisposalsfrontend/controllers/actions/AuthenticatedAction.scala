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

import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.Results._
import play.api.mvc._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions, NoActiveSession}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.NINO
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

final case class AuthenticatedRequest[A](nino: NINO, request: MessagesRequest[A]) extends WrappedRequest[A](request)

@Singleton
class AuthenticatedAction @Inject() (
    authConnector: AuthConnector,
    config: Configuration,
    errorHandler: ErrorHandler,
    playBodyParsers: PlayBodyParsers
)(implicit ec: ExecutionContext)
  extends ActionRefiner[MessagesRequest, AuthenticatedRequest] with Logging { self =>

  val authorisedFunctions: AuthorisedFunctions = new AuthorisedFunctions {
    override def authConnector: AuthConnector = self.authConnector
  }

  val signInUrl: String = config.underlying.getString("gg.url")

  val origin: String = config.underlying.getString("gg.origin")

  val selfBaseUrl: String = config.underlying.getString("self.url")

  @SuppressWarnings(Array("org.wartremover.warts.Product", "org.wartremover.warts.Serializable"))
  override protected def refine[A](request: MessagesRequest[A]): Future[Either[Result, AuthenticatedRequest[A]]] = {
    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

    authorisedFunctions.authorised().retrieve(Retrievals.nino) {
      case None =>
        logger.warn("Could not find NINO")
        Future.successful(Left(InternalServerError(errorHandler.internalServerErrorTemplate(request))))
      case Some(nino) =>
        Future.successful(Right(AuthenticatedRequest(NINO(nino), request)))
    }.recover {
      case _: NoActiveSession =>
        Left(Redirect(signInUrl, Map("continue" -> Seq(selfBaseUrl + request.uri), "origin" -> Seq(origin))))
    }
  }

  override protected def executionContext: ExecutionContext = ec

}

