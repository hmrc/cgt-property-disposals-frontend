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
import play.api.mvc.{ActionBuilder, AnyContent, BodyParser, MessagesControllerComponents, PlayBodyParsers, Request, Result}
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions, NoActiveSession}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AuthenticatedAction @Inject() (
    authConnector: AuthConnector,
    config: Configuration,
    playBodyParsers: PlayBodyParsers
)(implicit ec: ExecutionContext)
  extends ActionBuilder[Request, AnyContent] { self =>

  val authorisedFunctions: AuthorisedFunctions = new AuthorisedFunctions {
    override def authConnector: AuthConnector = self.authConnector
  }

  val signInUrl: String = config.underlying.getString("gg.url")

  val origin: String = config.underlying.getString("gg.origin")

  val selfBaseUrl: String = config.underlying.getString("self.url")

  def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] = {
    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

    authorisedFunctions.authorised() { block(request) }
      .recover {
        case _: NoActiveSession =>
          Redirect(signInUrl, Map("continue" -> Seq(selfBaseUrl + request.uri), "origin" -> Seq(origin)))
      }
  }

  override def parser: BodyParser[AnyContent] = playBodyParsers.default

  override protected def executionContext: ExecutionContext = ec

}
