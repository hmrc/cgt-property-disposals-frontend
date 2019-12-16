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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.filters

import akka.stream.Materializer
import com.google.inject.Inject
import play.api.Configuration
import play.api.mvc.Results.Redirect
import play.api.mvc.{Call, Filter, RequestHeader, Result}
import uk.gov.hmrc.auth.otac.{OtacAuthConnector, OtacAuthorisationFunctions}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.{HeaderCarrier, SessionKeys}
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

class EmailWhitelistingFilter @Inject()(
  val mat: Materializer,
  val authConnector: OtacAuthConnector,
  config: Configuration,
  errorHandler: ErrorHandler
)(implicit ec: ExecutionContext)
    extends Filter
    with OtacAuthorisationFunctions
    with Logging {

  val whitelistingEnabled: Boolean = config.underlying.getBoolean("passcodeAuthentication.enabled")

  val otacUrl: String = config.underlying.getString("otac.url")

  val selfBaseUrl: String = config.underlying.getString("self.url")

  lazy val thereIsAProblemCall: Call =
    uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.EmailWhitelistingController.thereIsAProblem()

  lazy val excludedPaths: Seq[Call] = Seq(
    Call("GET", uk.gov.hmrc.play.health.routes.HealthController.ping().url),
    Call("GET", thereIsAProblemCall.url)
  )

  override def apply(f: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] =
    if (whitelistingEnabled && !excludedPaths.contains(Call(rh.method, rh.uri))) {
      rh.session
        .get(SessionKeys.otacToken)
        .orElse(rh.queryString.get("p").flatMap(_.headOption))
        .orElse(rh.cookies.get("whitelisting").map(_.value))
        .map { token =>
          implicit val hc: HeaderCarrier =
            HeaderCarrierConverter.fromHeadersAndSession(rh.headers, Some(rh.session))
          withVerifiedPasscode[Result]("capital-gains-tax-property-disposals", Some(token)) {
            f(rh)
          }.recover {
            case NonFatal(_) =>
              Redirect(s"$otacUrl?p=$token")
                .addingToSession(
                  SessionKeys.redirect  -> s"$selfBaseUrl${routes.StartController.start()}?p=$token",
                  SessionKeys.otacToken -> token
                )(rh)

          }
        }
        .getOrElse {
          logger.warn("Could not find OTAC token for email whitelisting in request")
          Future.successful(Redirect(thereIsAProblemCall))
        }
    } else {
      f(rh)
    }
}
