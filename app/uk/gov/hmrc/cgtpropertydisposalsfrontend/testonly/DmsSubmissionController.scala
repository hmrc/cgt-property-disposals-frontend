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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.testonly

import cats.instances.future._
import cats.instances.int._
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class DmsSubmissionController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  cgtPropertyDisposalsConnector: CGTPropertyDisposalsConnector,
  cc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  private def withSubscribedUser(request: RequestWithSessionData[_])(
    f: (SessionData, Subscribed) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((s: SessionData, r: Subscribed)) =>
        f(s, r)
      case _                                     =>
        Future.successful(
          SeeOther(
            uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController
              .start()
              .url
          )
        )
    }

  def testSubmitToDms: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withSubscribedUser(request) { (_, subscribed) =>
        val result =
          cgtPropertyDisposalsConnector
            .testSubmitToDms(subscribed.subscribedDetails.cgtReference)
            .subflatMap { response =>
              if (response.status === OK)
                Right(())
              else
                Left(
                  Error(
                    s"Call to get subscribed details came back with status ${response.status}"
                  )
                )
            }
        result.fold(
          _ => BadRequest("failed submission to dms"),
          _ => Ok("successful submission to dms")
        )
      }
    }

}
