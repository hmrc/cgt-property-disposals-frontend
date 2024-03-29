/*
 * Copyright 2023 HM Revenue & Customs
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
import play.api.i18n.MessagesApi
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.ExecutionContext

final case class RequestWithSessionDataAndRetrievedData[A](
  sessionData: SessionData,
  authenticatedRequest: AuthenticatedRequestWithRetrievedData[A]
) extends WrappedRequest[A](authenticatedRequest)
    with PreferredMessagesProvider {
  override def messagesApi: MessagesApi =
    authenticatedRequest.request.messagesApi
}

@Singleton
class SessionDataActionWithRetrievedData @Inject() (
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler
)(implicit
  val executionContext: ExecutionContext
) extends SessionDataActionBase[AuthenticatedRequestWithRetrievedData, RequestWithSessionDataAndRetrievedData] {

  def sessionDataAction[A](
    sessionData: Option[SessionData],
    request: AuthenticatedRequestWithRetrievedData[A]
  ): RequestWithSessionDataAndRetrievedData[A] = {
    val data: SessionData =
      sessionData.getOrElse(SessionData.empty).copy(userType = request.userType)
    RequestWithSessionDataAndRetrievedData(data, request)
  }

}
