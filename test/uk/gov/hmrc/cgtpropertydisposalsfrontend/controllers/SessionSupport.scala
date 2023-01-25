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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait SessionSupport { this: MockFactory =>

  val mockSessionStore: SessionStore = mock[SessionStore]

  def mockGetSession(result: Either[Error, Option[SessionData]]) =
    (mockSessionStore
      .get()(_: HeaderCarrier))
      .expects(*)
      .returning(Future.successful(result))

  def mockGetSession(session: SessionData) =
    (mockSessionStore
      .get()(_: HeaderCarrier))
      .expects(*)
      .returning(Future.successful(Right(Some(session))))

  def mockStoreSession(session: SessionData)(result: Either[Error, Unit]) =
    (mockSessionStore
      .store(_: SessionData)(_: HeaderCarrier))
      .expects(session, *)
      .returning(Future.successful(result))

}
