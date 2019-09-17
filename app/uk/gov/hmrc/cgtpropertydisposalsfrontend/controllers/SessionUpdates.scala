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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import cats.syntax.eq._

import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates.SessionProvider
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{RequestWithSessionData, RequestWithSessionDataAndRetrievedData, RequestWithSubscriptionReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait SessionUpdates {

  def updateSession[R](sessionStore: SessionStore, request: R)(
    update: SessionData => SessionData
  )(implicit sessionProvider: SessionProvider[R], hc: HeaderCarrier): Future[Either[Error, Unit]] = {
    val session = sessionProvider.toSession(request)
    val updatedSession = update(session)

    if(session === updatedSession){
      // don't bother updating the session if it's the same
      Future.successful(Right(()))
    } else {
      sessionStore.store(update(sessionProvider.toSession(request)))
    }

  }

}

object SessionUpdates {

  trait SessionProvider[A] {

    def toSession(a: A): SessionData

  }

  object SessionProvider {

    private def instance[A](f: A => SessionData): SessionProvider[A] =
      new SessionProvider[A] {
        override def toSession(a: A): SessionData = f(a)
      }

    implicit def requestWithSessionDataInstance[A]: SessionProvider[RequestWithSessionData[A]] =
      instance(_.sessionData.getOrElse(SessionData.empty))

    implicit def requestWithSessionDataAndRetrievedDataInstance[A]: SessionProvider[RequestWithSessionDataAndRetrievedData[A]] =
      instance(_.sessionData.getOrElse(SessionData.empty))

    implicit def requestWithSubscriptionDetails[A]: SessionProvider[RequestWithSubscriptionReady[A]] =
      instance(_.sessionData)

  }

}
