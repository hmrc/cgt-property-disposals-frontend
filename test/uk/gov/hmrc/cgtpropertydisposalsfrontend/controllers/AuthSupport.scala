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

import org.joda.time.{LocalDate => JodaLocalDate}
import play.api.Configuration
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Name, Retrieval, ~}
import uk.gov.hmrc.auth.core.{AuthConnector, ConfidenceLevel}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.AuthenticatedAction
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait AuthSupport { this: ControllerSpec with SessionSupport =>

  val mockAuthConnector: AuthConnector = mock[AuthConnector]

  lazy val testAuthenticatedAction = new AuthenticatedAction(
    mockAuthConnector,
    instanceOf[Configuration],
    instanceOf[ErrorHandler],
    mockSessionStore
  )(instanceOf[ExecutionContext])

  def mockAuth[R](predicate: Predicate, retrieval: Retrieval[R])(result: Future[R]): Unit =
    (mockAuthConnector
      .authorise(_: Predicate, _: Retrieval[R])(_: HeaderCarrier, _: ExecutionContext))
      .expects(predicate, retrieval, *, *)
      .returning(result)

  def mockAuthWithCl200AndRetrievedAllRetrievals(
    retrievedNino: String,
    retrievedName: uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Name,
    retrievedDateOfBirth: JodaLocalDate
  ): Unit =
    mockAuth(ConfidenceLevel.L200, Retrievals.nino and Retrievals.name and Retrievals.dateOfBirth)(
      Future.successful(
        new ~(
          new ~(Some(retrievedNino), Some(Name(Some(retrievedName.forename), Some(retrievedName.surname)))),
          Some(retrievedDateOfBirth)
        )
      )
    )

}
