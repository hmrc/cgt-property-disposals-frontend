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

import org.joda.time.LocalDate
import play.api.Configuration
import uk.gov.hmrc.auth.core.authorise.{EmptyPredicate, Predicate}
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{EmptyRetrieval, ItmpName, Retrieval, ~}
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector, ConfidenceLevel, Enrolment, EnrolmentIdentifier, Enrolments}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Name, SAUTR}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait AuthSupport {
  this: ControllerSpec with SessionSupport =>

  val mockAuthConnector: AuthConnector = mock[AuthConnector]

  lazy val testAuthenticatedAction = new AuthenticatedActionWithRetrievedData(
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

  def mockAuthWithNoRetrievals(): Unit =
    mockAuth(EmptyPredicate, EmptyRetrieval)(Future.successful(()))

  val expectedRetrievals =
    Retrievals.confidenceLevel and Retrievals.affinityGroup and Retrievals.nino and
      Retrievals.itmpName and Retrievals.name and Retrievals.itmpDateOfBirth and
      Retrievals.email and Retrievals.allEnrolments

  def mockAuthWithAllRetrievals(
    retrievedConfidenceLevel: ConfidenceLevel,
    retrievedAffinityGroup: Option[AffinityGroup],
    retrievedNino: Option[String],
    retrievedName: Option[Name],
    retrievedDateOfBirth: Option[LocalDate],
    retrievedEmail: Option[String],
    retrievedEnrolments: Set[Enrolment]): Unit =
    mockAuth(EmptyPredicate, expectedRetrievals)(
      Future successful (
        new ~(retrievedConfidenceLevel, retrievedAffinityGroup) and
          retrievedNino and
          retrievedName.map(name => ItmpName(Some(name.firstName), None, Some(name.lastName))) and
          None and
          retrievedDateOfBirth and
          retrievedEmail and
          Enrolments(retrievedEnrolments)
        )
    )

  def mockAuthWithCl200AndWithAllIndividualRetrievals(
                                             retrievedNino: String,
                                             retrievedName: Name,
                                             retrievedDateOfBirth: LocalDate,
                                             retrievedEmail: Option[String]
                                           ): Unit =
    mockAuthWithAllRetrievals(
      ConfidenceLevel.L200,
      Some(AffinityGroup.Individual),
      Some(retrievedNino),
      Some(retrievedName),
      Some(retrievedDateOfBirth),
      retrievedEmail,
      Set.empty
    )

  def mockAuthWithAllTrustRetrievals(sautr: SAUTR, email: Option[String]): Unit =
    mockAuthWithAllRetrievals(
      ConfidenceLevel.L50,
      Some(AffinityGroup.Organisation),
      None,
      None,
      None,
      email,
      Set(Enrolment("HMRC-TERS-ORG", Seq(EnrolmentIdentifier("SAUTR", sautr.value)), ""))
    )

}
