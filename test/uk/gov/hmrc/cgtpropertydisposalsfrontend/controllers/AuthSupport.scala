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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import play.api.Configuration
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.{EmptyPredicate, Predicate}
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Credentials, EmptyRetrieval, Retrieval, ~}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{CgtEnrolment, ErrorHandler}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.AuthenticatedActionWithRetrievedData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.SAUTR
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.{OnboardingAuditService, SubscriptionService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait AuthSupport {
  this: ControllerSpec with SessionSupport =>

  val mockAuthConnector: AuthConnector                     = mock[AuthConnector]
  val mockSubscriptionService: SubscriptionService         = mock[SubscriptionService]
  val mockSubscriptionAuditService: OnboardingAuditService = mock[OnboardingAuditService]

  lazy val testAuthenticatedAction = new AuthenticatedActionWithRetrievedData(
    mockSubscriptionService,
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
      Retrievals.saUtr and Retrievals.email and Retrievals.allEnrolments and Retrievals.credentials

  def mockAuthWithAllRetrievals(
    retrievedConfidenceLevel: ConfidenceLevel,
    retrievedAffinityGroup: Option[AffinityGroup],
    retrievedNino: Option[String],
    retrievedSautr: Option[String],
    retrievedEmail: Option[String],
    retrievedEnrolments: Set[Enrolment],
    retrievedCredentials: Option[Credentials]
  ): Unit =
    mockAuth(EmptyPredicate, expectedRetrievals)(
      Future successful (
        new ~(retrievedConfidenceLevel, retrievedAffinityGroup) and
          retrievedNino and
          retrievedSautr and
          retrievedEmail and
          Enrolments(retrievedEnrolments) and
          retrievedCredentials
      )
    )

  def mockAuthWithCl200AndWithAllIndividualRetrievals(
    retrievedNino: String,
    retrievedEmail: Option[String],
    retrievedCredentials: Credentials
  ): Unit =
    mockAuthWithAllRetrievals(
      ConfidenceLevel.L200,
      Some(AffinityGroup.Individual),
      Some(retrievedNino),
      None,
      retrievedEmail,
      Set.empty,
      Some(retrievedCredentials)
    )

  def mockAuthWithAllTrustRetrievals(sautr: SAUTR, email: Option[String], retrievedCredentials: Credentials): Unit =
    mockAuthWithAllRetrievals(
      ConfidenceLevel.L50,
      Some(AffinityGroup.Organisation),
      None,
      None,
      email,
      Set(Enrolment("HMRC-TERS-ORG", Seq(EnrolmentIdentifier("SAUTR", sautr.value)), "")),
      Some(retrievedCredentials)
    )

  def mockAuthWithCgtEnrolmentRetrievals(): Unit =
    mockAuthWithAllRetrievals(
      ConfidenceLevel.L200,
      Some(AffinityGroup.Individual),
      None,
      None,
      None,
      Set(Enrolment(CgtEnrolment.enrolmentKey, Seq(EnrolmentIdentifier(CgtEnrolment.enrolmentIdentifier, "XCGTP123456789")), "")),
      Some(Credentials("gg-cred-id", "GovernmentGateway"))
    )

}
