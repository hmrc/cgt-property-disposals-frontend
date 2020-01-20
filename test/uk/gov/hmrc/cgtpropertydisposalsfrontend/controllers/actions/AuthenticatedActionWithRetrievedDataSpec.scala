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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions

import cats.data.EitherT
import julienrf.json.derived
import org.scalamock.scalatest.MockFactory
import play.api.i18n.MessagesApi
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.Results.Ok
import play.api.mvc.{MessagesRequest, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.ConfidenceLevel.{L0, L100, L50}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Credentials, ~}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{EnrolmentConfig, ErrorHandler}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{ControllerSpec, RetrievalOps, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherUtils.eitherFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, RetrievedUserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthenticatedActionWithRetrievedDataSpec
    extends ControllerSpec
    with MockFactory
    with SessionSupport
    with AuthActionSpec {

  val authenticatedAction =
    new AuthenticatedActionWithRetrievedData(
      mockSubscriptionService,
      mockAuthConnector,
      config,
      instanceOf[ErrorHandler],
      mockSessionStore
    )

  def mockHasSubscription()(response: Either[Error, Option[CgtReference]]) =
    (mockSubscriptionService
      .hasSubscription()(_: HeaderCarrier))
      .expects(*)
      .returning(EitherT(Future.successful(response)))

  implicit val ninoFormat: OFormat[NINO]                  = Json.format[NINO]
  implicit val sautrFormat: OFormat[SAUTR]                = Json.format[SAUTR]
  implicit val userTypeFormat: OFormat[RetrievedUserType] = derived.oformat[RetrievedUserType]

  def performAction[A](r: FakeRequest[A]): Future[Result] = {
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    val request = new MessagesRequest[A](r, stub[MessagesApi])
    authenticatedAction.invokeBlock(
      request, { a: AuthenticatedRequestWithRetrievedData[A] =>
        a.request.messagesApi shouldBe request.messagesApi
        Future.successful(Ok(Json.toJson(a.journeyUserType)))
      }
    )
  }

  val retrievals =
    Retrievals.confidenceLevel and Retrievals.affinityGroup and Retrievals.nino and
      Retrievals.saUtr and Retrievals.email and Retrievals.allEnrolments and Retrievals.credentials

  val emptyEnrolments = Enrolments(Set.empty)

  val cgtEnrolment = Enrolments(
    Set(
      Enrolment(
        EnrolmentConfig.Cgt.key,
        Seq(EnrolmentIdentifier(EnrolmentConfig.Cgt.cgtReferenceIdentifier, "XCGT123456789")),
        "Activated",
        None
      )
    )
  )

  implicit lazy val messagesApi: MessagesApi = instanceOf[MessagesApi]

  val (ggCredentials, ggCredId) = Credentials("id", "GovernmentGateway") -> GGCredId("id")

  "AuthenticatedActionWithRetrievedData" when {

    "handling a user who has logged in with an auth provider which isn't gg" must {

      "return the auth provider id" in {
        val providerType = "other provider"
        val retrievalsResult = Future successful (
          new ~(ConfidenceLevel.L50, Some(AffinityGroup.Organisation)) and
            None and None and None and emptyEnrolments and Some(Credentials("id", providerType))
        )

        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())

        status(result)        shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(RetrievedUserType.NonGovernmentGatewayRetrievedUser(providerType))
      }

    }

    "handling a logged in user with a cgt enrolment" must {

      "return the subscribed user details" in {
        val retrievalsResult = Future successful (
          new ~(ConfidenceLevel.L200, Some(AffinityGroup.Organisation)) and
            None and None and None and cgtEnrolment and Some(ggCredentials)
        )

        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())

        status(result) shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(
          RetrievedUserType.Subscribed(CgtReference("XCGT123456789"), GGCredId("id"))
        )

      }
    }

    "handling a logged in user with a corrupted cgt enrolment" must {

      "return an error" in {

        val badCgtEnrolment = Enrolments(
          Set(
            Enrolment(
              EnrolmentConfig.Cgt.key,
              Seq(EnrolmentIdentifier("XXXX-XXXX", "XCGT123456789")),
              "Activated",
              None
            )
          )
        )

        val retrievalsResult = Future successful (
          new ~(ConfidenceLevel.L200, Some(AffinityGroup.Organisation)) and
            None and None and None and badCgtEnrolment and Some(ggCredentials)
        )
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
        val result = performAction(FakeRequest())

        status(result) shouldBe INTERNAL_SERVER_ERROR

      }
    }

    "handling a logged in user who has no GG CGT enrolment but has successfully submitted a subscription to ETMP" must {

      "return the cgt reference" in {
        val retrievalsResult = Future successful (
          new ~(ConfidenceLevel.L200, Some(AffinityGroup.Organisation)) and
            None and None and None and emptyEnrolments and Some(ggCredentials)
        )

        inSequence {
          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          mockHasSubscription()(Right(Some(CgtReference("XCGT123456789"))))
        }

        val result = performAction(FakeRequest())

        status(result) shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(
          RetrievedUserType.Subscribed(CgtReference("XCGT123456789"), GGCredId("id"))
        )

      }
    }

    "handling a logged in user who has no GG CGT enrolment and no subscription in ETMP" must {

      "allow the user to proceed" in {
        val retrievalsResult = Future successful (
          new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
            Some("nino") and
            None and
            Some("") and
            emptyEnrolments and
            Some(ggCredentials)
        )

        val expectedRetrieval =
          RetrievedUserType.Individual(Right(NINO("nino")), None, ggCredId)

        inSequence {
          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          mockHasSubscription()(Right(None))
        }

        val result = performAction(FakeRequest())

        status(result)        shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)

      }
    }

    "handling a logged in user who has no CGT enrolment and there is an error querying the backend for the subscription status" must {

      "return an error" in {
        val retrievalsResult = Future successful (
          new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
            Some("nino") and
            None and
            Some("") and
            emptyEnrolments and
            Some(ggCredentials)
        )

        inSequence {
          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          mockHasSubscription()(Left(Error("Mongo error")))
        }

        val result = performAction(FakeRequest())

        status(result) shouldBe INTERNAL_SERVER_ERROR

      }
    }

    "handling a not logged in user" must {

      "redirect to the login page" in {
        val requestUri = "/abc"

        List[NoActiveSession](
          BearerTokenExpired(),
          MissingBearerToken(),
          InvalidBearerToken(),
          SessionRecordNotFound()
        ).foreach { e =>
          withClue(s"For error $e: ") {
            mockAuth(EmptyPredicate, retrievals)(Future.failed(e))

            val result = performAction(FakeRequest("GET", requestUri))
            status(result) shouldBe SEE_OTHER

            val redirectTo = redirectLocation(result)
            redirectTo shouldBe Some(s"$signInUrl?continue=${urlEncode(selfBaseUrl + requestUri)}&origin=$origin")
          }
        }
      }

    }

    "handling cases with incorrect type of credentials" must {

      "show an error page" when {

        def retrievalResult(credentials: Option[Credentials]) =
          new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
            Some("nino") and
            None and
            Some("email") and
            emptyEnrolments and
            credentials

        "no credentials can be retrieved" in {
          mockAuth(EmptyPredicate, retrievals)(Future.successful(retrievalResult(None)))

          checkIsTechnicalErrorPage(performAction(FakeRequest()))
        }

      }

    }

    "handling agents" must {

      "show an error page" when {

        "no gg cred id can be found" in {
          val retrievalsResult = Future successful (
            new ~(ConfidenceLevel.L50, Some(AffinityGroup.Agent)) and
              None and None and None and emptyEnrolments and None
          )

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

          checkIsTechnicalErrorPage(performAction(FakeRequest()))
        }

        "a gg cred id can be found but no agent enrolment can be found" in {
          val retrievalsResult = Future successful (
            new ~(ConfidenceLevel.L50, Some(AffinityGroup.Agent)) and
              None and None and None and emptyEnrolments and Some(ggCredentials)
          )

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

          checkIsTechnicalErrorPage(performAction(FakeRequest()))
        }

        "a gg cred id and an agent enrolment can be found but no agent reference nubmer can be found" in {
          val enrolments = Enrolments(Set(Enrolment(EnrolmentConfig.Agents.key)))

          val retrievalsResult = Future successful (
            new ~(ConfidenceLevel.L50, Some(AffinityGroup.Agent)) and
              None and None and None and enrolments and Some(ggCredentials)
          )

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

          checkIsTechnicalErrorPage(performAction(FakeRequest()))
        }
      }

      "allow the request through" when {

        "a gg cred id and agent reference number can be found" in {
          val arn = AgentReferenceNumber("arn")
          val enrolments = Enrolments(
            Set(
              Enrolment(
                EnrolmentConfig.Agents.key,
                Seq(EnrolmentIdentifier(EnrolmentConfig.Agents.agentReferenceNumberIdentifier, arn.value)),
                ""
              )
            )
          )

          val retrievalsResult = Future successful (
            new ~(ConfidenceLevel.L50, Some(AffinityGroup.Agent)) and
              None and None and None and enrolments and Some(ggCredentials)
          )

          val expectedRetrieval = RetrievedUserType.Agent(ggCredId, arn)

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

          val result = performAction(FakeRequest())

          status(result)        shouldBe OK
          contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)
        }

      }

    }

    "handling no affinity group" must {

      "show an error page" in {
        val retrievalsResult = Future successful (
          new ~(ConfidenceLevel.L50, None) and
            None and None and None and emptyEnrolments and Some(ggCredentials)
        )

        inSequence {
          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
        }

        checkIsTechnicalErrorPage(performAction(FakeRequest()))
      }

    }

    "handling organisations" must {

      "indicate when the organisation does not have a trust enrolment" in {
        val retrievalsResult = Future successful (
          new ~(ConfidenceLevel.L50, Some(AffinityGroup.Organisation)) and
            None and None and Some("email") and emptyEnrolments and Some(ggCredentials)
        )

        inSequence {
          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          mockHasSubscription()(Right(None))
        }

        val result = performAction(FakeRequest())

        status(result) shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(
          RetrievedUserType.OrganisationUnregisteredTrust(Some(Email("email")), GGCredId(ggCredentials.providerId))
        )
      }

      "show an error page" when {

        "the organisation has a trust enrolment but a SAUTR cannot be found" in {
          val trustEnrolment = Enrolment(EnrolmentConfig.Trusts.key)
          val retrievalsResult = Future successful (
            new ~(ConfidenceLevel.L50, Some(AffinityGroup.Organisation))
              and None and None and None and Enrolments(Set(trustEnrolment)) and Some(ggCredentials)
          )

          inSequence {
            mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
            mockHasSubscription()(Right(None))
          }

          val result = performAction(FakeRequest())
          checkIsTechnicalErrorPage(result)
        }

      }

      "effect the request action" when {

        "the organisation has a trust enrolment and a SAUTR can be found" in {
          val sautr = SAUTR("123456")
          val trustEnrolment =
            Enrolment(
              EnrolmentConfig.Trusts.key,
              Seq(EnrolmentIdentifier(EnrolmentConfig.Trusts.sautrIdentifier, sautr.value)),
              "state"
            )

          val retrievalsResult = Future successful (
            new ~(ConfidenceLevel.L50, Some(AffinityGroup.Organisation))
              and None and None and Some("email") and Enrolments(Set(trustEnrolment)) and Some(ggCredentials)
          )

          inSequence {
            mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
            mockHasSubscription()(Right(None))
          }

          val result = performAction(FakeRequest())
          status(result)        shouldBe OK
          contentAsJson(result) shouldBe Json.toJson(RetrievedUserType.Trust(sautr, Some(Email("email")), ggCredId))
        }

      }

      "filter out empty emails" in {
        val sautr = SAUTR("123456")
        val trustEnrolment =
          Enrolment(
            EnrolmentConfig.Trusts.key,
            Seq(EnrolmentIdentifier(EnrolmentConfig.Trusts.sautrIdentifier, sautr.value)),
            "state"
          )

        val retrievalsResult = Future successful (
          new ~(ConfidenceLevel.L50, Some(AffinityGroup.Organisation))
            and None and None and Some("") and Enrolments(Set(trustEnrolment)) and Some(ggCredentials)
        )

        inSequence {
          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          mockHasSubscription()(Right(None))
        }

        val result = performAction(FakeRequest())
        status(result)        shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(RetrievedUserType.Trust(sautr, None, ggCredId))
      }

    }
  }

  "handling individuals" when {

    "handling a logged in user with CL200 and all necessary data" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          None and
          Some("email") and
          emptyEnrolments and
          Some(ggCredentials)
      )

      val expectedRetrieval =
        RetrievedUserType.Individual(Right(NINO("nino")), Some(Email("email")), ggCredId)

      "effect the requested action" in {
        inSequence {
          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          mockHasSubscription()(Right(None))
        }

        val result = performAction(FakeRequest())
        status(result)        shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)
      }
    }

    "handling a logged in user with CL200 and a defined but empty email" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          None and
          Some("") and
          emptyEnrolments and
          Some(ggCredentials)
      )

      val expectedRetrieval =
        RetrievedUserType.Individual(Right(NINO("nino")), None, ggCredId)

      "effect the requested action" in {
        inSequence {
          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
          mockHasSubscription()(Right(None))
        }

        val result = performAction(FakeRequest())
        status(result)        shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)
      }
    }

    "handling a logged in user" when {

      "the CL is less than 200" must {

        "indicate so in the result" in {
          for {
            cl        <- List[ConfidenceLevel](L0, L50, L100)
            mayBeNino <- List[Option[NINO]](Some(NINO("nino")), None)
          } {
            withClue(s"For confidence level $cl ") {
              val retrievalsResult = Future successful (
                new ~(cl, Some(AffinityGroup.Individual)) and
                  mayBeNino.map(_.value) and
                  None and
                  Some("email") and
                  emptyEnrolments and
                  Some(ggCredentials)
              )

              inSequence {
                mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
                mockHasSubscription()(Right(None))
              }

              val result = performAction(FakeRequest())
              status(result) shouldBe OK
              contentAsJson(result) shouldBe Json.toJson(
                RetrievedUserType.IndividualWithInsufficientConfidenceLevel(
                  mayBeNino,
                  None,
                  Some(Email("email")),
                  GGCredId(ggCredentials.providerId)
                )
              )
            }
          }

        }

        "filter out empty emails" in {
          val retrievalsResult = Future successful (
            new ~(L50, Some(AffinityGroup.Individual)) and
              None and None and Some("") and emptyEnrolments and Some(ggCredentials)
          )

          inSequence {
            mockAuth(EmptyPredicate, retrievals)(retrievalsResult)
            mockHasSubscription()(Right(None))
          }

          val result = performAction(FakeRequest())
          status(result) shouldBe OK
          contentAsJson(result) shouldBe Json.toJson(
            RetrievedUserType.IndividualWithInsufficientConfidenceLevel(
              None,
              None,
              None,
              GGCredId(ggCredentials.providerId)
            )
          )

        }

      }
    }
  }

  "handling the case when an authorisation exception is thrown" must {

    "throw an exception" in {
      List[AuthorisationException](
        InsufficientEnrolments(),
        UnsupportedAffinityGroup(),
        UnsupportedAuthProvider(),
        UnsupportedCredentialRole(),
        IncorrectCredentialStrength(),
        InternalError()
      ).foreach { e =>
        withClue(s"For error $e: ") {
          val exception = intercept[AuthorisationException] {
            mockAuth(EmptyPredicate, retrievals)(Future.failed(e))

            await(performAction(FakeRequest()))
          }

          exception shouldBe e
        }
      }
    }
  }

}
