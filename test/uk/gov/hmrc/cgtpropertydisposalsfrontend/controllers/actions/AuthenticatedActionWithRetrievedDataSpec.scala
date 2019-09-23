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

import julienrf.json.derived
import org.scalamock.scalatest.MockFactory
import play.api.i18n.MessagesApi
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{MessagesRequest, Result}
import play.api.mvc.Results.Ok
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.RetrievalOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Email, NINO, SAUTR, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherFormat.eitherFormat

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthenticatedActionWithRetrievedDataSpec
  extends ControllerSpec with MockFactory with SessionSupport with AuthActionSpec {


    val authenticatedAction =
      new AuthenticatedActionWithRetrievedData(mockAuthConnector, config, instanceOf[ErrorHandler], mockSessionStore)

    implicit val ninoFormat: OFormat[NINO] = Json.format[NINO]
    implicit val sautrFormat: OFormat[SAUTR] = Json.format[SAUTR]
    implicit val userTypeFormat: OFormat[UserType] = derived.oformat[UserType]

    def performAction[A](r: FakeRequest[A]): Future[Result] = {
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      val request = new MessagesRequest[A](r, stub[MessagesApi])
      authenticatedAction.invokeBlock(request, { a: AuthenticatedRequestWithRetrievedData[A] =>
        a.request.messagesApi shouldBe request.messagesApi
        Future.successful(Ok(Json.toJson(a.userType)))
      })
    }

  val retrievals =
    Retrievals.confidenceLevel and Retrievals.affinityGroup and Retrievals.nino and
      Retrievals.saUtr and Retrievals.email and Retrievals.allEnrolments

  val emptyEnrolments = Enrolments(Set.empty)

  "AuthenticatedActionWithRetrievedData" when {

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

    "handling agents or unknown affinity groups" must {

      "show an error page" in {
        List(
          Some(AffinityGroup.Agent),
          None
        ).foreach{ affinityGroup =>
          withClue(s"For affinity group $affinityGroup: "){
            val retrievalsResult = Future successful (
              new ~(ConfidenceLevel.L50, affinityGroup) and
                None and None and None and emptyEnrolments
              )

            mockAuth(EmptyPredicate, retrievals)(retrievalsResult)


            checkIsTechnicalErrorPage(performAction(FakeRequest()))
          }
        }

      }

    }

    "handling organisations" must {

      "indicate when the organisation does not have a trust enrolment" in {
          val retrievalsResult = Future successful (
            new ~(ConfidenceLevel.L50, Some(AffinityGroup.Organisation)) and
              None and None and None and emptyEnrolments
            )

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

          val result  = performAction(FakeRequest())

        status(result) shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(UserType.OrganisationUnregisteredTrust)
      }

      "show an error page" when {

        "the organisation has a trust enrolment but a SAUTR cannot be found" in {
          val trustEnrolment = Enrolment("HMRC-TERS-ORG")
          val retrievalsResult = Future successful (
            new ~(ConfidenceLevel.L50, Some(AffinityGroup.Organisation))
              and None and None and None and Enrolments(Set(trustEnrolment))
          )

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

          val result  = performAction(FakeRequest())
          checkIsTechnicalErrorPage(result)
        }

      }

      "effect the request action" when {

        "the organisation has a trust enrolment and a SAUTR can be found" in {
          val sautr = SAUTR("123456")
          val trustEnrolment =
            Enrolment(
            "HMRC-TERS-ORG",
              Seq(EnrolmentIdentifier("SAUTR", sautr.value)),
              "state"
            )

          val retrievalsResult = Future successful (
            new ~(ConfidenceLevel.L50, Some(AffinityGroup.Organisation))
              and None and None and Some("email") and Enrolments(Set(trustEnrolment))
            )

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

          val result  = performAction(FakeRequest())
          status(result) shouldBe OK
          contentAsJson(result) shouldBe Json.toJson(UserType.Trust(sautr, Some(Email("email"))))
        }

      }

      "filter out empty emails" in {
        val sautr = SAUTR("123456")
        val trustEnrolment =
          Enrolment(
            "HMRC-TERS-ORG",
            Seq(EnrolmentIdentifier("SAUTR", sautr.value)),
            "state"
          )

        val retrievalsResult = Future successful (
          new ~(ConfidenceLevel.L50, Some(AffinityGroup.Organisation))
            and None and None and Some("") and Enrolments(Set(trustEnrolment))
          )

        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result  = performAction(FakeRequest())
        status(result) shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(UserType.Trust(sautr, None))
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
          emptyEnrolments
        )

      val expectedRetrieval =
        UserType.Individual(
          Right(NINO("nino")),
          Some(Email("email")))

      "effect the requested action" in {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)          shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)
      }
    }

    "handling a logged in user with CL200 and a defined but empty email" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          None and
          Some("") and
          emptyEnrolments
        )

      val expectedRetrieval =
        UserType.Individual(
          Right(NINO("nino")),
          None)

      "effect the requested action" in {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)        shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)
      }
    }

    "handling a logged in user" when {

      "the CL is less than 200" must {

        import ConfidenceLevel._

        "indicate so in the result" in {
          for{
            cl        <- List[ConfidenceLevel](L0, L50, L100)
            mayBeNino <- List[Option[NINO]](Some(NINO("nino")), None)
          }{
            withClue(s"For confidence level $cl "){
              val retrievalsResult = Future successful (
                new ~(cl, Some(AffinityGroup.Individual)) and
                  mayBeNino.map(_.value) and
                  None and
                  Some("email") and
                  emptyEnrolments
                )

              mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

              val result = performAction(FakeRequest())
              status(result) shouldBe OK
              contentAsJson(result) shouldBe Json.toJson(
                UserType.IndividualWithInsufficientConfidenceLevel(mayBeNino, None, Some(Email("email")))
              )
            }
          }

        }

        "filter out empty emails" in {
          val retrievalsResult = Future successful (
            new ~(L50, Some(AffinityGroup.Individual)) and
              None and None and Some("") and emptyEnrolments
            )

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

          val result = performAction(FakeRequest())
          status(result) shouldBe OK
          contentAsJson(result) shouldBe Json.toJson(
            UserType.IndividualWithInsufficientConfidenceLevel(None, None, None)
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
        UnsupportedCredentialRole(),
        UnsupportedAuthProvider(),
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

