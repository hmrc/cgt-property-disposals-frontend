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

import java.net.URLEncoder
import java.time.LocalDate

import julienrf.json.derived
import org.joda.time.{LocalDate => JodaLocalDate}
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
import uk.gov.hmrc.auth.core.retrieve.{ItmpName, ~, Name => GGName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{RetrievalOps, routes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{DateOfBirth, Email, NINO, Name, SAUTR, UserType}

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
      Retrievals.itmpName and Retrievals.name and Retrievals.itmpDateOfBirth and Retrievals.email and
      Retrievals.allEnrolments

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
                None and None and None and None and None and emptyEnrolments
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
            new ~(ConfidenceLevel.L50, Some(AffinityGroup.Organisation))
              and None and None and None and None and None and emptyEnrolments
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
          and None and None and None and None and None and Enrolments(Set(trustEnrolment))
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
              and None and None and None and None and None and Enrolments(Set(trustEnrolment))
            )

          mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

          val result  = performAction(FakeRequest())
          status(result) shouldBe OK
          contentAsJson(result) shouldBe Json.toJson(UserType.Trust(sautr))
        }

      }


    }
  }

  "handling individuals" when {

    "handling a logged in user with CL200 and all necessary data" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          Some(ItmpName(Some("givenName"), Some("middleName"), Some("familyName"))) and
          Some(GGName(Some("forename"), Some("surname"))) and
          Some(new JodaLocalDate(2000, 4, 10)) and
          Some("email") and
          emptyEnrolments
        )

      val expectedRetrieval =
        UserType.Individual(
          NINO("nino"),
          Name("givenName", "familyName"),
          DateOfBirth(LocalDate.of(2000, 4, 10)),
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
          Some(ItmpName(Some("givenName"), Some("middleName"), Some("familyName"))) and
          Some(GGName(Some("forename"), Some("surname"))) and
          Some(new JodaLocalDate(2000, 4, 10)) and
          Some("") and
          emptyEnrolments
        )

      val expectedRetrieval =
        UserType.Individual(
          NINO("nino"),
          Name("givenName", "familyName"),
          DateOfBirth(LocalDate.of(2000, 4, 10)),
          None)

      "effect the requested action" in {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)        shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)
      }
    }


    "handling a logged in user with CL200 and a NINO, and an incomplete ITMP name" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          Some(ItmpName(None, Some("middleName"), Some("familyName"))) and
          None and
          Some(new JodaLocalDate(2000, 4, 10)) and
          Some("email") and
          emptyEnrolments
        )
      "show an error" in {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        checkIsTechnicalErrorPage(result)
      }
    }

    "handling a logged in user with CL200 and a NINO, and no ITMP name and complete non-ITMP name" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          None and
          Some(GGName(Some("first-name second-name"), None)) and
          Some(new JodaLocalDate(2000, 4, 10)) and
          None and
          emptyEnrolments
        )
      val expectedRetrieval =
        UserType.Individual(
          NINO("nino"),
          Name("first-name", "second-name"),
          DateOfBirth(LocalDate.of(2000, 4, 10)),
          None)

      "effect the requested action" in {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)          shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)
      }
    }

    "handling a logged in user with CL200 and a NINO, and no ITMP name and complete non-ITMP name " +
      "with more than two parts should only retrieve the first and last name" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          None and
          Some(GGName(Some("first-name second-name third-name"), None)) and
          Some(new JodaLocalDate(2000, 4, 10)) and
          Some("email") and
          emptyEnrolments
        )

      val expectedRetrieval =
        UserType.Individual(
          NINO("nino"),
          Name("first-name", "third-name"),
          DateOfBirth(LocalDate.of(2000, 4, 10)),
          Some(Email("email")))

      "effect the requested action" in {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        status(result)          shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(expectedRetrieval)
      }
    }

    "handling a logged in user with CL200 and a NINO, and no ITMP name and incomplete non-ITMP name" must {

      def retrievalsResult(ggName: Option[String]) = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          None and
          Some(GGName(ggName, None)) and
          Some(new JodaLocalDate(2000, 4, 10)) and
          Some("email") and
          emptyEnrolments
        )

      "show an error" in {
        List(
          Some("first-name-only"),
          Some(""),
          None
        ).foreach{ name =>
          withClue(s"For name '$name': "){
            mockAuth(EmptyPredicate, retrievals)(retrievalsResult(name))

            val result = performAction(FakeRequest())
            checkIsTechnicalErrorPage(result)
          }
        }
      }
    }

    "handling a logged in user with CL200 and a NINO and no ITMP name" must {
      val retrievalsResult = Future successful (
        new ~(ConfidenceLevel.L200, Some(AffinityGroup.Individual)) and
          Some("nino") and
          None and
          None and
          Some(new JodaLocalDate(2000, 4, 10)) and
          None and
          emptyEnrolments
        )

      "show an error" in {
        mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

        val result = performAction(FakeRequest())
        checkIsTechnicalErrorPage(result)
      }
    }

    "handling a logged in user" when {

      "the CL is less than 200" must {

        "indicate so in the result" in {
          import ConfidenceLevel._
          for{
            cl        <- List[ConfidenceLevel](L0, L50, L100)
            mayBeNino <- List[Option[NINO]](Some(NINO("nino")), None)
          }{
            withClue(s"For confidence level $cl "){
              val retrievalsResult = Future successful (
                new ~(cl, Some(AffinityGroup.Individual)) and mayBeNino.map(_.value) and
                  None and None and None and None and emptyEnrolments
                )

              mockAuth(EmptyPredicate, retrievals)(retrievalsResult)

              val result = performAction(FakeRequest())
              status(result) shouldBe OK
              contentAsJson(result) shouldBe Json.toJson(
                UserType.InsufficientConfidenceLevel(mayBeNino)
              )
            }
          }
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

