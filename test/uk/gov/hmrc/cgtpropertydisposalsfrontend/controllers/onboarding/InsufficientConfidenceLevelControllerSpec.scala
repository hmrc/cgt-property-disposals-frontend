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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding

import cats.data.EitherT
import cats.instances.future._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.{Request, Result}
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, NameFormValidationTests, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.AlreadySubscribedWithDifferentGGAccount
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails.IndividualNameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecord, NameMatchError, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.BusinessPartnerRecordNameMatchRetryService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class InsufficientConfidenceLevelControllerSpec
    extends ControllerSpec
    with IvBehaviourSupport
    with SessionSupport
    with AuthSupport
    with ScalaCheckDrivenPropertyChecks
    with NameFormValidationTests
    with RedirectToStartBehaviour {

  val mockBprNameMatchService = mock[BusinessPartnerRecordNameMatchRetryService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[BusinessPartnerRecordNameMatchRetryService].toInstance(mockBprNameMatchService)
    )

  override lazy val additionalConfig = ivConfig(useRelativeUrls = false)

  lazy val controller = instanceOf[InsufficientConfidenceLevelController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def mockGetNumberOfUnsuccessfulAttempts(
    ggCredId: GGCredId
  )(
    result: Either[NameMatchError[IndividualNameMatchDetails], Option[
      UnsuccessfulNameMatchAttempts[IndividualNameMatchDetails]
    ]]
  ) =
    (
      mockBprNameMatchService
        .getNumberOfUnsuccessfulAttempts[IndividualNameMatchDetails](_: GGCredId)(
          _: Reads[IndividualNameMatchDetails],
          _: ExecutionContext
        )
      )
      .expects(ggCredId, *, *)
      .returning(EitherT.fromEither[Future](result))

  def mockAttemptNameMatch(
    sautr: SAUTR,
    name: IndividualName,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts[IndividualNameMatchDetails]]
  )(result: Either[NameMatchError[IndividualNameMatchDetails], (BusinessPartnerRecord, Option[CgtReference])]) =
    (
      mockBprNameMatchService
        .attemptBusinessPartnerRecordNameMatch[IndividualNameMatchDetails](
          _: IndividualNameMatchDetails,
          _: GGCredId,
          _: Option[UnsuccessfulNameMatchAttempts[IndividualNameMatchDetails]]
        )(
          _: Writes[IndividualNameMatchDetails],
          _: ExecutionContext,
          _: HeaderCarrier,
          _: Request[_]
        )
      )
      .expects(IndividualNameMatchDetails(name, sautr), ggCredId, previousUnsuccessfulNameMatchAttempts, *, *, *, *)
      .returning(EitherT.fromEither[Future](result))

  def session(subscriptionStatus: JourneyStatus): SessionData =
    SessionData.empty.copy(journeyStatus = Some(subscriptionStatus))

  val name = IndividualName("name", "surname")

  def commonBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: TryingToGetIndividualsFootprint => true
        case _                                  => false
      }
    )

  "InsufficientConfidenceLevelController" when {

    "handling requests to ask the user if they have a NINO" must {

      def performAction(): Future[Result] =
        controller.doYouHaveNINO()(FakeRequest())

      behave like commonBehaviour(performAction)

      "display the do you have a NINO page" when {

        "the session indicates that the user does not have sufficient confidence level" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(None, None, None, sample[GGCredId])))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(messageFromMessageKey("haveANino.title"))

        }

        "the session indicates that the user does not have sufficient confidence level and the user " +
          "has previously answered this question" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(Some(true), None, None, sample[GGCredId])))
          }

          val result = performAction()
          status(result) shouldBe OK
          val content = contentAsString(result)
          content should include(messageFromMessageKey("haveANino.title"))
          content should include("checked=\"checked\"")
        }

      }

    }

    "handling submitted answers to the have a NINO page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.doYouHaveNINOSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like commonBehaviour(() => performAction())

      "show a form error" when {

        "no data has been submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(None, None, None, sample[GGCredId])))
          }

          val result = performAction()
          status(result)          shouldBe BAD_REQUEST
          contentAsString(result) should include(messageFromMessageKey("hasNino.error.required"))
        }

        "the submitted data cannot be parsed" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(None, None, None, sample[GGCredId])))
          }

          val result = performAction("hasNino" -> "blah")
          status(result)          shouldBe BAD_REQUEST
          contentAsString(result) should include(messageFromMessageKey("hasNino.error.boolean"))
        }

      }

      "redirect to the IV journey" when {

        "the user indicates that they do have a NINO" in {
          val credId = sample[GGCredId]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(None, None, None, credId)))
            mockStoreSession(session(TryingToGetIndividualsFootprint(Some(true), None, None, credId)))(Right(()))
          }

          val result = performAction("hasNino" -> "true")
          checkIsRedirectToIv(result, false)
        }

        "the user indicates that they do have a NINO and the application " +
          "has been configured to used absolute urls to iv" in new ControllerSpec {

          override val overrideBindings =
            List[GuiceableModule](
              bind[BusinessPartnerRecordNameMatchRetryService].toInstance(mockBprNameMatchService),
              bind[AuthConnector].toInstance(mockAuthConnector),
              bind[SessionStore].toInstance(mockSessionStore)
            )

          override lazy val additionalConfig = ivConfig(useRelativeUrls = true)

          lazy val controller = instanceOf[InsufficientConfidenceLevelController]
          val credId          = sample[GGCredId]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(None, None, None, credId)))
            mockStoreSession(session(TryingToGetIndividualsFootprint(Some(true), None, None, credId)))(Right(()))
          }

          val result =
            controller.doYouHaveNINOSubmit()(FakeRequest().withFormUrlEncodedBody("hasNino" -> "true").withCSRFToken)
          checkIsRedirectToIv(result, true)
        }

      }

      "redirect to ask if the user has an SAUTR" when {

        "the user indicates that they do not have a NINO" in {
          val credId = sample[GGCredId]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(None, None, None, credId)))
            mockStoreSession(session(TryingToGetIndividualsFootprint(Some(false), None, None, credId)))(Right(()))
          }

          val result = performAction("hasNino" -> "false")
          checkIsRedirect(result, routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr())
        }

      }

      "not update the session" when {

        "the user has previously indicated that they have a NINO" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(Some(true), None, None, sample[GGCredId])))
          }

          val result = performAction("hasNino" -> "true")
          checkIsRedirectToIv(result, false)
        }

        "the user has previously indicated that they do not have a NINO" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(Some(false), None, None, sample[GGCredId])))
          }

          val result = performAction("hasNino" -> "false")
          checkIsRedirect(result, routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr())
        }
      }

    }

    "handling requests to ask the user if they have an SAUTR" must {

      def performAction(): Future[Result] =
        controller.doYouHaveAnSaUtr()(FakeRequest())

      behave like commonBehaviour(performAction)

      "redirect to the NINO page" when {

        "the session data indicates the user hasn't selected whether or not they have a NINO" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(None, None, None, sample[GGCredId])))
          }

          checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.doYouHaveNINO())
        }

      }

      "display the page" when {

        "the session indicates that the user does not have sufficient confidence level and the user " +
          "has indicated that they do not have a NINO" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(Some(false), None, None, sample[GGCredId])))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(messageFromMessageKey("haveAnSaUtr.title"))

        }

        "the session indicates that the user does not have sufficient confidence level and the user " +
          "has indicated that they do not have a NINO and the user has previously indicated that they " +
          "have an SAUTR" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(Some(false), Some(true), None, sample[GGCredId])))
          }

          val result = performAction()
          status(result) shouldBe OK
          val content = contentAsString(result)
          content should include(messageFromMessageKey("haveAnSaUtr.title"))
          content should include("checked=\"checked\"")
        }

        "the session indicates that the user does not have sufficient confidence level and the user " +
          "has indicated that they do not have a NINO and the user has previously indicated that they " +
          "do not have an SAUTR" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(Some(false), Some(false), None, sample[GGCredId])))
          }

          val result = performAction()
          status(result) shouldBe OK
          val content = contentAsString(result)
          content should include(messageFromMessageKey("haveAnSaUtr.title"))
          content should include("checked=\"checked\"")
        }

      }

    }

    "handling submitted answers to the have an SAUTR page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.doYouHaveSaUtrSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like commonBehaviour(() => performAction())

      "redirect to the NINO page" when {

        "the session data indicates the user hasn't selected whether or not they have a NINO" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(None, None, None, sample[GGCredId])))
          }

          checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.doYouHaveNINO())
        }

      }

      "show a form error" when {

        "the user did not select an option" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(TryingToGetIndividualsFootprint(Some(false), None, None, sample[GGCredId])))
          }

          val result = performAction()
          status(result)          shouldBe BAD_REQUEST
          contentAsString(result) should include(messageFromMessageKey("hasSaUtr.error.required"))
        }

      }

      "redirect to the enter sautr and name endpoint" when {

        "the user indicates they have an SA UTR and enters in a valid one" in {
          val subscriptionStatus =
            TryingToGetIndividualsFootprint(Some(false), None, None, sample[GGCredId])

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(subscriptionStatus))
            mockStoreSession(session(subscriptionStatus.copy(hasSautr = Some(true))))(Right(()))
          }

          val result = performAction("hasSaUtr" -> "true")
          checkIsRedirect(result, routes.InsufficientConfidenceLevelController.enterSautrAndName())
        }

      }

      "redirect to the registration journey" when {

        "the user indicates they do not have an SA UTR" in {
          val subscriptionStatus =
            TryingToGetIndividualsFootprint(Some(false), None, None, sample[GGCredId])

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session(subscriptionStatus))
            mockStoreSession(session(subscriptionStatus.copy(hasSautr = Some(false))))(Right(()))
          }

          val result = performAction("hasSaUtr" -> "false")
          checkIsRedirect(result, routes.RegistrationController.selectEntityType())
        }

      }

    }

    "handling requests to enter an SAUTR and name" must {

      val ggCredId = sample[GGCredId]

      def performAction(): Future[Result] =
        controller.enterSautrAndName()(FakeRequest())

      behave like commonBehaviour(performAction)

      "redirect to the select entity type page" when {

        def test(hasNino: Option[Boolean], hasSautr: Option[Boolean]) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session(
                TryingToGetIndividualsFootprint(hasNino, hasSautr, None, ggCredId)
              )
            )
          }

          val result = performAction()
          checkIsRedirect(result, routes.InsufficientConfidenceLevelController.doYouHaveNINO())
        }

        "the user has not answered the do you have a NINO question" in {
          test(None, None)
        }

        "the user has not answered the do you have an SAUTR question" in {
          test(Some(false), None)
        }

        "the user has answered that they have a NINO" in {
          test(Some(true), None)
        }

        "the user has answered that they do not have an SAUTR" in {
          test(Some(false), Some(false))
        }

      }

      "display the enter SA UTR and name page" when {

        "the user has indicated that they have no NINO but they do have an SA UTR" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session(
                TryingToGetIndividualsFootprint(Some(false), Some(true), None, ggCredId)
              )
            )
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(None))
          }

          val result = performAction()
          contentAsString(result) should include(messageFromMessageKey("enterSaUtr.title"))
          contentAsString(result) should not include (messageFromMessageKey("enterSaUtr.error.notFound", 0, 2))
        }

        "the user has indicated that they have no NINO but they do have an SA UTR and they have " +
          "previously made unsuccessful attempts to do a name match" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session(
                TryingToGetIndividualsFootprint(Some(false), Some(true), None, ggCredId)
              )
            )
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Right(
                Some(
                  UnsuccessfulNameMatchAttempts(1, 2, sample[IndividualNameMatchDetails])
                )
              )
            )
          }

          val result = performAction()
          contentAsString(result) should include(messageFromMessageKey("enterSaUtr.title"))
          contentAsString(result) should not include (messageFromMessageKey("enterSaUtr.error.notFound", 1, 2))
        }

      }

      "redirect to the too many attempts page" when {

        "the user has tried to do a name match too many times" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session(
                TryingToGetIndividualsFootprint(Some(false), Some(true), None, ggCredId)
              )
            )
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Left(NameMatchError.TooManyUnsuccessfulAttempts()))
          }

          checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.tooManyAttempts())
        }

      }

      "show an error page" when {

        "there is an error trying to see how many times a user has attempted to do a name match" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session(
                TryingToGetIndividualsFootprint(Some(false), Some(true), None, ggCredId)
              )
            )
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Left(NameMatchError.BackendError(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

    }

    "handling sumitted SAUTRs and names" must {

      val validSautr = SAUTR("1234567890")

      val validName = IndividualName("Elaine", "Belaine")

      val bpr = sample[BusinessPartnerRecord].copy(name = Right(validName))

      val ggCredId = sample[GGCredId]

      val expectedSessionData =
        session(TryingToGetIndividualsFootprint(Some(false), Some(true), None, ggCredId))

      val previousUnsuccessfulNameMatchAttempt =
        UnsuccessfulNameMatchAttempts(1, 3, sample[IndividualNameMatchDetails])

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterSautrAndNameSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like commonBehaviour(() => performAction())

      behave like nameFormValidationTests(
        data => performAction(data :+ ("saUtr" -> validSautr.value): _*),
        () =>
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Right(Some(UnsuccessfulNameMatchAttempts(1, 2, IndividualNameMatchDetails(validName, validSautr))))
            )
          }
      )

      "redirect to the select entity type page" when {

        def test(hasNino: Option[Boolean], hasSautr: Option[Boolean]) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              session(
                TryingToGetIndividualsFootprint(hasNino, hasSautr, None, sample[GGCredId])
              )
            )
          }

          val result = performAction()
          checkIsRedirect(result, routes.InsufficientConfidenceLevelController.doYouHaveNINO())
        }

        "the user has not answered the do you have a NINO question" in {
          test(None, None)
        }

        "the user has not answered the do you have an SAUTR question" in {
          test(Some(false), None)
        }

        "the user has answered that they have a NINO" in {
          test(Some(true), None)
        }

        "the user has answered that they do not have an SAUTR" in {
          test(Some(false), Some(false))
        }

      }

      "show a form error" when {

        "an SA UTR is not submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Right(Some(UnsuccessfulNameMatchAttempts(1, 2, IndividualNameMatchDetails(validName, validSautr))))
            )
          }

          val result = performAction("firstName" -> validName.firstName, "lastName" -> validName.lastName)
          status(result)          shouldBe BAD_REQUEST
          contentAsString(result) should include(messageFromMessageKey("saUtr.error.required"))
        }

        "the SA UTR is an empty string" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Right(Some(UnsuccessfulNameMatchAttempts(1, 2, IndividualNameMatchDetails(validName, validSautr))))
            )
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName"  -> validName.lastName,
            "saUtr"     -> ""
          )
          status(result)          shouldBe BAD_REQUEST
          contentAsString(result) should include(messageFromMessageKey("saUtr.error.required"))
        }

        "the SA UTR is not valid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Right(Some(UnsuccessfulNameMatchAttempts(1, 2, IndividualNameMatchDetails(validName, validSautr))))
            )
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName"  -> validName.lastName,
            "saUtr"     -> "not-an-sa-utr"
          )
          status(result)          shouldBe BAD_REQUEST
          contentAsString(result) should include(messageFromMessageKey("saUtr.error.pattern"))
        }

        "a valid SA UTR and name are entered but they cannot be matched to a BPR and the " +
          "user has not yet made too many unsuccessful attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
            mockAttemptNameMatch(validSautr, validName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
              Left(
                NameMatchError.NameMatchFailed(
                  UnsuccessfulNameMatchAttempts(2, 3, IndividualNameMatchDetails(validName, validSautr))
                )
              )
            )
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName"  -> validName.lastName,
            "saUtr"     -> validSautr.value
          )
          status(result)          shouldBe BAD_REQUEST
          contentAsString(result) should include(messageFromMessageKey("enterSaUtr.error.notFound", 2, 3))
        }

      }

      "display an error page" when {

        "there is an error retrieving the number of previous unsuccessful attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Left(NameMatchError.BackendError(Error(""))))
          }

          val result = performAction()

          checkIsTechnicalErrorPage(result)
        }

        "there is an error getting the BPR" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
            mockAttemptNameMatch(validSautr, validName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
              Left(NameMatchError.BackendError(Error("")))
            )
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName"  -> validName.lastName,
            "saUtr"     -> validSautr.value
          )

          checkIsTechnicalErrorPage(result)
        }

        "there is an error storing a retrieved BPR in mongo" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
            mockAttemptNameMatch(validSautr, validName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
              Right(bpr -> None)
            )
            mockStoreSession(session(SubscriptionMissingData(bpr, None, ggCredId, None)))(Left(Error("")))
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName"  -> validName.lastName,
            "saUtr"     -> validSautr.value
          )

          checkIsTechnicalErrorPage(result)
        }

        "a BPR is found but it is one for a trust instead of an individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
            mockAttemptNameMatch(validSautr, validName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
              Right(bpr.copy(name = Left(TrustName(""))) -> None)
            )
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName"  -> validName.lastName,
            "saUtr"     -> validSautr.value
          )

          checkIsTechnicalErrorPage(result)

        }

      }

      "update the session and redirect to the start endpoint" when {

        "the user submits a valid SA UTR and name and a BPR can be retrieved and the user does not already have a cgt reference" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
            mockAttemptNameMatch(validSautr, validName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
              Right(bpr -> None)
            )
            mockStoreSession(session(SubscriptionMissingData(bpr, None, ggCredId, None)))(Right(()))
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName"  -> validName.lastName,
            "saUtr"     -> validSautr.value
          )

          checkIsRedirect(result, uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController.start())
        }

      }

      "update the session and redirect to the already subscribed page" when {

        "the user submits a valid SA UTR and name and a BPR can be retrieved and the user already has a cgt reference" in {
          val cgtReference = sample[CgtReference]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
            mockAttemptNameMatch(validSautr, validName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
              Right(bpr -> Some(cgtReference))
            )
            mockStoreSession(session(AlreadySubscribedWithDifferentGGAccount(ggCredId, Some(cgtReference))))(Right(()))
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName"  -> validName.lastName,
            "saUtr"     -> validSautr.value
          )

          checkIsRedirect(result, routes.SubscriptionController.alreadySubscribedWithDifferentGGAccount())
        }

      }

      "redirect to the too many attempts page" when {

        "the user has attempted to perform a name match too many times" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Left(NameMatchError.TooManyUnsuccessfulAttempts()))
          }

          val result = performAction()
          checkIsRedirect(result, routes.InsufficientConfidenceLevelController.tooManyAttempts())

        }

        "the user has not initially made too many unsuccessful attempts but submits " +
          "details which do not match a BPR and have now made too many attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
            mockAttemptNameMatch(validSautr, validName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
              Left(NameMatchError.TooManyUnsuccessfulAttempts())
            )
          }

          val result = performAction(
            "firstName" -> validName.firstName,
            "lastName"  -> validName.lastName,
            "saUtr"     -> validSautr.value
          )

          checkIsRedirect(result, routes.InsufficientConfidenceLevelController.tooManyAttempts())
        }

      }

    }

    "handling requests to display the too many attempts page" must {

      def performAction() = controller.tooManyAttempts()(FakeRequest())

      val ggCredId = sample[GGCredId]

      val expectedSessionData =
        session(TryingToGetIndividualsFootprint(Some(false), Some(true), None, ggCredId))

      behave like commonBehaviour(performAction)

      "display an error page" when {

        "there is an error trying to get the number of unsuccessful name match attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Left(NameMatchError.BackendError(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "display the too many attempts page" when {

          "the user has attempted a name match too many times" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(expectedSessionData)
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Left(NameMatchError.TooManyUnsuccessfulAttempts()))
            }

            val result = performAction()
            status(result)          shouldBe OK
            contentAsString(result) should include(messageFromMessageKey("enterSaUtr.tooManyAttempts.title"))
          }
        }

        "redirect to the enter sautr and name page" when {

          "the user has attempted a name match but they still have attempts left" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(expectedSessionData)
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
                Right(Some(UnsuccessfulNameMatchAttempts(1, 2, sample[IndividualNameMatchDetails])))
              )
            }

            checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.enterSautrAndName())
          }

          "the user has not attempted a name match" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(expectedSessionData)
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(None))
            }

            checkIsRedirect(performAction(), routes.InsufficientConfidenceLevelController.enterSautrAndName())
          }

        }

      }

    }

  }

}
