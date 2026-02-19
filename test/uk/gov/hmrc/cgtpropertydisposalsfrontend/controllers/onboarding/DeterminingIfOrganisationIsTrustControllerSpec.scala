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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding

import cats.data.EitherT
import cats.instances.future.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.Reads
import play.api.mvc.{Request, Result}
import play.api.test.CSRFTokenHelper.*
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.routes as onboardingRoutes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{AlreadySubscribedWithDifferentGGAccount, NewEnrolmentCreatedForMissingEnrolment}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.NameMatchServiceError.TooManyUnsuccessfulAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails.TrustNameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.BusinessPartnerRecordGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameMatchGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId, TRN}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecord, BusinessPartnerRecordResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, NameMatchServiceError, SessionData, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.NameMatchRetryService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class DeterminingIfOrganisationIsTrustControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour {

  private val mockBprNameMatchService = mock[NameMatchRetryService]

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[NameMatchRetryService].toInstance(mockBprNameMatchService),
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  private lazy val controller = instanceOf[DeterminingIfOrganisationIsTrustController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    behave like redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: DeterminingIfOrganisationIsTrust => true
        case _                                   => false
      }
    )

  def sessionDataWithStatus(journeyStatus: JourneyStatus): SessionData =
    SessionData.empty.copy(journeyStatus = Some(journeyStatus))

  private def mockGetNumberOfUnsuccessfulAttempts(
    ggCredId: GGCredId
  )(
    result: Either[NameMatchServiceError[TrustNameMatchDetails], Option[
      UnsuccessfulNameMatchAttempts[TrustNameMatchDetails]
    ]]
  ) =
    (
      (credId: GGCredId, reads: Reads[TrustNameMatchDetails], hc: HeaderCarrier, req: Request[?]) =>
        mockBprNameMatchService
          .getNumberOfUnsuccessfulAttempts[TrustNameMatchDetails](credId)(using reads, hc, req)
    )
      .expects(ggCredId, *, *, *)
      .returning(EitherT.fromEither[Future](result))

  private def mockAttemptNameMatch(
    trn: TRN,
    name: TrustName,
    ggCredId: GGCredId,
    previousUnsuccessfulNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts[TrustNameMatchDetails]],
    lang: Lang
  )(
    result: Either[NameMatchServiceError[TrustNameMatchDetails], (BusinessPartnerRecord, BusinessPartnerRecordResponse)]
  ) =
    (
      mockBprNameMatchService
        .attemptBusinessPartnerRecordNameMatch(
          _: TrustNameMatchDetails,
          _: GGCredId,
          _: Option[UnsuccessfulNameMatchAttempts[TrustNameMatchDetails]],
          _: Lang
        )(using
          _: HeaderCarrier,
          _: Request[?]
        )
      )
      .expects(
        TrustNameMatchDetails(name, trn),
        ggCredId,
        previousUnsuccessfulNameMatchAttempts,
        lang,
        *,
        *
      )
      .returning(EitherT.fromEither[Future](result))

  private val ggCredId = sample[GGCredId]

  "DeterminingIfOrganisationIsTrustController" when {

    "handling requests to display the 'do you want to report for a trust' page" must {

      def performAction(): Future[Result] =
        controller.doYouWantToReportForATrust()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      "show the page" when {

        "the user has not selected an option before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(ggCredId, None, None, None)
              )
            )
          }

          val result = performAction()
          status(result)        shouldBe OK
          contentAsString(result) should include(
            messageFromMessageKey("isReportingForATrust.title")
          )
        }

        "the user has selected an option before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  None
                )
              )
            )
          }

          val result  = performAction()
          val content = contentAsString(result)

          status(result) shouldBe OK
          content          should include(
            messageFromMessageKey("isReportingForATrust.title")
          )
          content          should include("checked")
        }

      }

    }

    "handling submitted answers from the 'do you want to report for a trust' page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.doYouWantToReportForATrustSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      "show a form error" when {

        "an option has not been selected" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(ggCredId, None, None, None)
              )
            )
          }

          val result = performAction()
          status(result)        shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("isReportingForATrust.error.required")
          )
        }

        "the data submitted cannot be read" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(ggCredId, None, None, None)
              )
            )
          }

          val result = performAction("isReportingForATrust" -> "123")
          status(result)        shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("isReportingForATrust.error.boolean")
          )
        }

      }

      "show an error page" when {

        "the answer cannot be stored in mongo" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(ggCredId, None, None, None)
              )
            )
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  DeterminingIfOrganisationIsTrust(
                    ggCredId,
                    None,
                    Some(true),
                    None
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction("isReportingForATrust" -> "true")
          )
        }

      }

      "redirect to the 'report with corporate tax' page" when {

        "the answer says they are not reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(ggCredId, None, None, None)
              )
            )
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  DeterminingIfOrganisationIsTrust(
                    ggCredId,
                    None,
                    Some(false),
                    None
                  )
                )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction("isReportingForATrust" -> "false"),
            onboardingRoutes.DeterminingIfOrganisationIsTrustController
              .reportWithCorporateTax()
          )
        }

      }

      "redirect to the 'do you have a trn' page" when {

        "the answer says they are reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(ggCredId, None, None, None)
              )
            )
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  DeterminingIfOrganisationIsTrust(
                    ggCredId,
                    None,
                    Some(true),
                    None
                  )
                )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction("isReportingForATrust" -> "true"),
            onboardingRoutes.DeterminingIfOrganisationIsTrustController
              .doYouHaveATrn()
          )
        }
      }

    }

    "handling requests to display the 'report with corporate tax' page" must {

      def performAction(): Future[Result] =
        controller.reportWithCorporateTax()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the start endpoint" when {

        "the user has not answered whether or not they're reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(ggCredId, None, None, None)
              )
            )
          }

          checkIsRedirect(
            performAction(),
            cgtpropertydisposalsfrontend.controllers.routes.StartController
              .start()
          )
        }

        "the user has said that they're reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  None
                )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            cgtpropertydisposalsfrontend.controllers.routes.StartController
              .start()
          )
        }

      }

      "show the page" when {

        "the user has said that they're not reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(false),
                  None
                )
              )
            )
          }

          val result = performAction()
          status(result)        shouldBe 200
          contentAsString(result) should include(
            messageFromMessageKey("reportCorpTax.title")
          )
        }

      }

    }

    "handling requests to display the 'do you have a TRN' page" must {

      def performAction(): Future[Result] =
        controller.doYouHaveATrn()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the start endpoint" when {

        "the user has not indicated whether or not they are reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(ggCredId, None, None, None)
              )
            )
          }

          checkIsRedirect(
            performAction(),
            cgtpropertydisposalsfrontend.controllers.routes.StartController
              .start()
          )
        }

        "the user has indicated that they are not reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(false),
                  None
                )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            cgtpropertydisposalsfrontend.controllers.routes.StartController
              .start()
          )
        }

      }

      "show the page" when {

        "the user has not selected an option before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  None
                )
              )
            )
          }

          val result = performAction()
          status(result)        shouldBe OK
          contentAsString(result) should include(
            messageFromMessageKey("haveATrn.title")
          )
        }

        "the user has selected an option before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  Some(true)
                )
              )
            )
          }

          val result  = performAction()
          val content = contentAsString(result)

          status(result) shouldBe OK
          content          should include(messageFromMessageKey("haveATrn.title"))
          content          should include("checked")
        }

      }

    }

    "handling submitted answers from the 'do you have a TRN' page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.doYouHaveATrnSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the start endpoint" when {

        "the user has not indicated whether or not they are reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(ggCredId, None, None, None)
              )
            )
          }

          checkIsRedirect(
            performAction(),
            cgtpropertydisposalsfrontend.controllers.routes.StartController
              .start()
          )
        }

        "the user has indicated that they are not reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(false),
                  None
                )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            cgtpropertydisposalsfrontend.controllers.routes.StartController
              .start()
          )
        }

      }

      "show a form error" when {

        "an option has not been selected" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  None
                )
              )
            )
          }

          val result = performAction()
          status(result)        shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("hasTrn.error.required")
          )
        }

        "the data submitted cannot be read" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  None
                )
              )
            )
          }

          val result = performAction("hasTrn" -> "123")
          status(result)        shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("hasTrn.error.boolean")
          )
        }

      }

      "show an error page" when {

        "the answer cannot be stored in mongo" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  None
                )
              )
            )
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  DeterminingIfOrganisationIsTrust(
                    ggCredId,
                    None,
                    Some(true),
                    Some(true)
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction("hasTrn" -> "true"))
        }

      }

      "redirect to the 'register your trust' page" when {

        "the user does not have a TRN" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  None
                )
              )
            )
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  DeterminingIfOrganisationIsTrust(
                    ggCredId,
                    None,
                    Some(true),
                    Some(false)
                  )
                )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction("hasTrn" -> "false"),
            onboardingRoutes.DeterminingIfOrganisationIsTrustController
              .registerYourTrust()
          )
        }

      }

      "redirect to the 'enter trn' page" when {

        "the user does  have a TRN" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  None
                )
              )
            )
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  DeterminingIfOrganisationIsTrust(
                    ggCredId,
                    None,
                    Some(true),
                    Some(true)
                  )
                )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction("hasTrn" -> "true"),
            onboardingRoutes.DeterminingIfOrganisationIsTrustController
              .enterTrn()
          )
        }
      }

    }

    "handling requests to display the register your trust page" must {

      def performAction(): Future[Result] =
        controller.registerYourTrust()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      "show the register your trust page" when {

        "the session data indicates the user is an organisation which is not associated with a registered trust" in {
          val sessionData = sessionDataWithStatus(
            DeterminingIfOrganisationIsTrust(ggCredId, None, None, None)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          val result = performAction()
          status(result)        shouldBe OK
          contentAsString(result) should include(
            messageFromMessageKey("registerTrust.title")
          )
        }

      }
    }

    "handling requests to display the enter a TRN and name page" must {

      val ggCredId = sample[GGCredId]

      def performAction(): Future[Result] =
        controller.enterTrn()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the start endpoint" when {

        def test(hasTrn: Option[Boolean]): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  hasTrn
                )
              )
            )
          }

          val result = performAction()
          checkIsRedirect(
            result,
            cgtpropertydisposalsfrontend.controllers.routes.StartController
              .start()
          )
        }

        "the user has not answered the do you have a TRN question" in {
          test(None)
        }

        "the user has answered that they do not have a TRN" in {
          test(Some(false))
        }

      }

      "display the enter TRN and trust name page" when {

        "the user has indicated that they do have a TRN" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  Some(true)
                )
              )
            )
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(None))
          }

          val result = performAction()
          contentAsString(result) should include(
            messageFromMessageKey("enterTrn.title")
          )
          contentAsString(result) should not include messageFromMessageKey(
            "enterTrn.error.notFound",
            0,
            2
          )
        }

        "the user has indicated that they have a TRN and they have " +
          "previously made unsuccessful attempts to do a name match" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStatus(
                  DeterminingIfOrganisationIsTrust(
                    ggCredId,
                    None,
                    Some(true),
                    Some(true)
                  )
                )
              )
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
                Right(
                  Some(
                    UnsuccessfulNameMatchAttempts(
                      1,
                      2,
                      sample[TrustNameMatchDetails]
                    )
                  )
                )
              )
            }

            val result = performAction()
            contentAsString(result) should include(
              messageFromMessageKey("enterTrn.title")
            )
            contentAsString(result) should not include messageFromMessageKey(
              "enterTrn.error.notFound",
              1,
              2
            )
          }

      }

      "redirect to the too many attempts page" when {

        "the user has tried to do a name match too many times" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  Some(true)
                )
              )
            )
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Left(TooManyUnsuccessfulAttempts())
            )
          }

          checkIsRedirect(
            performAction(),
            onboardingRoutes.DeterminingIfOrganisationIsTrustController
              .tooManyAttempts()
          )
        }

      }

      "show an error page" when {

        "there is an error trying to see how many times a user has attempted to do a name match" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  Some(true)
                )
              )
            )
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Left(NameMatchServiceError.BackendError(Error("")))
            )
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

    }

    "handling submitted TRN's and trust names" must {

      val validTrn = TRN("123456789012345")

      val validTrustName = TrustName("Some trust 123")

      val bpr = sample[BusinessPartnerRecord].copy(name = Left(validTrustName))

      val ggCredId = sample[GGCredId]

      val expectedSessionData =
        sessionDataWithStatus(
          DeterminingIfOrganisationIsTrust(
            ggCredId,
            None,
            Some(true),
            Some(true)
          )
        )

      val previousUnsuccessfulNameMatchAttempt =
        UnsuccessfulNameMatchAttempts(1, 3, sample[TrustNameMatchDetails])

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterTrnSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the start endpoint" when {

        def test(hasTrn: Option[Boolean]): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStatus(
                DeterminingIfOrganisationIsTrust(
                  ggCredId,
                  None,
                  Some(true),
                  hasTrn
                )
              )
            )
          }

          val result = performAction()
          checkIsRedirect(
            result,
            cgtpropertydisposalsfrontend.controllers.routes.StartController
              .start()
          )
        }

        "the user has not answered the do you have a TRN question" in {
          test(None)
        }

        "the user has answered that they do not have a TRN" in {
          test(Some(false))
        }

      }

      "show a form error" when {

        def mockActions(): Unit =
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Right(
                Some(
                  UnsuccessfulNameMatchAttempts(
                    1,
                    2,
                    TrustNameMatchDetails(validTrustName, validTrn)
                  )
                )
              )
            )
          }

        "an TRN is not submitted" in {
          mockActions()

          val result = performAction("trustName" -> validTrustName.value)
          status(result)        shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("trn.error.required")
          )
        }

        "the TRN is an empty string" in {
          mockActions()

          val result =
            performAction("trustName" -> validTrustName.value, "trn" -> "")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("trn.error.required")
          )
        }

        "the TRN is non empty but less than 15 characters" in {
          val trn = "1" + (" " * 15) + "23"
          mockActions()

          val result =
            performAction("trustName" -> validTrustName.value, "trn" -> trn)
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("trn.error.tooShort")
          )
          // make sure trn is displayed as submitted by the user
          contentAsString(result) should include(trn)
        }

        "the TRN is more than 15 characters" in {
          mockActions()

          val result = performAction(
            "trustName" -> validTrustName.value,
            "trn"       -> ("1" * 16)
          )
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("trn.error.tooLong")
          )
        }

        "the TRN contains invalid characters" in {
          mockActions()

          val result = performAction(
            "trustName" -> validTrustName.value,
            "trn"       -> ("?" * 15)
          )
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("trn.error.pattern")
          )
        }

        "a trust name is not submitted" in {
          mockActions()

          val result = performAction("trn" -> validTrn.value)
          status(result)        shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("trustName.error.required")
          )
        }

        "a trust name is submitted but it is an empty string" in {
          mockActions()

          val result =
            performAction("trustName" -> "", "trn" -> validTrn.value)
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("trustName.error.required")
          )
        }

        "a trust name is submitted but it is more than 105 characters" in {
          mockActions()

          val result =
            performAction("trustName" -> ("a" * 106), "trn" -> validTrn.value)
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("trustName.error.tooLong")
          )
        }

        "a trust name is submitted but it contains invalid characters" in {
          mockActions()

          val result =
            performAction("trustName" -> "???", "trn" -> validTrn.value)
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("trustName.error.pattern")
          )
        }

        "a trust name is submitted but it contains colon" in {
          mockActions()

          val result =
            performAction("trustName" -> "trust name:", "trn" -> validTrn.value)
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("trustName.error.pattern")
          )
        }

        "a valid SA UTR and name are entered but they cannot be matched to a BPR and the " +
          "user has not yet made too many unsuccessful attempts" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(expectedSessionData)
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
                Right(Some(previousUnsuccessfulNameMatchAttempt))
              )
              mockAttemptNameMatch(
                validTrn,
                validTrustName,
                ggCredId,
                Some(previousUnsuccessfulNameMatchAttempt),
                lang
              )(
                Left(
                  NameMatchServiceError.NameMatchFailed(
                    UnsuccessfulNameMatchAttempts(
                      2,
                      3,
                      TrustNameMatchDetails(validTrustName, validTrn)
                    )
                  )
                )
              )
            }

            val result = performAction(
              "trustName" -> validTrustName.value,
              "trn"       -> validTrn.value
            )
            status(result) shouldBe BAD_REQUEST
            contentAsString(result) should include(
              messageFromMessageKey("enterTrn.error.notFound", 2, 3)
            )
          }

      }

      "display an error page" when {

        "there is an error retrieving the number of previous unsuccessful attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Left(NameMatchServiceError.BackendError(Error("")))
            )
          }

          val result = performAction()

          checkIsTechnicalErrorPage(result)
        }

        "there is an error getting the BPR" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Right(Some(previousUnsuccessfulNameMatchAttempt))
            )
            mockAttemptNameMatch(
              validTrn,
              validTrustName,
              ggCredId,
              Some(previousUnsuccessfulNameMatchAttempt),
              lang
            )(
              Left(NameMatchServiceError.BackendError(Error("")))
            )
          }

          val result = performAction(
            "trustName" -> validTrustName.value,
            "trn"       -> validTrn.value
          )
          checkIsTechnicalErrorPage(result)
        }

        "there is an error storing a retrieved BPR in mongo" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Right(Some(previousUnsuccessfulNameMatchAttempt))
            )
            mockAttemptNameMatch(
              validTrn,
              validTrustName,
              ggCredId,
              Some(previousUnsuccessfulNameMatchAttempt),
              lang
            )(
              Right(bpr -> BusinessPartnerRecordResponse(Some(bpr), None, None))
            )
            mockStoreSession(
              sessionDataWithStatus(
                SubscriptionMissingData(bpr, None, None, ggCredId, None)
              )
            )(
              Left(Error(""))
            )
          }

          val result = performAction(
            "trustName" -> validTrustName.value,
            "trn"       -> validTrn.value
          )
          checkIsTechnicalErrorPage(result)
        }

        "a BPR is found but it is one for an individual instead of a trust" in {
          val individualBpr = bpr.copy(name = Right(IndividualName("Name", "Wame")))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Right(Some(previousUnsuccessfulNameMatchAttempt))
            )
            mockAttemptNameMatch(
              validTrn,
              validTrustName,
              ggCredId,
              Some(previousUnsuccessfulNameMatchAttempt),
              lang
            )(
              Right(
                individualBpr -> BusinessPartnerRecordResponse(Some(individualBpr), None, None)
              )
            )
          }

          val result = performAction(
            "trustName" -> validTrustName.value,
            "trn"       -> validTrn.value
          )
          checkIsTechnicalErrorPage(result)

        }

      }

      "update the session and redirect to the start endpoint" when {

        "the user submits a valid TRN and trust name and a BPR can be retrieved and the " +
          "user does not already have a cgt reference" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(expectedSessionData)
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
                Right(Some(previousUnsuccessfulNameMatchAttempt))
              )
              mockAttemptNameMatch(
                validTrn,
                validTrustName,
                ggCredId,
                Some(previousUnsuccessfulNameMatchAttempt),
                lang
              )(
                Right(bpr -> BusinessPartnerRecordResponse(Some(bpr), None, None))
              )
              mockStoreSession(
                sessionDataWithStatus(
                  SubscriptionMissingData(bpr, None, None, ggCredId, None)
                )
              )(Right(()))
            }

            val result = performAction(
              "trustName" -> validTrustName.value,
              "trn"       -> validTrn.value
            )
            checkIsRedirect(
              result,
              cgtpropertydisposalsfrontend.controllers.routes.StartController
                .start()
            )
          }

        "the user submits a valid TRN and trust name and a BPR can be retrieved and a " +
          "new enrolment has been created for the user" in {
            val cgtReference      = sample[CgtReference]
            val subscribedDetails = sample[SubscribedDetails]

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(expectedSessionData)
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
                Right(Some(previousUnsuccessfulNameMatchAttempt))
              )
              mockAttemptNameMatch(
                validTrn,
                validTrustName,
                ggCredId,
                Some(previousUnsuccessfulNameMatchAttempt),
                lang
              )(
                Right(bpr -> BusinessPartnerRecordResponse(Some(bpr), Some(cgtReference), Some(subscribedDetails)))
              )
              mockStoreSession(
                sessionDataWithStatus(
                  NewEnrolmentCreatedForMissingEnrolment(subscribedDetails, ggCredId)
                )
              )(Right(()))
            }

            val result = performAction(
              "trustName" -> validTrustName.value,
              "trn"       -> validTrn.value
            )
            checkIsRedirect(
              result,
              cgtpropertydisposalsfrontend.controllers.routes.StartController
                .start()
            )
          }

      }

      "update the session and redirect to the already subscribed page" when {

        "the user submits a valid TRN and trust name and a BPR can be retrieved and the user already has a cgt reference" in {
          val cgtReference = sample[CgtReference]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Right(Some(previousUnsuccessfulNameMatchAttempt))
            )
            mockAttemptNameMatch(
              validTrn,
              validTrustName,
              ggCredId,
              Some(previousUnsuccessfulNameMatchAttempt),
              lang
            )(
              Right(bpr -> BusinessPartnerRecordResponse(Some(bpr), Some(cgtReference), None))
            )
            mockStoreSession(
              sessionDataWithStatus(
                AlreadySubscribedWithDifferentGGAccount(
                  ggCredId,
                  Some(cgtReference)
                )
              )
            )(Right(()))
          }

          val result = performAction(
            "trustName" -> validTrustName.value,
            "trn"       -> validTrn.value
          )
          checkIsRedirect(
            result,
            onboardingRoutes.SubscriptionController
              .alreadySubscribedWithDifferentGGAccount()
          )
        }

      }

      "redirect to the too many attempts page" when {

        "the user has attempted to perform a name match too many times" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Left(NameMatchServiceError.TooManyUnsuccessfulAttempts())
            )
          }

          checkIsRedirect(
            performAction(),
            onboardingRoutes.DeterminingIfOrganisationIsTrustController
              .tooManyAttempts()
          )
        }

        "the user has not initially made too many unsuccessful attempts but submits " +
          "details which do not match a BPR and have now made too many attempts" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(expectedSessionData)
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
                Right(Some(previousUnsuccessfulNameMatchAttempt))
              )
              mockAttemptNameMatch(
                validTrn,
                validTrustName,
                ggCredId,
                Some(previousUnsuccessfulNameMatchAttempt),
                lang
              )(
                Left(NameMatchServiceError.TooManyUnsuccessfulAttempts())
              )
            }

            checkIsRedirect(
              performAction(
                "trustName" -> validTrustName.value,
                "trn"       -> validTrn.value
              ),
              onboardingRoutes.DeterminingIfOrganisationIsTrustController
                .tooManyAttempts()
            )
          }

      }

      "be able to handle submitted TRN's with spaces" in {
        val validTrnWithSpaces = TRN("1234567890     12 345")
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(expectedSessionData)
          mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
            Right(Some(previousUnsuccessfulNameMatchAttempt))
          )
          mockAttemptNameMatch(
            validTrnWithSpaces,
            validTrustName,
            ggCredId,
            Some(previousUnsuccessfulNameMatchAttempt),
            lang
          )(
            Right(bpr -> BusinessPartnerRecordResponse(Some(bpr), None, None))
          )
          mockStoreSession(
            sessionDataWithStatus(
              SubscriptionMissingData(bpr, None, None, ggCredId, None)
            )
          )(Right(()))
        }

        val result = performAction(
          "trustName" -> validTrustName.value,
          "trn"       -> validTrnWithSpaces.value
        )
        checkIsRedirect(
          result,
          cgtpropertydisposalsfrontend.controllers.routes.StartController
            .start()
        )

      }

    }

    "handling requests to display the too many attempts page" must {

      def performAction(): Future[Result] = controller.tooManyAttempts()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      val expectedSessionData =
        sessionDataWithStatus(
          DeterminingIfOrganisationIsTrust(
            ggCredId,
            None,
            Some(true),
            Some(true)
          )
        )

      "display an error page" when {

        "there is an error trying to get the number of unsuccessful name match attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(expectedSessionData)
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Left(NameMatchServiceError.BackendError(Error("")))
            )
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "display the too many attempts page" when {

          "the user has attempted a name match too many times" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(expectedSessionData)
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
                Left(NameMatchServiceError.TooManyUnsuccessfulAttempts())
              )
            }

            val result = performAction()
            status(result)        shouldBe OK
            contentAsString(result) should include(
              messageFromMessageKey("enterTrn.tooManyAttempts.title")
            )
          }
        }

        "redirect to the enter TRN and trust name page" when {

          "the user has attempted a name match but they still have attempts left" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(expectedSessionData)
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
                Right(
                  Some(
                    UnsuccessfulNameMatchAttempts(
                      1,
                      2,
                      sample[TrustNameMatchDetails]
                    )
                  )
                )
              )
            }

            checkIsRedirect(
              performAction(),
              onboardingRoutes.DeterminingIfOrganisationIsTrustController
                .enterTrn()
            )
          }

          "the user has not attempted a name match" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(expectedSessionData)
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(None))
            }

            checkIsRedirect(
              performAction(),
              onboardingRoutes.DeterminingIfOrganisationIsTrustController
                .enterTrn()
            )
          }

        }

      }

    }

  }

}
