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

import cats.data.EitherT
import cats.instances.future._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.CSRFTokenHelper._
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.NameMatchError.TooManyUnsuccessfulAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.{BusinessPartnerRecord, NameMatchError, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails.TrustNameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{GGCredId, TRN}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordNameMatchRetryService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class DeterminingIfOrganisationIsTrustControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockBprNameMatchService = mock[BusinessPartnerRecordNameMatchRetryService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[BusinessPartnerRecordNameMatchRetryService].toInstance(mockBprNameMatchService),
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[DeterminingIfOrganisationIsTrustController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi


  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    behave like redirectToStartWhenInvalidJourney(
      performAction, {
        case _: DeterminingIfOrganisationIsTrust => true
        case _                 => false
      }
    )

  def sessionDataWithStatus(journeyStatus: JourneyStatus): SessionData =
    SessionData.empty.copy(journeyStatus = Some(journeyStatus))
  def mockGetNumberOfUnsuccessfulAttempts(
                                           ggCredId: GGCredId
                                         )(result: Either[NameMatchError[TrustNameMatchDetails], Option[UnsuccessfulNameMatchAttempts[TrustNameMatchDetails]]]) =
    (mockBprNameMatchService
      .getNumberOfUnsuccessfulAttempts[TrustNameMatchDetails](_: GGCredId)(_: Reads[TrustNameMatchDetails], _: ExecutionContext))
      .expects(ggCredId, *, *)
      .returning(EitherT.fromEither[Future](result))

  def mockAttemptNameMatch(
                            trn: TRN,
                            name: TrustName,
                            ggCredId: GGCredId,
                            previousUnsuccessfulNameMatchAttempts: Option[UnsuccessfulNameMatchAttempts[TrustNameMatchDetails]]
                          )(result: Either[NameMatchError[TrustNameMatchDetails], BusinessPartnerRecord]) =
    (
      mockBprNameMatchService
        .attemptBusinessPartnerRecordNameMatch[TrustNameMatchDetails](
          _: TrustNameMatchDetails,
          _: GGCredId,
          _: Option[UnsuccessfulNameMatchAttempts[TrustNameMatchDetails]]
        )(
          _: Writes[TrustNameMatchDetails],
          _: ExecutionContext,
          _: HeaderCarrier
        )
      )
      .expects(TrustNameMatchDetails(name, trn), ggCredId, previousUnsuccessfulNameMatchAttempts, *, *, *)
      .returning(EitherT.fromEither[Future](result))

  val ggCredId = sample[GGCredId]

  "DeterminingIfOrganisationIsTrustController" when {

    "handling requests to display the 'do you want to report for a trust' page" must {

      def performAction(): Future[Result] =
        controller.doYouWantToReportForATrust()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "show the page" when {

        "the user has not selected an option before" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, None, None))
            ))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("isReportingForATrust.title"))
        }

        "the user has selected an option before" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), None))
            ))))
          }

          val result = performAction()
          val content = contentAsString(result)

          status(result) shouldBe OK
          content should include(message("isReportingForATrust.title"))
          content should include("checked=\"checked\"")
        }

      }

    }

    "handling submitted answers from the 'do you want to report for a trust' page" must {

      def performAction(formData: (String,String)*): Future[Result] =
        controller.doYouWantToReportForATrustSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction())

      "show a form error" when {

        "an option has not been selected" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, None, None))
            ))))
          }

          val result = performAction()
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("isReportingForATrust.error.required"))
        }

        "the data submitted cannot be read" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, None, None))
            ))))
          }

          val result = performAction("isReportingForATrust" -> "123")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("isReportingForATrust.error.boolean"))
        }

      }

      "show an error page" when {

        "the answer cannot be stored in mongo" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, None, None))
            ))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), None))
              )
            )(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction("isReportingForATrust" -> "true"))
        }

      }

      "redirect to the 'report with corporate tax' page" when {

        "the answer says they are not reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, None, None))
            ))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(DeterminingIfOrganisationIsTrust(ggCredId, Some(false), None))
              )
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction("isReportingForATrust" -> "false"),
            routes.DeterminingIfOrganisationIsTrustController.reportWithCorporateTax()
          )
        }


      }

      "redirect to the 'do you have a trn' page" when {

        "the answer says they are reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, None, None))
            ))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), None))
              )
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction("isReportingForATrust" -> "true"),
            routes.DeterminingIfOrganisationIsTrustController.doYouHaveATrn()
          )
        }
      }

    }

    "handling requests to display the 'report with corporate tax' page" must {

      def performAction(): Future[Result] =
        controller.reportWithCorporateTax()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the start endpoint" when {

        "the user has not answered whether or not they're reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, None, None))
            ))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

        "the user has said that they're reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), None))
            ))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

      }

      "show the page" when {

        "the user has said that they're not reporting for a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(false), None))
            ))))
          }

          val result = performAction()
          status(result) shouldBe 200
          contentAsString(result) should include(message("reportCorpTax.title"))
        }

      }

    }

    "handling requests to display the 'do you have a TRN' page" must {

      def performAction(): Future[Result] =
        controller.doYouHaveATrn()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the start endpoint" when {

        "the user has not indicated whether or not they are reporting for a trust" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, None, None))
            ))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

        "the user has indicated that they are not reporting for a trust" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(false), None))
            ))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

      }

      "show the page" when {

        "the user has not selected an option before" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), None))
            ))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("haveATrn.title"))
        }

        "the user has selected an option before" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), Some(true)))
            ))))
          }

          val result = performAction()
          val content = contentAsString(result)

          status(result) shouldBe OK
          content should include(message("haveATrn.title"))
          content should include("checked=\"checked\"")
        }

      }

    }

    "handling submitted answers from the 'do you have a TRN' page" must {

      def performAction(formData: (String,String)*): Future[Result] =
        controller.doYouHaveATrnSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the start endpoint" when {

        "the user has not indicated whether or not they are reporting for a trust" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, None, None))
            ))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

        "the user has indicated that they are not reporting for a trust" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(false), None))
            ))))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }

      }

      "show a form error" when {

        "an option has not been selected" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), None))
            ))))
          }

          val result = performAction()
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("hasTrn.error.required"))
        }

        "the data submitted cannot be read" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), None))
            ))))
          }

          val result = performAction("hasTrn" -> "123")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("hasTrn.error.boolean"))
        }

      }

      "show an error page" when {

        "the answer cannot be stored in mongo" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), None))
            ))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), Some(true)))
              )
            )(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction("hasTrn" -> "true"))
        }

      }

      "redirect to the 'register your trust' page" when {

        "the user does not have a TRN" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), None))
            ))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), Some(false)))
              )
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction("hasTrn" -> "false"),
            routes.DeterminingIfOrganisationIsTrustController.registerYourTrust()
          )
        }


      }

      "redirect to the 'enter trn' page" when {

        "the user does  have a TRN" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), None))
            ))))
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), Some(true)))
              )
            )(Future.successful(Right(())))
          }

          checkIsRedirect(
            performAction("hasTrn" -> "true"),
            routes.DeterminingIfOrganisationIsTrustController.enterTrn()
          )
        }
      }

    }

    "handling requests to display the register your trust page" must {

      def performAction(): Future[Result] = controller.registerYourTrust()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "show the register your trust page" when {

        "the session data indicates the user is an organisation which is not associated with a registered trust" in {
          val sessionData = sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, None, None))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result)          shouldBe OK
          contentAsString(result) should include(message("registerTrust.title"))
        }

      }
    }

    "handling requests to display the enter a TRN and name page" must {

      val ggCredId = sample[GGCredId]

      def performAction(): Future[Result] =
        controller.enterTrn()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the start endpoint" when {

        def test(hasTrn: Option[Boolean]) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                 Some(sessionDataWithStatus(
                    DeterminingIfOrganisationIsTrust(ggCredId, Some(true), hasTrn))
                )
              ))
            )
          }

          val result = performAction()
          checkIsRedirect(result, routes.StartController.start())
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
              Future.successful(
                Right(
                  Some(
                   sessionDataWithStatus(
                      DeterminingIfOrganisationIsTrust(ggCredId, Some(true), Some(true))
                    )
                  )
                )
              )
            )
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(None))
          }

          val result = performAction()
          contentAsString(result) should include(message("enterTrn.title"))
          contentAsString(result) should not include (message("enterTrn.error.notFound", 0, 2))
        }

        "the user has indicated that they have a TRN and they have " +
          "previously made unsuccessful attempts to do a name match" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionDataWithStatus(
                      DeterminingIfOrganisationIsTrust(ggCredId, Some(true), Some(true))
                    )
                  )
                )
              )
            )
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
              Right(
                Some(
                  UnsuccessfulNameMatchAttempts(1, 2, sample[TrustNameMatchDetails])
                )
              )
            )
          }

          val result = performAction()
          contentAsString(result) should include(message("enterTrn.title"))
          contentAsString(result) should include(message("enterTrn.error.notFound", 1, 2))
        }

      }

      "redirect to the too many attempts page" when {

        "the user has tried to do a name match too many times" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionDataWithStatus(
                      DeterminingIfOrganisationIsTrust(ggCredId, Some(true), Some(true))
                    )
                  )
                )
              )
            )
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Left(TooManyUnsuccessfulAttempts())
            )
          }

          checkIsRedirect(performAction(), routes.DeterminingIfOrganisationIsTrustController.tooManyAttempts())
        }

      }

      "show an error page" when {

        "there is an error trying to see how many times a user has attempted to do a name match" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Future.successful(
                Right(
                  Some(
                    sessionDataWithStatus(
                      DeterminingIfOrganisationIsTrust(ggCredId, Some(true), Some(true))
                    )
                  )
                )
              )
            )
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Left(NameMatchError.BackendError(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

    }

    "handling submitted TRN's and trust names" must {

      val validTrn = TRN("123456789012345")

      val validTrustName = TrustName("Some trust")

      val bpr = sample[BusinessPartnerRecord].copy(name = Left(validTrustName))

      val ggCredId = sample[GGCredId]

      val expectedSessionData =
        sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), Some(true)))

      val previousUnsuccessfulNameMatchAttempt =
        UnsuccessfulNameMatchAttempts(1, 3, sample[TrustNameMatchDetails])

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterTrnSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the start endpoint" when {

        "redirect to the start endpoint" when {

          def test(hasTrn: Option[Boolean]) = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                Future.successful(
                  Right(
                    Some(sessionDataWithStatus(
                      DeterminingIfOrganisationIsTrust(ggCredId, Some(true), hasTrn))
                    )
                  ))
              )
            }

            val result = performAction()
            checkIsRedirect(result, routes.StartController.start())
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
              mockGetSession(Future.successful(Right(Some(expectedSessionData))))
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
                Right(Some(UnsuccessfulNameMatchAttempts(1, 2, TrustNameMatchDetails(validTrustName, validTrn))))
              )
            }


          "an TRN is not submitted" in {
            mockActions()

            val result = performAction("trustName" -> validTrustName.value)
            status(result) shouldBe BAD_REQUEST
            contentAsString(result) should include(message("trn.error.required"))
          }

          "the TRN is an empty string" in {
            mockActions()

            val result = performAction("trustName" -> validTrustName.value, "trn" -> "")
            status(result) shouldBe BAD_REQUEST
            contentAsString(result) should include(message("trn.error.required"))
          }

          "the TRN is non empty but less than 15 characters" in {
            val trn = "1" + (" " * 15) + "23"
            mockActions()

            val result = performAction("trustName" -> validTrustName.value, "trn" -> trn)
            status(result) shouldBe BAD_REQUEST
            contentAsString(result) should include(message("trn.error.tooShort"))
            // make sure trn is displayed as submitted by the user
            contentAsString(result) should include(trn)
          }

          "the TRN is more than 15 characters" in {
            mockActions()

            val result = performAction("trustName" -> validTrustName.value, "trn" -> ("1" * 16))
            status(result) shouldBe BAD_REQUEST
            contentAsString(result) should include(message("trn.error.tooLong"))
          }


          "the TRN contains invalid characters" in {
            mockActions()

            val result = performAction("trustName" -> validTrustName.value, "trn" -> ("?" * 15))
            status(result) shouldBe BAD_REQUEST
            contentAsString(result) should include(message("trn.error.pattern"))
          }

          "a trust name is not submitted" in {
            mockActions()

            val result = performAction("trn" -> validTrn.value)
            status(result) shouldBe BAD_REQUEST
            contentAsString(result) should include(message("trustName.error.required"))
          }

          "a trust name is submitted but it is an empty string" in {
            mockActions()

            val result = performAction("trustName" -> "", "trn" -> validTrn.value)
            status(result) shouldBe BAD_REQUEST
            contentAsString(result) should include(message("trustName.error.required"))
          }

          "a trust name is submitted but it is more than 105 characters" in {
            mockActions()

            val result = performAction("trustName" -> ("a" * 106), "trn" -> validTrn.value)
            status(result) shouldBe BAD_REQUEST
            contentAsString(result) should include(message("trustName.error.tooLong"))
          }

          "a trust name is submitted but it contains invalid characters" in {
            mockActions()

            val result = performAction("trustName" -> "???", "trn" -> validTrn.value)
            status(result) shouldBe BAD_REQUEST
            contentAsString(result) should include(message("trustName.error.pattern"))
          }



          "a valid SA UTR and name are entered but they cannot be matched to a BPR and the " +
            "user has not yet made too many unsuccessful attempts" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(expectedSessionData))))
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
              mockAttemptNameMatch(validTrn, validTrustName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
                Left(NameMatchError.NameMatchFailed(UnsuccessfulNameMatchAttempts(2, 3, TrustNameMatchDetails(validTrustName, validTrn))))
              )
            }

            val result = performAction("trustName" -> validTrustName.value, "trn" -> validTrn.value)
            status(result) shouldBe BAD_REQUEST
            contentAsString(result) should include(message("enterTrn.error.notFound", 2, 3))
          }

        }

        "display an error page" when {

          "there is an error retrieving the number of previous unsuccessful attempts" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(expectedSessionData))))
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Left(NameMatchError.BackendError(Error(""))))
            }

            val result = performAction()

            checkIsTechnicalErrorPage(result)
          }

          "there is an error getting the BPR" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(expectedSessionData))))
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
              mockAttemptNameMatch(validTrn, validTrustName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
                Left(NameMatchError.BackendError(Error("")))
              )
            }

            val result = performAction("trustName" -> validTrustName.value, "trn" -> validTrn.value)
            checkIsTechnicalErrorPage(result)
          }

          "there is an error storing a retrieved BPR in mongo" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(expectedSessionData))))
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
              mockAttemptNameMatch(validTrn, validTrustName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
                Right(bpr)
              )
              mockStoreSession(sessionDataWithStatus(SubscriptionMissingData(bpr, None, ggCredId)))(Future.successful(Left(Error(""))))
            }

            val result = performAction("trustName" -> validTrustName.value, "trn" -> validTrn.value)
            checkIsTechnicalErrorPage(result)
          }

          "a BPR is found but it is one for an individual instead of a trust" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(expectedSessionData))))
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
              mockAttemptNameMatch(validTrn, validTrustName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
                Right(bpr.copy(name = Right(IndividualName("Name", "Wame"))))
              )
            }

            val result = performAction("trustName" -> validTrustName.value, "trn" -> validTrn.value)
            checkIsTechnicalErrorPage(result)

          }

        }

        "update the session and redirect to the start endpoint" when {

          "the user submits a valid TRN and trust name and a BPR can be retrieved" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(expectedSessionData))))
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
              mockAttemptNameMatch(validTrn, validTrustName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
                Right(bpr)
              )
              mockStoreSession(sessionDataWithStatus(SubscriptionMissingData(bpr, None, ggCredId)))(Future.successful(Right(())))
            }

            val result = performAction("trustName" -> validTrustName.value, "trn" -> validTrn.value)
            checkIsRedirect(result, routes.StartController.start())
          }

        }

        "redirect to the too many attempts page" when {

          "the user has attempted to perform a name match too many times" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(expectedSessionData))))
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Left(NameMatchError.TooManyUnsuccessfulAttempts()))
            }

            checkIsRedirect(performAction(), routes.DeterminingIfOrganisationIsTrustController.tooManyAttempts())
          }

          "the user has not initially made too many unsuccessful attempts but submits " +
            "details which do not match a BPR and have now made too many attempts" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(expectedSessionData))))
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
              mockAttemptNameMatch(validTrn, validTrustName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
                Left(NameMatchError.TooManyUnsuccessfulAttempts())
              )
            }

            checkIsRedirect(
              performAction("trustName" -> validTrustName.value, "trn" -> validTrn.value),
              routes.DeterminingIfOrganisationIsTrustController.tooManyAttempts()
            )
          }

        }

        "be able to handle submitted TRN's with spaces" in {
          val validTrnWithSpaces = TRN("1234567890     12 345" )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(expectedSessionData))))
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(Some(previousUnsuccessfulNameMatchAttempt)))
            mockAttemptNameMatch(validTrnWithSpaces, validTrustName, ggCredId, Some(previousUnsuccessfulNameMatchAttempt))(
              Right(bpr)
            )
            mockStoreSession(sessionDataWithStatus(SubscriptionMissingData(bpr, None, ggCredId)))(Future.successful(Right(())))
          }

          val result = performAction("trustName" -> validTrustName.value, "trn" -> validTrnWithSpaces.value)
          checkIsRedirect(result, routes.StartController.start())


        }

      }
    }


    "handling requests to display the too many attempts page" must {

      def performAction() = controller.tooManyAttempts()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      val expectedSessionData = sessionDataWithStatus(DeterminingIfOrganisationIsTrust(ggCredId, Some(true), Some(true)))

      "display an error page" when {

        "there is an error trying to get the number of unsuccessful name match attempts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(expectedSessionData))))
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Left(NameMatchError.BackendError(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "display the too many attempts page" when {

          "the user has attempted a name match too many times" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(expectedSessionData))))
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Left(NameMatchError.TooManyUnsuccessfulAttempts()))
            }

            val result = performAction()
            status(result)          shouldBe OK
            contentAsString(result) should include(message("enterTrn.tooManyAttempts.title"))
          }
        }

        "redirect to the enter TRN and trust name page" when {

          "the user has attempted a name match but they still have attempts left" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(expectedSessionData))))
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
                Right(Some(UnsuccessfulNameMatchAttempts(1, 2, sample[TrustNameMatchDetails])))
              )
            }

            checkIsRedirect(performAction(), routes.DeterminingIfOrganisationIsTrustController.enterTrn())
          }

          "the user has not attempted a name match" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(expectedSessionData))))
              mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(None))
            }

            checkIsRedirect(performAction(), routes.DeterminingIfOrganisationIsTrustController.enterTrn())
          }

        }

      }

    }



  }

}
