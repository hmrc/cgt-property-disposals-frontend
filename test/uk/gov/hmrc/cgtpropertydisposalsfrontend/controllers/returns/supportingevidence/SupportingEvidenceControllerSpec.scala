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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.supportingevidence
import cats.data.EitherT
import cats.instances.future._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.{CompleteSupportingEvidenceAnswers, IncompleteSupportingEvidenceAnswers, SupportingEvidence}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftSingleDisposalReturn, SupportingEvidenceAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanCallBack.{UpscanFailure, UpscanSuccess}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanUploadStatus.Uploaded
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UploadReference, UpscanUpload}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.upscan.UpscanService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SupportingEvidenceControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  val mockUpscanService: UpscanService = mock[UpscanService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[UpscanService].toInstance(mockUpscanService)
    )

  lazy val controller = instanceOf[SupportingEvidenceController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def mockUpscanInitiate(errorRedirect: Call)(
    result: Either[Error, UpscanUpload]
  ) =
    (mockUpscanService
      .initiate(_: Call, _: UploadReference => Call)(_: HeaderCarrier))
      .expects(errorRedirect, *, *)
      .returning(EitherT.fromEither[Future](result))

  def mockGetUpscanUpload(uploadReference: UploadReference)(
    result: Either[Error, UpscanUpload]
  ) =
    (mockUpscanService
      .getUpscanUpload(_: UploadReference)(_: HeaderCarrier))
      .expects(uploadReference, *)
      .returning(EitherT.fromEither[Future](result))

  def mockUpdateUpscanUpload(uploadReference: UploadReference, upscanUpload: UpscanUpload)(
    result: Either[Error, Unit]
  ) =
    (mockUpscanService
      .updateUpscanUpload(_: UploadReference, _: UpscanUpload)(_: HeaderCarrier))
      .expects(uploadReference, upscanUpload, *)
      .returning(EitherT.fromEither[Future](result))

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  def sessionWithSingleDisposalState(
    supportingEvidenceAnswers: Option[SupportingEvidenceAnswers]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = sample[DraftSingleDisposalReturn].copy(
      supportingEvidenceAnswers = supportingEvidenceAnswers
    )
    val journey = sample[FillingOutReturn].copy(draftReturn = draftReturn)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftReturn
    )
  }

  def sessionWithMultipleDisposalsState(
    supportingEvidenceAnswers: Option[SupportingEvidenceAnswers]
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn = sample[DraftMultipleDisposalsReturn].copy(
      supportingEvidenceAnswers = supportingEvidenceAnswers
    )
    val journey = sample[FillingOutReturn].copy(draftReturn = draftReturn)
    (
      SessionData.empty.copy(
        journeyStatus = Some(journey)
      ),
      journey,
      draftReturn
    )
  }

  def testFormError(
    data: (String, String)*
  )(expectedErrorMessageKey: String, errorArgs: Seq[String] = Nil)(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionWithSingleDisposalState(
      Some(sample[CompleteSupportingEvidenceAnswers])
    )._1
  ): Unit = {
    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(currentSession)
    }
    checkPageIsDisplayed(
      performAction(data),
      messageFromMessageKey(pageTitleKey, titleArgs: _*), { doc =>
        doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey,
          errorArgs: _*
        )
        doc.title() should startWith("Error:")
      },
      BAD_REQUEST
    )
  }

  "Supporting Evidence Controller" when {

    "handling requests to display the do you want to upload supporting evidence page" must {

      def performAction(): Future[Result] = controller.doYouWantToUploadSupportingDocuments()(FakeRequest())

      "display the page" when {

        "a user has not answered the question before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                None
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("supporting-evidence.do-you-want-to-upload.title"), { doc =>
              doc.select("#back").attr("href") shouldBe returns.routes.TaskListController.taskList().url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.SupportingEvidenceController
                .doYouWantToUploadSupportingDocuments()
                .url
            }
          )
        }

        "a user has answered the question before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                Some(IncompleteSupportingEvidenceAnswers(Some(false), List.empty, List.empty))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("supporting-evidence.do-you-want-to-upload.title"), { doc =>
              doc.select("#back").attr("href") shouldBe returns.routes.TaskListController.taskList().url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.SupportingEvidenceController
                .doYouWantToUploadSupportingDocuments()
                .url
            }
          )
        }
      }
    }

    "handling submission of do you want to upload supporting evidence page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.doYouWantToUploadSupportingDocumentsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "display the technical error page" when {

        "an error occurs when storing the draft return and answer is true" in {
          val answers =
            IncompleteSupportingEvidenceAnswers.empty.copy(
              doYouWantToUploadSupportingEvidence = Some(true),
              List.empty,
              List.empty
            )

          val (session, journey, draftReturn) = sessionWithSingleDisposalState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              draftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Left(Error("some error")))
          }

          checkIsTechnicalErrorPage(performAction("supporting-evidence.do-you-want-to-upload" -> "true"))
        }

        "an error occurs when storing the draft return and answer is false" in {
          val answers =
            IncompleteSupportingEvidenceAnswers.empty.copy(
              doYouWantToUploadSupportingEvidence = Some(false),
              List.empty,
              List.empty
            )

          val (session, journey, draftReturn) = sessionWithSingleDisposalState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              draftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Left(Error("some error")))
          }

          checkIsTechnicalErrorPage(performAction("supporting-evidence.do-you-want-to-upload" -> "false"))
        }
      }

      "show a form error" when {

        val answers = IncompleteSupportingEvidenceAnswers.empty

        val currentSession = sessionWithSingleDisposalState(
          Some(answers)
        )._1

        def test(data: (String, String)*)(expectedErrorMessageKey: String) =
          testFormError(data: _*)(expectedErrorMessageKey)("supporting-evidence.do-you-want-to-upload.title")(
            performAction,
            currentSession
          )

        "a user has not chosen an answer" in {
          test()("supporting-evidence.do-you-want-to-upload.error.required")
        }
      }

      "redirect to check your answers page" when {

        "the user has never answered question before and has answered yes and is on a single disposable journey" in {

          val (session, journey, draftReturn) = sessionWithSingleDisposalState(None)

          val updatedDraftReturn =
            draftReturn.copy(supportingEvidenceAnswers =
              Some(IncompleteSupportingEvidenceAnswers(Some(true), List.empty, List.empty))
            )

          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction("supporting-evidence.do-you-want-to-upload" -> "true"),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }

        "the user has answered yes to the question before and has answered no and is on a single disposable journey" in {

          val answers =
            sample[IncompleteSupportingEvidenceAnswers].copy(doYouWantToUploadSupportingEvidence = Some(true))

          val (session, journey, draftReturn) = sessionWithSingleDisposalState(Some(answers))

          val updatedDraftReturn =
            draftReturn.copy(supportingEvidenceAnswers =
              Some(
                answers.copy(
                  doYouWantToUploadSupportingEvidence = Some(false),
                  evidences                           = List.empty,
                  expiredEvidences                    = List.empty
                )
              )
            )

          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction("supporting-evidence.do-you-want-to-upload" -> "false"),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }

        "the user has answered yes to the question before and has answered yes and is on a single disposable journey" in {

          val answers =
            sample[IncompleteSupportingEvidenceAnswers].copy(doYouWantToUploadSupportingEvidence = Some(true))

          val (session, journey, draftReturn) = sessionWithSingleDisposalState(Some(answers))

          val updatedDraftReturn = draftReturn

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
          }

          checkIsRedirect(
            performAction("supporting-evidence.do-you-want-to-upload" -> "true"),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }

        "the user has answered no to the question before and has answered yes and is on a single disposable journey" in {

          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(false),
            evidences                           = List.empty,
            expiredEvidences                    = List.empty
          )

          val (session, journey, draftReturn) = sessionWithSingleDisposalState(Some(answers))

          val updatedDraftReturn =
            draftReturn.copy(supportingEvidenceAnswers =
              Some(
                sample[IncompleteSupportingEvidenceAnswers]
                  .copy(
                    doYouWantToUploadSupportingEvidence = Some(true),
                    evidences                           = List.empty,
                    expiredEvidences                    = List.empty
                  )
              )
            )

          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction("supporting-evidence.do-you-want-to-upload" -> "true"),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }

      }
    }
  }

  "handling requests to upload supporting evidence" must {

    "show check your answers page" when {

      "the number of uploads has reached maximum allowed" in {

        def performAction(): Future[Result] = controller.uploadSupportingEvidence()(FakeRequest())

        val supportingEvidence = sample[SupportingEvidence]

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(true),
          evidences                           = List.fill(2)(supportingEvidence),
          expiredEvidences                    = List.empty
        )

        val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.SupportingEvidenceController.checkYourAnswers()
        )
      }
    }

    "show technical error page" when {

      "upscan initiate call fails" in {
        def performAction(): Future[Result] = controller.uploadSupportingEvidence()(FakeRequest())

        val supportingEvidence = sample[SupportingEvidence]

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(false),
          evidences                           = List.fill(0)(supportingEvidence),
          expiredEvidences                    = List.empty
        )

        val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockUpscanInitiate(routes.SupportingEvidenceController.uploadSupportingEvidenceError())(
            Left(Error("some upscan error"))
          )
        }

        checkIsTechnicalErrorPage(performAction())

      }

    }

    "show file upload page" when {

      "number of uploads has not exceeded limit" in {
        def performAction(): Future[Result] = controller.uploadSupportingEvidence()(FakeRequest())

        val supportingEvidence = sample[SupportingEvidence]

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(false),
          evidences                           = List.fill(0)(supportingEvidence),
          expiredEvidences                    = List.empty
        )

        val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

        val upscanUpload = sample[UpscanUpload]

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockUpscanInitiate(routes.SupportingEvidenceController.uploadSupportingEvidenceError())(Right(upscanUpload))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("supporting-evidence.upload.title"),
          doc =>
            doc.select("#back").attr("href") shouldBe routes.SupportingEvidenceController
              .doYouWantToUploadSupportingDocuments()
              .url
        )
      }

    }

  }

  "handling upscan call back" must {

    "show the technical error " when {

      "the file upload failed" in {

        def performAction(): Future[Result] = controller.uploadSupportingEvidenceError()(FakeRequest())

        val supportingEvidence = sample[SupportingEvidence]

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(true),
          evidences                           = List.fill(2)(supportingEvidence),
          expiredEvidences                    = List.empty
        )

        val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsTechnicalErrorPage(performAction())
      }
    }
  }

  "handling expired evidence" must {

    "show technical error page" when {

      "update of draft fails" in {

        def performAction(): Future[Result] = controller.supportingEvidenceExpiredSubmit()(FakeRequest())

        val supportingEvidence = sample[SupportingEvidence]

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(true),
          evidences                           = List.fill(1)(supportingEvidence),
          expiredEvidences                    = List(supportingEvidence)
        )

        val (session, journey, draftReturn) = sessionWithSingleDisposalState(Some(answers))

        val updatedDraftReturn =
          draftReturn.copy(supportingEvidenceAnswers =
            Some(
              answers.copy(
                doYouWantToUploadSupportingEvidence = Some(true),
                expiredEvidences                    = List.empty
              )
            )
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(
            updatedDraftReturn,
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Left(Error("update failed")))
        }

        checkIsTechnicalErrorPage(
          performAction()
        )
      }
    }

    "redirect to check your answers" when {

      "if answers are complete" in {
        def performAction(): Future[Result] = controller.supportingEvidenceExpired()(FakeRequest())

        val answers = CompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = false,
          evidences                           = List.empty
        )

        val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.SupportingEvidenceController.checkYourAnswers()
        )
      }
    }

    "display supporting evidence expired page" when {

      "supporting evidence has expired" in {
        def performAction(): Future[Result] = controller.supportingEvidenceExpired()(FakeRequest())

        val supportingEvidence = sample[SupportingEvidence]

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(true),
          evidences                           = List.fill(0)(supportingEvidence),
          expiredEvidences                    = List(supportingEvidence)
        )

        val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("supporting-evidence.expired.title")
        )
      }
    }

    "redirect to check your answers " when {

      "evidence has not expired" in {

        def performAction(): Future[Result] = controller.supportingEvidenceExpiredSubmit()(FakeRequest())

        val supportingEvidence = sample[SupportingEvidence]

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(true),
          evidences                           = List.fill(1)(supportingEvidence),
          expiredEvidences                    = List.empty
        )

        val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.SupportingEvidenceController.checkYourAnswers()
        )
      }

      "evidence has expired" in {

        def performAction(): Future[Result] = controller.supportingEvidenceExpiredSubmit()(FakeRequest())

        val supportingEvidence = sample[SupportingEvidence]

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(true),
          evidences                           = List.fill(1)(supportingEvidence),
          expiredEvidences                    = List(supportingEvidence)
        )

        val (session, journey, draftReturn) = sessionWithSingleDisposalState(Some(answers))

        val updatedDraftReturn =
          draftReturn.copy(supportingEvidenceAnswers =
            Some(
              answers.copy(
                doYouWantToUploadSupportingEvidence = Some(true),
                expiredEvidences                    = List.empty
              )
            )
          )

        val updatedSession: SessionData =
          session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(
            updatedDraftReturn,
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Right(()))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(),
          routes.SupportingEvidenceController.checkYourAnswers()
        )
      }

    }
  }

  "handling check your answer" must {

    "display file upload page" when {

      "the user has answered yes to adding supporting evidences" in {
        def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

        val supportingEvidence = sample[SupportingEvidence]

        val answers = IncompleteSupportingEvidenceAnswers.empty.copy(
          doYouWantToUploadSupportingEvidence = Some(true),
          List.empty,
          List.empty
        )

        val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.SupportingEvidenceController.uploadSupportingEvidence()
        )
      }
    }

    "display supporting do you want to upload supporting evidence page" when {

      "the use has never answered this question" in {
        def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

        val answers = IncompleteSupportingEvidenceAnswers.empty

        val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.SupportingEvidenceController.doYouWantToUploadSupportingDocuments()
        )
      }
    }

    "display supporting evidence expired page" when {

      "there are expired supporting evidences" in {
        def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

        val supportingEvidence = sample[SupportingEvidence]
        val answers = IncompleteSupportingEvidenceAnswers.empty.copy(
          doYouWantToUploadSupportingEvidence = Some(false),
          List(supportingEvidence),
          List(supportingEvidence)
        )

        val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(),
          routes.SupportingEvidenceController.supportingEvidenceExpired()
        )
      }
    }

    "display the check your answers page" when {

      "has completed the supporting evidence section" in {
        def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

        val answers = CompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = false,
          List.empty
        )

        val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("supporting-evidence.check-your-answers.title")
        )
      }

      "answered the do you want to upload a file question" in {
        def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

        val answers = IncompleteSupportingEvidenceAnswers.empty.copy(
          doYouWantToUploadSupportingEvidence = Some(false),
          List.empty,
          List.empty
        )

        val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("supporting-evidence.check-your-answers.title")
        )
      }
    }

    def performAction(): Future[Result] = controller.checkYourAnswersSubmit()(FakeRequest())

    behave like redirectToStartBehaviour(() => performAction())

    "display the technical error page" when {

      "an error occurs when storing the draft return" in {

        val answers = IncompleteSupportingEvidenceAnswers.empty.copy(
          doYouWantToUploadSupportingEvidence = Some(false),
          List.empty,
          List.empty
        )

        val updatedAnswers = CompleteSupportingEvidenceAnswers(false, List.empty)

        val (session, journey, draftReturn) = sessionWithSingleDisposalState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers)),
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Left(Error("some error")))
        }

        checkIsTechnicalErrorPage(performAction())
      }
    }

    "redirect to summary" when {

      "the user accepts the check your answers and is on a single disposal journey" in {

        val answers = IncompleteSupportingEvidenceAnswers.empty.copy(
          doYouWantToUploadSupportingEvidence = Some(false),
          List.empty,
          List.empty
        )

        val updatedAnswers = CompleteSupportingEvidenceAnswers(false, List.empty)

        val (session, journey, draftReturn) = sessionWithSingleDisposalState(Some(answers))

        val updatedDraftReturn = draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers))
        val updatedSession: SessionData =
          session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers)),
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Right(()))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(),
          returns.routes.TaskListController.taskList()
        )

      }

      "the user accepts the check your answers and is on a multiple disposal journey" in {

        val answers = IncompleteSupportingEvidenceAnswers.empty.copy(
          doYouWantToUploadSupportingEvidence = Some(false),
          List.empty,
          List.empty
        )

        val updatedAnswers = CompleteSupportingEvidenceAnswers(false, List.empty)

        val (session, journey, draftReturn) = sessionWithMultipleDisposalsState(Some(answers))

        val updatedDraftReturn = draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers))
        val updatedSession: SessionData =
          session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers)),
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Right(()))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(),
          returns.routes.TaskListController.taskList()
        )

      }

    }
  }

  "handling requests to delete supporting evidence" must {
    def performAction(uploadReference: UploadReference, addNew: Boolean): Future[Result] =
      controller.deleteSupportingEvidence(uploadReference, addNew)(FakeRequest())

    "show technical error page" when {

      "failed to update database" in {
        val uploadReference    = sample[UploadReference]
        val supportingEvidence = sample[SupportingEvidence].copy(uploadReference = uploadReference)

        val answers = CompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = true,
          List(supportingEvidence)
        )

        val updatedAnswers = IncompleteSupportingEvidenceAnswers(Some(true), List.empty, List.empty)

        val (session, journey, draftReturn) = sessionWithMultipleDisposalsState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers)),
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Left(Error("update failed")))
        }

        checkIsTechnicalErrorPage(
          performAction(uploadReference, false)
        )

      }

    }

    "return the user to the check your answers page" when {

      "user has not completed the section and supporting evidence has been deleted and add new is false" in {

        val uploadReference    = sample[UploadReference]
        val supportingEvidence = sample[SupportingEvidence].copy(uploadReference = uploadReference)

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(true),
          List(supportingEvidence),
          List.empty
        )

        val updatedAnswers = IncompleteSupportingEvidenceAnswers(Some(true), List.empty, List.empty)

        val (session, journey, draftReturn) = sessionWithMultipleDisposalsState(Some(answers))

        val updatedDraftReturn = draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers))
        val updatedSession: SessionData =
          session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers)),
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Right(()))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(uploadReference, false),
          routes.SupportingEvidenceController.checkYourAnswers()
        )

      }

      "supporting evidence has been deleted and add new is false" in {

        val uploadReference    = sample[UploadReference]
        val supportingEvidence = sample[SupportingEvidence].copy(uploadReference = uploadReference)

        val answers = CompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = true,
          List(supportingEvidence)
        )

        val updatedAnswers = IncompleteSupportingEvidenceAnswers(Some(true), List.empty, List.empty)

        val (session, journey, draftReturn) = sessionWithMultipleDisposalsState(Some(answers))

        val updatedDraftReturn = draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers))
        val updatedSession: SessionData =
          session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers)),
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Right(()))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(uploadReference, false),
          routes.SupportingEvidenceController.checkYourAnswers()
        )

      }

      "supporting evidence has been deleted and add new is true" in {

        val uploadReference    = sample[UploadReference]
        val supportingEvidence = sample[SupportingEvidence].copy(uploadReference = uploadReference)

        val answers = CompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = true,
          List(supportingEvidence)
        )

        val updatedAnswers = IncompleteSupportingEvidenceAnswers(Some(true), List.empty, List.empty)

        val (session, journey, draftReturn) = sessionWithMultipleDisposalsState(Some(answers))

        val updatedDraftReturn = draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers))
        val updatedSession: SessionData =
          session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockStoreDraftReturn(
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers)),
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Right(()))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(uploadReference, true),
          routes.SupportingEvidenceController.uploadSupportingEvidence()
        )
      }
    }
  }

  "handling requests to check the upload status of the supporting evidence" must {
    def performAction(uploadReference: UploadReference): Future[Result] =
      controller.uploadSupportingEvidenceVirusCheck(uploadReference)(FakeRequest())

    "show technical error page" when {
      "update of draft return fails" in {
        val uploadReference    = sample[UploadReference]
        val supportingEvidence = sample[SupportingEvidence].copy(uploadReference = uploadReference)

        val upscanSuccess = sample[UpscanSuccess]
        val updatedUpscanSuccess =
          upscanSuccess.copy(uploadDetails = Map("fileName" -> supportingEvidence.fileName))

        val upscanUpload =
          sample[UpscanUpload].copy(uploadReference = uploadReference, upscanCallBack = Some(updatedUpscanSuccess))

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(true),
          List(supportingEvidence),
          List.empty
        )

        val (session, journey, draftReturn) = sessionWithMultipleDisposalsState(Some(answers))

        val newSupportingEvidence = SupportingEvidence(
          upscanUpload.uploadReference,
          upscanUpload.upscanUploadMeta,
          upscanUpload.uploadedOn,
          updatedUpscanSuccess,
          updatedUpscanSuccess.fileName
        )

        val updatedAnswers = answers.copy(evidences = newSupportingEvidence :: answers.evidences)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
          mockUpdateUpscanUpload(uploadReference, upscanUpload.copy(upscanUploadStatus = Uploaded))(Right(()))
          mockStoreDraftReturn(
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers)),
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Left(Error("update failed")))
        }

        checkIsTechnicalErrorPage(
          performAction(uploadReference)
        )
      }
    }

    "return the user to the check your answers page" when {

      "the user has completed their upload of supporting evidences" in {

        val uploadReference    = sample[UploadReference]
        val supportingEvidence = sample[SupportingEvidence].copy(uploadReference = uploadReference)

        val answers = CompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = true,
          List(supportingEvidence)
        )

        val (session, _, _) = sessionWithMultipleDisposalsState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(uploadReference),
          routes.SupportingEvidenceController.checkYourAnswers()
        )

      }

      "the upscan call back came back with a success status" in {

        val uploadReference    = sample[UploadReference]
        val supportingEvidence = sample[SupportingEvidence].copy(uploadReference = uploadReference)

        val upscanSuccess = sample[UpscanSuccess]
        val updatedUpscanSuccess =
          upscanSuccess.copy(uploadDetails = Map("fileName" -> supportingEvidence.fileName))

        val upscanUpload =
          sample[UpscanUpload].copy(uploadReference = uploadReference, upscanCallBack = Some(updatedUpscanSuccess))

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(true),
          List(supportingEvidence),
          List.empty
        )

        val (session, journey, draftReturn) = sessionWithMultipleDisposalsState(Some(answers))

        val newSupportingEvidence = SupportingEvidence(
          upscanUpload.uploadReference,
          upscanUpload.upscanUploadMeta,
          upscanUpload.uploadedOn,
          updatedUpscanSuccess,
          updatedUpscanSuccess.fileName
        )

        val updatedAnswers = answers.copy(evidences = newSupportingEvidence :: answers.evidences)

        val updatedDraftReturn = draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers))
        val updatedSession: SessionData =
          session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
          mockUpdateUpscanUpload(uploadReference, upscanUpload.copy(upscanUploadStatus = Uploaded))(Right(()))
          mockStoreDraftReturn(
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers)),
            journey.subscribedDetails.cgtReference,
            journey.agentReferenceNumber
          )(Right(()))
          mockStoreSession(updatedSession)(Right(()))
        }

        checkIsRedirect(
          performAction(uploadReference),
          routes.SupportingEvidenceController.checkYourAnswers()
        )

      }

      "the upscan call back came back with a failed status" in {

        val uploadReference    = sample[UploadReference]
        val supportingEvidence = sample[SupportingEvidence].copy(uploadReference = uploadReference)

        val upscanFailure = sample[UpscanFailure]

        val upscanUpload =
          sample[UpscanUpload].copy(uploadReference = uploadReference, upscanCallBack = Some(upscanFailure))

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(true),
          List(supportingEvidence),
          List.empty
        )

        val (session, _, _) = sessionWithMultipleDisposalsState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
          mockUpdateUpscanUpload(uploadReference, upscanUpload.copy(upscanUploadStatus = Uploaded))(Right(()))
        }

        checkPageIsDisplayed(
          performAction(uploadReference),
          messageFromMessageKey("supporting-evidence.check-upscan-status.title")
        )

      }

      "the upscan call back has not arrived" in {

        val uploadReference    = sample[UploadReference]
        val supportingEvidence = sample[SupportingEvidence].copy(uploadReference = uploadReference)

        val upscanUpload =
          sample[UpscanUpload].copy(uploadReference = uploadReference, upscanCallBack = None)

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(true),
          List(supportingEvidence),
          List.empty
        )

        val (session, _, _) = sessionWithMultipleDisposalsState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
          mockUpdateUpscanUpload(uploadReference, upscanUpload.copy(upscanUploadStatus = Uploaded))(Right(()))
        }

        checkPageIsDisplayed(
          performAction(uploadReference),
          messageFromMessageKey("supporting-evidence.check-upscan-status.title")
        )

      }

    }
  }

  "handling submit requests to check the upload status of the supporting evidence" must {
    def performAction(uploadReference: UploadReference): Future[Result] =
      controller.uploadSupportingEvidenceVirusCheckSubmit(uploadReference.toString)(FakeRequest())

    "redirect the user to the virus check page" when {

      "they click the submit button" in {

        val uploadReference    = sample[UploadReference]
        val supportingEvidence = sample[SupportingEvidence].copy(uploadReference = uploadReference)

        val answers = IncompleteSupportingEvidenceAnswers(
          doYouWantToUploadSupportingEvidence = Some(true),
          List(supportingEvidence),
          List.empty
        )

        val (session, _, _) = sessionWithMultipleDisposalsState(Some(answers))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(
          performAction(uploadReference),
          routes.SupportingEvidenceController.uploadSupportingEvidenceVirusCheck(
            UploadReference(uploadReference.toString)
          )
        )
      }
    }
  }
}
