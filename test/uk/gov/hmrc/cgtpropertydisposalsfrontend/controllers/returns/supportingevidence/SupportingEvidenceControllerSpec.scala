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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.supportingevidence
import cats.data.EitherT
import cats.instances.future.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, returns}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.FileUploadGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.{CompleteSupportingEvidenceAnswers, IncompleteSupportingEvidenceAnswers, SupportingEvidence}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftSingleDisposalReturn, SupportingEvidenceAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.UpscanCallBack.{UpscanFailure, UpscanSuccess}
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
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  private val mockUUIDGenerator = mock[UUIDGenerator]

  val mockUpscanService: UpscanService = mock[UpscanService]

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[UpscanService].toInstance(mockUpscanService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    )

  private lazy val controller = instanceOf[SupportingEvidenceController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private def mockUpscanInitiate(
    errorRedirectCall: Call,
    successRedirectCall: UploadReference => Call
  )(
    result: Either[Error, UpscanUpload]
  ) =
    (mockUpscanService
      .initiate(_: Call, _: UploadReference => Call)(using _: HeaderCarrier))
      .expects(
        where {
          (
            actualErrorRedirectCall: Call,
            actualSuccessRedirectCall: UploadReference => Call,
            _: HeaderCarrier
          ) =>
            val uploadReference = sample[UploadReference]
            actualErrorRedirectCall shouldBe errorRedirectCall
            actualSuccessRedirectCall(
              uploadReference
            )                       shouldBe successRedirectCall(uploadReference)
            true
        }
      )
      .returning(EitherT.fromEither(result))

  private def mockGetUpscanUpload(uploadReference: UploadReference)(
    result: Either[Error, UpscanUpload]
  ) =
    (mockUpscanService
      .getUpscanUpload(_: UploadReference)(using _: HeaderCarrier))
      .expects(uploadReference, *)
      .returning(EitherT.fromEither[Future](result))

  private def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case _: FillingOutReturn      => true
        case _: StartingToAmendReturn => true
        case _                        => false
      }
    )

  def sessionWithSingleDisposalState(
    supportingEvidenceAnswers: Option[SupportingEvidenceAnswers]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = sample[DraftSingleDisposalReturn].copy(
      supportingEvidenceAnswers = supportingEvidenceAnswers
    )
    val journey     = sample[FillingOutReturn].copy(draftReturn = draftReturn)
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
    val journey     = sample[FillingOutReturn].copy(draftReturn = draftReturn)
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
  )(
    expectedErrorMessageKey: String,
    errorArgs: Seq[String] = Nil
  )(pageTitleKey: String, titleArgs: String*)(
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
      messageFromMessageKey(pageTitleKey, titleArgs*),
      { doc =>
        doc
          .select("[data-spec='errorSummaryDisplay'] a")
          .text() shouldBe messageFromMessageKey(
          expectedErrorMessageKey,
          errorArgs*
        )
        doc.title() should startWith("Error:")
      },
      BAD_REQUEST
    )
  }

  "Supporting Evidence Controller" when {

    "handling requests to display the do you want to upload supporting evidence page" must {

      def performAction(): Future[Result] =
        controller.doYouWantToUploadSupportingEvidence()(FakeRequest())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.doYouWantToUploadSupportingEvidence(),
        mockUUIDGenerator
      )

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
            messageFromMessageKey(
              "supporting-evidence.do-you-want-to-upload.title"
            ),
            { doc =>
              doc
                .select("#back, .govuk-back-link")
                .attr("href")   shouldBe returns.routes.TaskListController
                .taskList()
                .url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action") shouldBe routes.SupportingEvidenceController
                .doYouWantToUploadSupportingEvidence()
                .url
            }
          )
        }

        "a user has answered the question before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithSingleDisposalState(
                Some(
                  IncompleteSupportingEvidenceAnswers(
                    Some(false),
                    List.empty,
                    List.empty
                  )
                )
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "supporting-evidence.do-you-want-to-upload.title"
            ),
            { doc =>
              doc
                .select("#back, .govuk-back-link")
                .attr("href")   shouldBe returns.routes.TaskListController
                .taskList()
                .url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action") shouldBe routes.SupportingEvidenceController
                .doYouWantToUploadSupportingEvidence()
                .url
            }
          )
        }
      }
    }

    "handling submission of do you want to upload supporting evidence page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.doYouWantToUploadSupportingEvidenceSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.doYouWantToUploadSupportingEvidenceSubmit(),
        mockUUIDGenerator
      )

      "display the technical error page" when {

        "an error occurs storing the draft return and the user wants to upload supporting evidence" in {
          val answers =
            IncompleteSupportingEvidenceAnswers.empty.copy(
              doYouWantToUploadSupportingEvidence = Some(true),
              List.empty,
              List.empty
            )

          val (session, journey, draftReturn) =
            sessionWithSingleDisposalState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(journey.copy(draftReturn = draftReturn))(Left(Error("some error")))
          }

          checkIsTechnicalErrorPage(
            performAction("supporting-evidence.do-you-want-to-upload" -> "true")
          )
        }

        "an error occurs when storing the draft return and the user does not want to upload supporting evidence" in {
          val answers =
            IncompleteSupportingEvidenceAnswers.empty.copy(
              doYouWantToUploadSupportingEvidence = Some(false),
              List.empty,
              List.empty
            )

          val (session, journey, draftReturn) =
            sessionWithSingleDisposalState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(journey.copy(draftReturn = draftReturn))(Left(Error("some error")))

          }

          checkIsTechnicalErrorPage(
            performAction(
              "supporting-evidence.do-you-want-to-upload" -> "false"
            )
          )
        }
      }

      "show a form error" when {

        val answers = IncompleteSupportingEvidenceAnswers.empty

        val currentSession = sessionWithSingleDisposalState(
          Some(answers)
        )._1

        def test(data: (String, String)*)(expectedErrorMessageKey: String): Unit =
          testFormError(data*)(expectedErrorMessageKey)(
            "supporting-evidence.do-you-want-to-upload.title"
          )(
            performAction,
            currentSession
          )

        "the user has not selected an answer to the question asking whether they want to upload supporting evidence" in {
          test()("supporting-evidence.do-you-want-to-upload.error.required")
        }
      }

      "redirect to the check your answers page" when {

        "the user has never answered question before and wants to upload supporting evidence and is on a single disposable journey" in {

          val (session, journey, draftReturn) =
            sessionWithSingleDisposalState(None)

          val updatedDraftReturn =
            draftReturn.copy(supportingEvidenceAnswers =
              Some(
                IncompleteSupportingEvidenceAnswers(
                  Some(true),
                  List.empty,
                  List.empty
                )
              )
            )

          val updatedJourney              = journey.copy(draftReturn = updatedDraftReturn)
          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(
              "supporting-evidence.do-you-want-to-upload" -> "true"
            ),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }

        "the user has answered yes to the question before and has answered no this time and is on a single disposable journey" in {

          val answers =
            sample[IncompleteSupportingEvidenceAnswers]
              .copy(doYouWantToUploadSupportingEvidence = Some(true))

          val (session, journey, draftReturn) =
            sessionWithSingleDisposalState(Some(answers))

          val updatedDraftReturn =
            draftReturn.copy(supportingEvidenceAnswers =
              Some(
                answers.copy(
                  doYouWantToUploadSupportingEvidence = Some(false),
                  evidences = List.empty,
                  expiredEvidences = List.empty
                )
              )
            )

          val updatedJourney = journey.copy(draftReturn = updatedDraftReturn)

          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(
              "supporting-evidence.do-you-want-to-upload" -> "false"
            ),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }

        "the user has answered yes to the question before and has answered yes again and is on a single disposable journey" in {

          val answers =
            sample[IncompleteSupportingEvidenceAnswers]
              .copy(doYouWantToUploadSupportingEvidence = Some(true))

          val (session, journey, draftReturn) =
            sessionWithSingleDisposalState(Some(answers))

          val updatedDraftReturn = draftReturn
          val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
          }

          checkIsRedirect(
            performAction(
              "supporting-evidence.do-you-want-to-upload" -> "true"
            ),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }

        "the user has answered no to the question before and has answered yes this time and is on a single disposable journey" in {

          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(false),
            evidences = List.empty,
            expiredEvidences = List.empty
          )

          val (session, journey, draftReturn) =
            sessionWithSingleDisposalState(Some(answers))

          val updatedDraftReturn =
            draftReturn.copy(supportingEvidenceAnswers =
              Some(
                sample[IncompleteSupportingEvidenceAnswers]
                  .copy(
                    doYouWantToUploadSupportingEvidence = Some(true),
                    evidences = List.empty,
                    expiredEvidences = List.empty
                  )
              )
            )

          val updatedJourney = journey.copy(draftReturn = updatedDraftReturn)

          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(
              "supporting-evidence.do-you-want-to-upload" -> "true"
            ),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }
      }
    }

    "handling requests to upload supporting evidence" must {

      def performAction(): Future[Result] =
        controller.uploadSupportingEvidence()(FakeRequest())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.uploadSupportingEvidence(),
        mockUUIDGenerator
      )

      "show check your answers page" when {

        "the number of uploads have reached the maximum allowed" in {
          val supportingEvidence = sample[SupportingEvidence]

          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(true),
            evidences = List.fill(2)(supportingEvidence),
            expiredEvidences = List.empty
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
          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(false),
            evidences = List.empty,
            expiredEvidences = List.empty
          )

          val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.SupportingEvidenceController
                .handleUpscanErrorRedirect(),
              uploadReference =>
                routes.SupportingEvidenceController
                  .scanProgress(uploadReference)
            )(
              Left(Error("some upscan error"))
            )
          }
          checkIsTechnicalErrorPage(performAction())
        }

      }

      "show file upload page" when {

        "number of uploads has not exceeded limit" in {

          val uploadReference = sample[UploadReference]

          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(false),
            evidences = List.empty,
            expiredEvidences = List.empty
          )

          val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

          val upscanUpload = sample[UpscanUpload].copy(
            uploadReference = uploadReference
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockUpscanInitiate(
              routes.SupportingEvidenceController
                .handleUpscanErrorRedirect(),
              uploadReference =>
                routes.SupportingEvidenceController
                  .scanProgress(uploadReference)
            )(Right(upscanUpload))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("supporting-evidence.upload.title"),
            doc =>
              doc
                .select("#back, .govuk-back-link")
                .attr("href") shouldBe routes.SupportingEvidenceController
                .doYouWantToUploadSupportingEvidence()
                .url
          )
        }

      }

      "show the file upload failed page" when {

        "there is an error redirect from upscan" in {
          def performAction(): Future[Result] =
            controller.handleUpscanErrorRedirect()(
              FakeRequest(
                "GET",
                s"/supporting-evidence/handle-upscan-redirect-error?upscan-query-params=some-error-value"
              )
            )

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(true),
            evidences = List.fill(2)(supportingEvidence),
            expiredEvidences = List.empty
          )

          val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          checkIsRedirect(
            performAction(),
            routes.SupportingEvidenceController.documentDidNotUpload()
          )
        }

      }
    }

    "handling expired evidence" must {

      "show technical error page" when {

        "update of draft fails" in {

          def performAction(): Future[Result] =
            controller.supportingEvidenceExpiredSubmit()(FakeRequest())

          val supportingEvidence = sample[SupportingEvidence]

          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(true),
            evidences = List.fill(1)(supportingEvidence),
            expiredEvidences = List(supportingEvidence)
          )

          val (session, journey, draftReturn) =
            sessionWithSingleDisposalState(Some(answers))

          val updatedDraftReturn =
            draftReturn.copy(supportingEvidenceAnswers =
              Some(
                answers.copy(
                  doYouWantToUploadSupportingEvidence = Some(true),
                  expiredEvidences = List.empty
                )
              )
            )
          val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Left(Error("update failed")))
          }

          checkIsTechnicalErrorPage(
            performAction()
          )
        }
      }

      "redirect to check your answers" when {

        "if answers are complete" in {
          def performAction(): Future[Result] =
            controller.supportingEvidenceExpired()(FakeRequest())

          val answers = CompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = false,
            evidences = List.empty
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
          def performAction(): Future[Result] =
            controller.supportingEvidenceExpired()(FakeRequest())

          val supportingEvidence = sample[SupportingEvidence]

          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(true),
            evidences = List.empty,
            expiredEvidences = List(supportingEvidence)
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

          def performAction(): Future[Result] =
            controller.supportingEvidenceExpiredSubmit()(FakeRequest())

          val supportingEvidence = sample[SupportingEvidence]

          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(true),
            evidences = List.fill(1)(supportingEvidence),
            expiredEvidences = List.empty
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

          def performAction(): Future[Result] =
            controller.supportingEvidenceExpiredSubmit()(FakeRequest())

          val supportingEvidence = sample[SupportingEvidence]

          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(true),
            evidences = List.fill(1)(supportingEvidence),
            expiredEvidences = List(supportingEvidence)
          )

          val (session, journey, draftReturn) =
            sessionWithSingleDisposalState(Some(answers))

          val updatedDraftReturn =
            draftReturn.copy(supportingEvidenceAnswers =
              Some(
                answers.copy(
                  doYouWantToUploadSupportingEvidence = Some(true),
                  expiredEvidences = List.empty
                )
              )
            )
          val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)

          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }

      }
    }

    "handling actions on check your answer" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswers(),
        mockUUIDGenerator
      )

      "display the file upload page" when {

        "the user has already answered yes to adding supporting evidences and has not uploaded any evidences so far" in {

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

          def stripUUID(endPoint: String): String =
            endPoint.split("/").dropRight(1).mkString("/")

          val result              = performAction()
          val expectedRedirectUrl =
            routes.SupportingEvidenceController.uploadSupportingEvidence().url

          status(result)                                    shouldBe SEE_OTHER
          redirectLocation(result).map(ep => stripUUID(ep)) shouldBe Some(
            stripUUID(expectedRedirectUrl)
          )
        }
      }

      "display the do you want to upload supporting evidence page" when {

        "the use has never answered this question" in {

          val answers = IncompleteSupportingEvidenceAnswers.empty

          val (session, _, _) = sessionWithSingleDisposalState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkIsRedirect(
            performAction(),
            routes.SupportingEvidenceController
              .doYouWantToUploadSupportingEvidence()
          )
        }
      }

      "display supporting evidence expired page" when {

        "there are expired supporting evidences" in {

          val supportingEvidence = sample[SupportingEvidence]
          val answers            = IncompleteSupportingEvidenceAnswers.empty.copy(
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

        "the user has completed the supporting evidence section" in {

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
            messageFromMessageKey(
              "supporting-evidence.check-your-answers.title"
            )
          )
        }

        "the user has answered the do you want to upload supporting evidence question" in {

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
            messageFromMessageKey(
              "supporting-evidence.check-your-answers.title"
            )
          )
        }
      }

      "display the technical error page" when {

        def performAction(): Future[Result] =
          controller.checkYourAnswersSubmit()(FakeRequest())

        "an error occurs when storing the draft return" in {

          val answers = IncompleteSupportingEvidenceAnswers.empty.copy(
            doYouWantToUploadSupportingEvidence = Some(false),
            List.empty,
            List.empty
          )

          val updatedAnswers =
            CompleteSupportingEvidenceAnswers(doYouWantToUploadSupportingEvidence = false, List.empty)

          val (session, journey, draftReturn) =
            sessionWithSingleDisposalState(Some(answers))

          val updatedJourney =
            journey.copy(draftReturn = draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Left(Error("some error")))
          }

          checkIsTechnicalErrorPage(performAction())
        }
      }

      "redirect to summary" when {

        def performAction(): Future[Result] =
          controller.checkYourAnswersSubmit()(FakeRequest())

        behave like redirectToStartBehaviour(() => performAction())

        behave like amendReturnToFillingOutReturnSpecBehaviour(
          controller.checkYourAnswersSubmit(),
          mockUUIDGenerator
        )

        "the user accepts the check your answers and is on a single disposal journey" in {

          val answers = IncompleteSupportingEvidenceAnswers.empty.copy(
            doYouWantToUploadSupportingEvidence = Some(false),
            List.empty,
            List.empty
          )

          val updatedAnswers =
            CompleteSupportingEvidenceAnswers(doYouWantToUploadSupportingEvidence = false, List.empty)

          val (session, journey, draftReturn) =
            sessionWithSingleDisposalState(Some(answers))

          val updatedDraftReturn          =
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          val updatedJourney              = journey.copy(draftReturn = updatedDraftReturn)
          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
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

          val updatedAnswers =
            CompleteSupportingEvidenceAnswers(doYouWantToUploadSupportingEvidence = false, List.empty)

          val (session, journey, draftReturn) =
            sessionWithMultipleDisposalsState(Some(answers))

          val updatedDraftReturn          =
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          val updatedJourney              = journey.copy(draftReturn = updatedDraftReturn)
          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
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
      def performAction(
        uploadReference: UploadReference,
        addNew: Boolean
      ): Future[Result] =
        controller.deleteSupportingEvidence(uploadReference, addNew)(
          FakeRequest()
        )

      "show technical error page" when {

        "failed to update database" in {
          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val answers = CompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = true,
            List(supportingEvidence)
          )

          val updatedAnswers = IncompleteSupportingEvidenceAnswers(
            Some(true),
            List.empty,
            List.empty
          )

          val (session, journey, draftReturn) =
            sessionWithMultipleDisposalsState(Some(answers))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              journey.copy(draftReturn =
                draftReturn
                  .copy(supportingEvidenceAnswers = Some(updatedAnswers))
              )
            )(Left(Error("update failed")))
          }

          checkIsTechnicalErrorPage(
            performAction(uploadReference, addNew = false)
          )
        }
      }

      "return the user to the check your answers page" when {

        "user has not completed the section and supporting evidence has been deleted and add new is false" in {

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(true),
            List(supportingEvidence),
            List.empty
          )

          val updatedAnswers = IncompleteSupportingEvidenceAnswers(
            Some(true),
            List.empty,
            List.empty
          )

          val (session, journey, draftReturn) =
            sessionWithMultipleDisposalsState(Some(answers))

          val updatedDraftReturn          =
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          val updatedJourney              = journey.copy(draftReturn = updatedDraftReturn)
          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(uploadReference, addNew = false),
            routes.SupportingEvidenceController.checkYourAnswers()
          )

        }

        "supporting evidence has been deleted and add new is false" in {

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val answers = CompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = true,
            List(supportingEvidence)
          )

          val updatedAnswers = IncompleteSupportingEvidenceAnswers(
            Some(true),
            List.empty,
            List.empty
          )

          val (session, journey, draftReturn) =
            sessionWithMultipleDisposalsState(Some(answers))

          val updatedDraftReturn          =
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          val updatedJourney              = journey.copy(draftReturn = updatedDraftReturn)
          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(uploadReference, addNew = false),
            routes.SupportingEvidenceController.checkYourAnswers()
          )

        }

        "supporting evidence has been deleted and add new is true" in {

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val answers = CompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = true,
            List(supportingEvidence)
          )

          val updatedAnswers = IncompleteSupportingEvidenceAnswers(
            Some(true),
            List.empty,
            List.empty
          )

          val (session, journey, draftReturn) =
            sessionWithMultipleDisposalsState(Some(answers))

          val updatedDraftReturn          =
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          val updatedJourney              = journey.copy(draftReturn = updatedDraftReturn)
          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(uploadReference, addNew = true),
            routes.SupportingEvidenceController.uploadSupportingEvidence()
          )
        }
      }
    }

    "handling requests to check the upload status of the supporting evidence" must {
      def performAction(uploadReference: UploadReference): Future[Result] =
        controller.scanProgress(uploadReference)(
          FakeRequest()
        )

      "show technical error page" when {
        "update of draft return fails" in {
          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val upscanSuccess        = sample[UpscanSuccess]
          val updatedUpscanSuccess =
            upscanSuccess.copy(uploadDetails = Map("fileName" -> supportingEvidence.fileName))

          val upscanUpload =
            sample[UpscanUpload].copy(
              uploadReference = uploadReference,
              upscanCallBack = Some(updatedUpscanSuccess)
            )

          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(true),
            List(supportingEvidence),
            List.empty
          )

          val (session, journey, draftReturn) =
            sessionWithMultipleDisposalsState(Some(answers))

          val newSupportingEvidence = SupportingEvidence(
            upscanUpload.uploadReference,
            upscanUpload.upscanUploadMeta,
            upscanUpload.uploadedOn,
            updatedUpscanSuccess,
            updatedUpscanSuccess.fileName
          )

          val updatedAnswers =
            answers.copy(evidences = newSupportingEvidence :: answers.evidences)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
            mockStoreDraftReturn(
              journey.copy(draftReturn = draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers)))
            )(Left(Error("update failed")))
          }

          checkIsTechnicalErrorPage(
            performAction(uploadReference)
          )
        }
      }

      "show the document did not upload error page" when {
        "an error redirect from upscan is handled" in {
          def performAction(): Future[Result] =
            controller.documentDidNotUpload()(
              FakeRequest()
            )

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val answers = CompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = true,
            List(supportingEvidence)
          )

          val (session, _, _) = sessionWithMultipleDisposalsState(Some(answers))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "supporting-evidence.upload-failed.title"
            )
          )

        }
      }

      "show there is a problem with your document error page" when {
        "an upscan failure call back is received" in {
          def performAction(): Future[Result] =
            controller.handleUpscanCallBackFailures()(
              FakeRequest()
            )

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val answers = CompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = true,
            List(supportingEvidence)
          )

          val (session, _, _) = sessionWithMultipleDisposalsState(Some(answers))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "supporting-evidence.scan-failed.title"
            )
          )

        }
      }

      "return the user to the check your answers page" when {

        "the user has completed their upload of supporting evidences" in {

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

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
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val upscanSuccess        = sample[UpscanSuccess]
          val updatedUpscanSuccess =
            upscanSuccess.copy(uploadDetails = Map("fileName" -> supportingEvidence.fileName))

          val upscanUpload =
            sample[UpscanUpload].copy(
              uploadReference = uploadReference,
              upscanCallBack = Some(updatedUpscanSuccess)
            )

          val answers = IncompleteSupportingEvidenceAnswers(
            doYouWantToUploadSupportingEvidence = Some(true),
            List(supportingEvidence),
            List.empty
          )

          val (session, journey, draftReturn) =
            sessionWithMultipleDisposalsState(Some(answers))

          val newSupportingEvidence = SupportingEvidence(
            upscanUpload.uploadReference,
            upscanUpload.upscanUploadMeta,
            upscanUpload.uploadedOn,
            updatedUpscanSuccess,
            updatedUpscanSuccess.fileName
          )

          val updatedAnswers =
            answers.copy(evidences = newSupportingEvidence :: answers.evidences)

          val updatedDraftReturn          =
            draftReturn.copy(supportingEvidenceAnswers = Some(updatedAnswers))
          val updatedJourney              = journey.copy(draftReturn = updatedDraftReturn)
          val updatedSession: SessionData =
            session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockGetUpscanUpload(uploadReference)(Right(upscanUpload))
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction(uploadReference),
            routes.SupportingEvidenceController.checkYourAnswers()
          )
        }

        "the upscan call back came back with a failed status" in {

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val upscanFailure = sample[UpscanFailure]

          val upscanUpload =
            sample[UpscanUpload].copy(
              uploadReference = uploadReference,
              upscanCallBack = Some(upscanFailure)
            )

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
          }

          checkIsRedirect(
            performAction(uploadReference),
            routes.SupportingEvidenceController.handleUpscanCallBackFailures()
          )
        }

        "the upscan call back has not arrived" in {

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

          val upscanUpload =
            sample[UpscanUpload]
              .copy(uploadReference = uploadReference, upscanCallBack = None)

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
          }

          checkPageIsDisplayed(
            performAction(uploadReference),
            messageFromMessageKey(
              "supporting-evidence.scan-progress.title"
            ),
            { doc =>
              doc
                .select("#main-content > div > div > div > p:nth-child(3)")
                .text() shouldBe messageFromMessageKey(
                "supporting-evidence.scan-progress.p1"
              )
              doc
                .select("#main-content > div > div > div > p:nth-child(4)")
                .text() shouldBe messageFromMessageKey(
                "supporting-evidence.scan-progress.p2"
              )
            }
          )
        }
      }
    }

    "handling submit requests to check the upload status of the supporting evidence" must {
      def performAction(uploadReference: UploadReference): Future[Result] =
        controller.scanProgressSubmit(
          uploadReference.toString
        )(FakeRequest())

      "redirect the user to the status check page" when {

        "they click the submit button" in {

          val uploadReference    = sample[UploadReference]
          val supportingEvidence =
            sample[SupportingEvidence].copy(uploadReference = uploadReference)

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
            routes.SupportingEvidenceController
              .scanProgress(
                UploadReference(uploadReference.toString)
              )
          )
        }
      }
    }

  }

}
