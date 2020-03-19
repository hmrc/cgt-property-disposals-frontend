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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address

import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AddressControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => returnsAddressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftReturn, MultipleDisposalsDraftReturn, SingleDisposalDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsExamplePropertyDetailsAnswers.{CompleteMultipleDisposalsExamplePropertyDetailsAnswers, IncompleteMultipleDisposalsExamplePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class MultipleDisposalsPropertyDetailsControllerSpec
    extends AddressControllerSpec[FillingOutReturn]
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with ReturnsServiceSupport {

  val incompleteAnswers =
    IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty.copy(address = Some(ukAddress(1)))

  val draftReturn: MultipleDisposalsDraftReturn =
    sample[MultipleDisposalsDraftReturn].copy(examplePropertyDetailsAnswers = Some(incompleteAnswers))

  val validJourneyStatus = FillingOutReturn(sample[SubscribedDetails], sample[GGCredId], None, draftReturn)

  override def overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](bind[ReturnsService].toInstance(mockReturnsService)) ::: super.overrideBindings

  lazy val controller = instanceOf[PropertyDetailsController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  override def updateAddress(journey: FillingOutReturn, address: Address): FillingOutReturn = address match {
    case a: UkAddress =>
      journey.copy(draftReturn =
        draftReturn.copy(examplePropertyDetailsAnswers = Some(incompleteAnswers.copy(address = Some(a))))
      )
    case _: NonUkAddress => journey
  }

  override val mockUpdateAddress: Option[(FillingOutReturn, Address, Either[Error, Unit]) => Unit] =
    Some {
      case (newDetails: FillingOutReturn, a: UkAddress, r: Either[Error, Unit]) =>
        mockStoreDraftReturn(
          draftReturn.copy(examplePropertyDetailsAnswers = Some(incompleteAnswers.copy(address = Some(a)))),
          newDetails.subscribedDetails.cgtReference,
          newDetails.agentReferenceNumber
        )(r)

      case (_, _: NonUkAddress, _) =>
        sys.error("Non UK addresses not handled in this spec")
    }

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  "AddressController" when {

    "handling requests to display the guidance page" must {

      def performAction(): Future[Result] =
        controller.multipleDisposalsGuidance()(FakeRequest())

      "redirect to the check your answers page" when {

        "the user is on a single disposal journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[SingleDisposalDraftReturn]
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.PropertyDetailsController.checkYourAnswers())
        }

      }

      "display the page" when {

        def test(
          draftReturn: MultipleDisposalsDraftReturn,
          expectedBackLink: Call
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("property-details.multiple-disposals.guidance.title"), { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.PropertyDetailsController
                .multipleDisposalsGuidanceSubmit()
                .url
            }
          )
        }

        "the user has not started this section before" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = None
            ),
            controllers.returns.routes.TaskListController.taskList()
          )
        }

        "the user has started but not completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers])
            ),
            controllers.returns.routes.TaskListController.taskList()
          )
        }

        "the user has completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers])
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling submits on the guidance page" must {

      def performAction(): Future[Result] =
        controller.multipleDisposalsGuidanceSubmit()(FakeRequest())

      def test(
        draftReturn: DraftReturn,
        expectedRedirect: Call
      ): Unit = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            SessionData.empty.copy(
              journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = draftReturn
                )
              )
            )
          )
        }

        checkIsRedirect(performAction(), expectedRedirect)
      }

      "redirect to the check your answers page" when {

        "the user is on a single disposal journey" in {
          test(
            sample[SingleDisposalDraftReturn],
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

        "the user ins on a multiple disposals journey and has completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[CompleteMultipleDisposalsExamplePropertyDetailsAnswers])
            ),
            routes.PropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "redirect to the enter postcode page" when {

        "the user has not started this section before" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = None
            ),
            routes.PropertyDetailsController.enterPostcode()
          )
        }

        "the user has started but not completed this section" in {
          test(
            sample[MultipleDisposalsDraftReturn].copy(
              examplePropertyDetailsAnswers = Some(sample[IncompleteMultipleDisposalsExamplePropertyDetailsAnswers])
            ),
            routes.PropertyDetailsController.enterPostcode()
          )
        }

      }
    }

    "handling requests to display the enter UK address page" must {

      def performAction(): Future[Result] = controller.enterUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displayEnterUkAddressPage(performAction)

    }

    "handling submitted addresses from enter UK address page" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterUkAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitEnterUkAddress(
        performAction,
        returnsAddressRoutes.PropertyDetailsController.checkYourAnswers()
      )

    }

    "handling requests to display the enter postcode page" must {

      def performAction(): Future[Result] = controller.enterPostcode()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like enterPostcodePage(performAction)

    }

    "handling submitted postcodes and filters" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterPostcodeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitEnterPostcode(performAction, returnsAddressRoutes.PropertyDetailsController.selectAddress())

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like displaySelectAddress(
        performAction,
        controllers.returns.address.routes.PropertyDetailsController.enterPostcode()
      )

    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitSelectAddress(
        performAction,
        controllers.returns.address.routes.PropertyDetailsController.enterPostcode(),
        returnsAddressRoutes.PropertyDetailsController.checkYourAnswers()
      )

      "not update the session" when {

        "the user selects an address which is already in their session" in {
          val session = sessionWithValidJourneyStatus.copy(addressLookupResult = Some(addressLookupResult))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          val result = performAction(Seq("address-select" -> "0"))
          checkIsRedirect(result, returnsAddressRoutes.PropertyDetailsController.checkYourAnswers())
        }

      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      val address = sample[UkAddress]

      val completeAnswers      = CompleteMultipleDisposalsExamplePropertyDetailsAnswers(address)
      val allQuestionsAnswered = IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(Some(completeAnswers.address))
      val currentDraftReturn =
        sample[MultipleDisposalsDraftReturn].copy(examplePropertyDetailsAnswers = Some(allQuestionsAnswered))
      val currentJourney     = sample[FillingOutReturn].copy(draftReturn             = currentDraftReturn)
      val updatedDraftReturn = currentDraftReturn.copy(examplePropertyDetailsAnswers = Some(completeAnswers))
      val updatedJourney     = currentJourney.copy(draftReturn                       = updatedDraftReturn)

      "redirect to the guidance page" when {

        "the user has not started the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = draftReturn.copy(
                    examplePropertyDetailsAnswers = None
                  )
                )
              )
              )
            )
          }

          checkIsRedirect(performAction(), routes.PropertyDetailsController.multipleDisposalsGuidance())
        }

        "the user has started the section but there is no property address" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(journeyStatus = Some(
                sample[FillingOutReturn].copy(
                  draftReturn = draftReturn.copy(
                    examplePropertyDetailsAnswers = Some(allQuestionsAnswered.copy(address = None))
                  )
                )
              )
              )
            )
          }

          checkIsRedirect(performAction(), routes.PropertyDetailsController.multipleDisposalsGuidance())
        }
      }

      "display the page" when {

        def testIsCheckYourAnswers(
          result: Future[Result],
          ukAddressDetails: UkAddress,
          expectedTitleKey: String
        ): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey), { doc =>
              val guidanceLink = doc.select("#guidanceLink")
              guidanceLink.attr("href") shouldBe routes.PropertyDetailsController.multipleDisposalsGuidance().url
              guidanceLink.text() shouldBe messageFromMessageKey(
                "returns.property-details.multiple-disposals.cya.guidanceLink"
              )

              MultipleDisposalsPropertyDetailsControllerSpec.validatePropertyAddressPage(ukAddressDetails, doc)
            }
          )

        "the user has just finished answering the questions and all updates are successful" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(currentJourney)))
            mockStoreDraftReturn(
              updatedDraftReturn,
              currentJourney.subscribedDetails.cgtReference,
              currentJourney.agentReferenceNumber
            )(Right(()))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(updatedJourney)))(Right(()))
          }

          testIsCheckYourAnswers(
            performAction(),
            address,
            "returns.property-address.cya.title"
          )
        }

        "the user hsa already completed the section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(updatedJourney)))
          }

          testIsCheckYourAnswers(
            performAction(),
            address,
            "returns.property-address.cya.title"
          )
        }

      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(currentJourney)))
            mockStoreDraftReturn(
              updatedDraftReturn,
              currentJourney.subscribedDetails.cgtReference,
              currentJourney.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(SessionData.empty.copy(journeyStatus = Some(currentJourney)))
            mockStoreDraftReturn(
              updatedDraftReturn,
              currentJourney.subscribedDetails.cgtReference,
              currentJourney.agentReferenceNumber
            )(Right(()))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(updatedJourney)))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

    }

    "handling check your answers submits" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithValidJourneyStatus)
        }

        checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
      }

    }

  }
}

object MultipleDisposalsPropertyDetailsControllerSpec extends Matchers {
  def validatePropertyAddressPage(
    ukAddress: UkAddress,
    doc: Document
  )(implicit messages: MessagesApi, lang: Lang): Unit =
    doc.select("#property-address-answer").text() shouldBe
      List(Some(ukAddress.line1), ukAddress.line2, ukAddress.town, ukAddress.county, Some(ukAddress.postcode.value))
        .collect { case Some(s) => s.trim }
        .mkString(" ")
}
