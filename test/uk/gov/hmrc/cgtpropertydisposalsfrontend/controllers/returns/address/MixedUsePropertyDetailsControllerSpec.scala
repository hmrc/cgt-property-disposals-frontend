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

import cats.Eq
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.{sample, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Agent, Individual, Organisation}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MixedUsePropertyDetailsAnswers.{CompleteMixedUsePropertyDetailsAnswers, IncompleteMixedUsePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class MixedUsePropertyDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case FillingOutReturn(_, _, _, _: DraftSingleMixedUseDisposalReturn) => true
        case _                                                               => false
      }
    )

  override val overrideBindings = List[GuiceableModule](
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[ReturnsService].toInstance(mockReturnsService)
  )

  lazy val controller = instanceOf[MixedUsePropertyDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def sessionWithDraftMixedUseDisposal(
    name: Either[TrustName, IndividualName],
    userType: UserType,
    individualUserType: Option[IndividualUserType],
    mixedUsePropertyDetailsAnswers: Option[MixedUsePropertyDetailsAnswers]
  ): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) = {
    val draftReturn      = sample[DraftSingleMixedUseDisposalReturn].copy(
      triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = individualUserType),
      mixedUsePropertyDetailsAnswers = mixedUsePropertyDetailsAnswers
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      agentReferenceNumber =
        if (Eq.eqv(userType, Agent)) Some(sample[AgentReferenceNumber])
        else None
    )
    val sessionData      = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn),
      userType = Some(userType)
    )
    (sessionData, fillingOutReturn, draftReturn)
  }

  def individualState(
    address: Option[UkAddress] = None
  ): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) =
    sessionWithDraftMixedUseDisposal(
      Right(sample[IndividualName]),
      UserType.Individual,
      Some(Self),
      Some(IncompleteMixedUsePropertyDetailsAnswers(address, None, None))
    )

  def capacitorState(): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) =
    sessionWithDraftMixedUseDisposal(
      Right(sample[IndividualName]),
      UserType.Individual,
      Some(Capacitor),
      None
    )

  def personalRepState(): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) =
    sessionWithDraftMixedUseDisposal(
      Right(sample[IndividualName]),
      UserType.Agent,
      Some(PersonalRepresentative),
      None
    )

  def agentState(): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) =
    sessionWithDraftMixedUseDisposal(
      Right(sample[IndividualName]),
      UserType.Agent,
      Some(Self),
      None
    )

  def trustState(): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) =
    sessionWithDraftMixedUseDisposal(
      Left(sample[TrustName]),
      UserType.Organisation,
      None,
      None
    )

  def deriveUserKey(isAgent: Boolean, isATrust: Boolean): String =
    if (isAgent) ".agent"
    else if (isATrust) ".trust"
    else ""

  "MixedUsePropertyDetailsController" when {

    "handling requests to display the enter address page" must {

      def performAction(): Future[Result] =
        controller.enterUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        def test(result: Future[Result], expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc
                .select("#address > legend > h1 > span")
                .text() shouldBe messageFromMessageKey(
                "singleMixedUse.caption"
              )

              doc
                .select("#address > div:nth-child(2) > label")
                .text() shouldBe messageFromMessageKey(
                "address.uk.line1.label"
              )

              doc
                .select("#address > div:nth-child(3) > label")
                .text() shouldBe messageFromMessageKey(
                "address.uk.line2.label"
              )

              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.MixedUsePropertyDetailsController
                .enterUkAddressSubmit()
                .url
            }
          )

        "handling individuals" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test(performAction(), "address.uk.title")
        }

        "handling capacitors" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(performAction(), "address.uk.title")
        }

        "handling personal representatives" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }

          test(performAction(), "address.uk.title")
        }

        "handling agents" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(agentState()._1)
          }

          test(performAction(), "address.uk.title")
        }

        "handling trusts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(trustState()._1)
          }

          test(performAction(), "address.uk.title")

        }

      }

    }

    "handling submitted uk address" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterUkAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*)
        )

      behave like redirectToStartBehaviour(() => performAction())

      "return a form error" when {

        def test(
          formData: (String, String)*
        )(expectedErrorMessageKey: String, expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey(expectedTitleKey),
            doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )

        "address line 1 is empty" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test("postcode" -> "W1A2HV")(
            "address-line1.error.required",
            "address.uk.title"
          )
        }

        "address line 1 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(agentState()._1)
          }

          test(
            "address-line1" -> ("a" * 100),
            "postcode"      -> "W1A2HV"
          )(
            "address-line1.error.tooLong",
            "address.uk.title"
          )
        }

        "address line 1 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(trustState()._1)
          }

          test(
            "address-line1" -> "ab%csd",
            "postcode"      -> "W1A2HV"
          )(
            "address-line1.error.pattern",
            "address.uk.title"
          )
        }

        "address line 2 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }
          test(
            "address-line1" -> "address line 1",
            "address-line2" -> ("a" * 100),
            "postcode"      -> "W1A2HV"
          )(
            "address-line2.error.tooLong",
            "address.uk.title"
          )
        }

        "address line 2 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "address-line1" -> "address line 1",
            "address-line2" -> "fsdhio*fde@df",
            "postcode"      -> "W1A2HV"
          )(
            "address-line2.error.pattern",
            "address.uk.title"
          )
        }

        "address line 3 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }
          test(
            "address-line1" -> "address line 1",
            "address-town"  -> ("a" * 100),
            "postcode"      -> "W1A2HV"
          )(
            "address-town.error.tooLong",
            "address.uk.title"
          )
        }

        "address line 3 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "address-line1" -> "address line 1",
            "address-town"  -> "fsdhio*fde@df",
            "postcode"      -> "W1A2HV"
          )(
            "address-town.error.pattern",
            "address.uk.title"
          )
        }

        "address line 4 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }
          test(
            "address-line1"  -> "address line 1",
            "address-county" -> ("a" * 100),
            "postcode"       -> "W1A2HV"
          )(
            "address-county.error.tooLong",
            "address.uk.title"
          )
        }

        "address line 4 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "address-line1"  -> "address line 1",
            "address-county" -> "fsdhio*fde@df",
            "postcode"       -> "W1A2HV"
          )(
            "address-county.error.pattern",
            "address.uk.title"
          )
        }

        "address postcode is empty" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test("address-line1" -> "1 the Street")(
            "postcode.error.required",
            "address.uk.title"
          )
        }

        "the address postcode contains invalid characters" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test(
            "address-line1" -> "1 the Street",
            "postcode"      -> "W1A,2HV"
          )(
            "postcode.error.invalidCharacters",
            "address.uk.title"
          )
        }

        "the address postcode does not have a valid format" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test(
            "address-line1" -> "1 the Street",
            "postcode"      -> "ABC123"
          )(
            "postcode.error.pattern",
            "address.uk.title"
          )
        }

      }

      "show an error page" when {

        val (session, journey, draftReturn) = individualState()
        val address                         =
          UkAddress("address line 1", None, None, None, Postcode("ZZ10ZZ"))

        val newDraftReturn = draftReturn.copy(mixedUsePropertyDetailsAnswers =
          Some(IncompleteMixedUsePropertyDetailsAnswers(Some(address), None, None))
        )
        val newJourney     = journey.copy(draftReturn = newDraftReturn)
        val newSession     = session.copy(journeyStatus = Some(newJourney))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              newDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(
            performAction(
              "address-line1" -> address.line1,
              "postcode"      -> address.postcode.value
            )
          )
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              newDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(newSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "address-line1" -> address.line1,
              "postcode"      -> address.postcode.value
            )
          )
        }

      }

      "redirect to the cya page" when {

        "all updates are successful" in {
          val (session, journey, draftReturn) = agentState()
          val address                         =
            UkAddress("address line 1", None, None, None, Postcode("ZZ10ZZ"))

          val newDraftReturn = draftReturn.copy(mixedUsePropertyDetailsAnswers =
            Some(IncompleteMixedUsePropertyDetailsAnswers(Some(address), None, None))
          )
          val newJourney     = journey.copy(draftReturn = newDraftReturn)
          val newSession     = session.copy(journeyStatus = Some(newJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              newDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(newSession)(Right(()))
          }

          checkIsRedirect(
            performAction(
              "address-line1" -> address.line1,
              "postcode"      -> address.postcode.value
            ),
            routes.MixedUsePropertyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the disposal price page" must {

      val key = "singleMixedUseDisposalsDisposalPrice"

      def performAction(): Future[Result] =
        controller.enterDisposalValue()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        def test(
          draftReturn: DraftSingleMixedUseDisposalReturn,
          expectedBackLink: Call,
          userType: UserType
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                userType = Some(userType),
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn,
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name =
                        if (userType === Organisation) Left(sample[TrustName])
                        else Right(sample[IndividualName])
                    ),
                    agentReferenceNumber =
                      if (userType === Agent) Some(sample[AgentReferenceNumber])
                      else None
                  )
                )
              )
            )
          }
          val userKey = deriveUserKey(userType === Agent, userType === Organisation)

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action")                shouldBe routes.MixedUsePropertyDetailsController
                .enterDisposalValueSubmit()
                .url
              doc
                .select(s"#$key-form-hint")
                .text()                        shouldBe messageFromMessageKey(s"$key$userKey.helpText")
            }
          )
        }

        "individual user has not started this section before" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance(),
            Individual
          )
        }

        "trust user has not started this section before" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance(),
            Organisation
          )
        }

        "agent user has not started this section before" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance(),
            Agent
          )
        }

        "individual user has started but not completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                  disposalPrice = None
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance(),
            Individual
          )
        }

        "trust user has started but not completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                  disposalPrice = None
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance(),
            Organisation
          )
        }

        "agent user has started but not completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                  disposalPrice = None
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance(),
            Agent
          )
        }

        "individual user has completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[CompleteMixedUsePropertyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance(),
            Individual
          )
        }

        "trust user has completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[CompleteMixedUsePropertyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance(),
            Organisation
          )
        }

        "agent user has completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[CompleteMixedUsePropertyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance(),
            Agent
          )
        }

      }

    }

    "handling submitted answers to the disposal price page" must {

      val key = "singleMixedUseDisposalsDisposalPrice"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterDisposalValueSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*)
        )

      behave like redirectToStartBehaviour(() => performAction())

      "not update the session" when {

        "the data submitted is the same as one that already exists in session" in {

          val disposalPrice = AmountInPence.fromPounds(1000)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
                      mixedUsePropertyDetailsAnswers = Some(
                        sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                          disposalPrice = Some(disposalPrice)
                        )
                      )
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(key -> "1000"),
            routes.MixedUsePropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "show a form error for amount" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
                      mixedUsePropertyDetailsAnswers = Some(
                        sample[CompleteMixedUsePropertyDetailsAnswers]
                      )
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
                    )
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey(s"$key.title"),
            doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios(key).foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

      }

      "redirect to the cya page" when {

        def test(
          result: => Future[Result],
          oldDraftReturn: DraftReturn,
          updatedDraftReturn: DraftReturn
        ): Unit = {

          val journey = sample[FillingOutReturn].copy(
            draftReturn = oldDraftReturn
          )
          val session = SessionData.empty.copy(
            journeyStatus = Some(journey)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              session
                .copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))
            )(Right(()))
          }

          checkIsRedirect(
            result,
            routes.MixedUsePropertyDetailsController.checkYourAnswers()
          )
        }

        "the user has completed this section" in {

          val answers = sample[CompleteMixedUsePropertyDetailsAnswers].copy(
            disposalPrice = AmountInPence.fromPounds(1)
          )

          val oldDraftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
            mixedUsePropertyDetailsAnswers = Some(answers)
          )

          val updatedDraftReturn = oldDraftReturn.copy(
            mixedUsePropertyDetailsAnswers = Some(
              answers.copy(
                disposalPrice = AmountInPence.fromPounds(10)
              )
            ),
            yearToDateLiabilityAnswers = None
          )

          test(
            performAction(key -> "10"),
            oldDraftReturn,
            updatedDraftReturn
          )
        }

        "the user hasn't ever answered the disposal price question " +
          "and the draft return and session data has been successfully updated" in {

          val answers = sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
            address = Some(sample[UkAddress]),
            disposalPrice = Some(AmountInPence.fromPounds(1))
          )

          val oldDraftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
            mixedUsePropertyDetailsAnswers = Some(answers)
          )

          val updatedDraftReturn = oldDraftReturn.copy(
            mixedUsePropertyDetailsAnswers = Some(
              answers.copy(
                disposalPrice = Some(AmountInPence.fromPounds(10))
              )
            ),
            yearToDateLiabilityAnswers = None
          )

          test(
            performAction(key -> "10"),
            oldDraftReturn,
            updatedDraftReturn
          )
        }

      }

    }

    "handling requests to display the acquisition price page" must {

      val key = "singleMixedUseDisposalsAcquisitionPrice"

      def performAction(): Future[Result] =
        controller.enterAcquisitionValue()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        def test(
          draftReturn: DraftSingleMixedUseDisposalReturn,
          expectedBackLink: Call,
          userType: UserType
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                userType = Some(userType),
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = draftReturn.copy(triageAnswers =
                      sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = None)
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name =
                        if (userType === Organisation) Left(sample[TrustName])
                        else Right(sample[IndividualName])
                    ),
                    agentReferenceNumber =
                      if (userType === Agent) Some(sample[AgentReferenceNumber])
                      else None
                  )
                )
              )
            )
          }

          val userKey = deriveUserKey(userType === Agent, userType === Organisation)
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form")
                .attr("action")                shouldBe routes.MixedUsePropertyDetailsController
                .enterAcquisitionValue()
                .url
              doc
                .select(s"#$key-form-hint")
                .text()                        shouldBe messageFromMessageKey(s"$key$userKey.helpText")
            }
          )
        }

        "individual user has not started this section before" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterDisposalValue(),
            Individual
          )
        }

        "trust user has not started this section before" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterDisposalValue(),
            Organisation
          )
        }

        "agent user has not started this section before" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = None,
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterDisposalValue(),
            Agent
          )
        }

        "individual user has started but not completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                  acquisitionPrice = None
                )
              )
            ),
            routes.MixedUsePropertyDetailsController.enterDisposalValue(),
            Individual
          )
        }

        "trust user has started but not completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                  acquisitionPrice = None
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterDisposalValue(),
            Organisation
          )
        }

        "agent user has started but not completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                  acquisitionPrice = None
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.enterDisposalValue(),
            Agent
          )
        }

        "individual user has completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[CompleteMixedUsePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.checkYourAnswers(),
            Individual
          )
        }

        "trust user has completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[CompleteMixedUsePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.checkYourAnswers(),
            Organisation
          )
        }

        "agent user has completed this section" in {
          test(
            sample[DraftSingleMixedUseDisposalReturn].copy(
              mixedUsePropertyDetailsAnswers = Some(
                sample[CompleteMixedUsePropertyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.MixedUsePropertyDetailsController.checkYourAnswers(),
            Agent
          )
        }

      }

    }

    "handling submitted answers to the acquisition price page" must {

      val key = "singleMixedUseDisposalsAcquisitionPrice"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterAcquisitionValueSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*)
        )

      behave like redirectToStartBehaviour(() => performAction())

      "not update the session" when {

        "the data submitted is the same as one that already exists in session" in {

          val acquisitionPrice = AmountInPence.fromPounds(1000)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
                      mixedUsePropertyDetailsAnswers = Some(
                        sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
                          acquisitionPrice = Some(acquisitionPrice)
                        )
                      )
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
                    )
                  )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(key -> "1000"),
            routes.MixedUsePropertyDetailsController.checkYourAnswers()
          )
        }

      }

      "show a form error for amount" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
                      mixedUsePropertyDetailsAnswers = Some(
                        sample[CompleteMixedUsePropertyDetailsAnswers]
                      )
                    ),
                    subscribedDetails = sample[SubscribedDetails].copy(
                      name = Right(sample[IndividualName])
                    )
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey(s"$key.title"),
            doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios(key).foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

      }

      "redirect to the cya page" when {

        def test(
          result: => Future[Result],
          oldDraftReturn: DraftReturn,
          updatedDraftReturn: DraftReturn
        ): Unit = {
          val journey =
            sample[FillingOutReturn].copy(draftReturn = oldDraftReturn)
          val session = SessionData.empty.copy(journeyStatus = Some(journey))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              updatedDraftReturn,
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              session
                .copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))
            )(Right(()))
          }

          checkIsRedirect(
            result,
            routes.MixedUsePropertyDetailsController.checkYourAnswers()
          )
        }

        "the user has completed this section" in {
          val answers = sample[CompleteMixedUsePropertyDetailsAnswers].copy(
            acquisitionPrice = AmountInPence.fromPounds(10)
          )

          val oldDraftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
            mixedUsePropertyDetailsAnswers = Some(answers)
          )

          val newDraftReturn = oldDraftReturn.copy(
            mixedUsePropertyDetailsAnswers = Some(
              answers.copy(
                acquisitionPrice = AmountInPence.fromPounds(100)
              )
            ),
            yearToDateLiabilityAnswers = None
          )

          test(
            performAction(key -> "100"),
            oldDraftReturn,
            newDraftReturn
          )
        }

        "the user hasn't ever answered the acquisition price question " +
          "and the draft return and session data has been successfully updated" in {

          val answers = sample[IncompleteMixedUsePropertyDetailsAnswers].copy(
            acquisitionPrice = Some(AmountInPence.fromPounds(10))
          )

          val oldDraftReturn = sample[DraftSingleMixedUseDisposalReturn].copy(
            mixedUsePropertyDetailsAnswers = Some(answers)
          )

          val newDraftReturn = oldDraftReturn.copy(
            mixedUsePropertyDetailsAnswers = Some(
              answers.copy(
                acquisitionPrice = Some(AmountInPence.fromPounds(100))
              )
            ),
            yearToDateLiabilityAnswers = None
          )

          test(
            performAction(key -> "100"),
            oldDraftReturn,
            newDraftReturn
          )
        }

      }

    }

    "handling requests to display the cya page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the guidance page" when {

        "there is no address in the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState(address = None)._1)
          }

          checkIsRedirect(
            performAction(),
            routes.MixedUsePropertyDetailsController.singleMixedUseGuidance()
          )
        }

      }

    }

    "handling submits on the cya page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the tasklist" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(individualState()._1)
        }

        checkIsRedirect(
          performAction(),
          controllers.returns.routes.TaskListController.taskList()
        )
      }

    }

  }

}
