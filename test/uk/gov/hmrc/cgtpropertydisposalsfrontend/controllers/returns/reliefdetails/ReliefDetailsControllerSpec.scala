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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails

import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails.ReliefDetailsControllerSpec.validateReliefDetailsCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.OtherReliefsOption.{NoOtherReliefs, OtherReliefs}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, TaxYear, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class ReliefDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  lazy val controller = instanceOf[ReliefDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  val maxLettingsReliefValue = AmountInPence.fromPounds(40000)

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  def userMessageKey(userType: UserType): String = userType match {
    case UserType.Individual   => ""
    case UserType.Organisation => ".trust"
    case UserType.Agent        => ".agent"
    case other                 => sys.error(s"User type '$other' not handled")
  }

  def userTypeClue(userType: UserType): String = userType match {
    case UserType.Individual   => "an individual"
    case UserType.Organisation => "a trust"
    case UserType.Agent        => "an agent"
    case other                 => sys.error(s"User type '$other' not handled")
  }

  def setAgentReferenceNumber(userType: UserType): Option[AgentReferenceNumber] = userType match {
    case UserType.Agent => Some(sample[AgentReferenceNumber])
    case _              => None
  }

  def setNameForUserType(userType: UserType): Either[TrustName, IndividualName] = userType match {
    case UserType.Organisation => Left(sample[TrustName])
    case _                     => Right(sample[IndividualName])
  }

  def sessionWithReliefDetailsAnswers(
    reliefDetailsAnswers: Option[ReliefDetailsAnswers],
    userType: UserType
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = sample[DraftSingleDisposalReturn].copy(
      reliefDetailsAnswers = reliefDetailsAnswers,
      triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(disposalDate =
        sample[DisposalDate]
          .copy(taxYear = sample[TaxYear].copy(maxLettingsReliefAmount = maxLettingsReliefValue))
      )
    )

    val journey = sample[FillingOutReturn].copy(
      agentReferenceNumber = setAgentReferenceNumber(userType),
      draftReturn          = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      )
    )
    (
      SessionData.empty.copy(
        userType      = Some(userType),
        journeyStatus = Some(journey)
      ),
      journey,
      draftReturn
    )
  }

  def sessionWithReliefDetailsAnswers(
    fillingOutReturn: FillingOutReturn,
    singleDisposalDraftReturn: DraftSingleDisposalReturn,
    reliefDetailsAnswers: Option[ReliefDetailsAnswers],
    exemptionAndLossesAnswers: Option[ExemptionAndLossesAnswers],
    disposalDate: DisposalDate,
    completeSingleDisposalTriageAnswers: CompleteSingleDisposalTriageAnswers,
    taxYear: TaxYear,
    userType: UserType
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn = singleDisposalDraftReturn.copy(
      reliefDetailsAnswers      = reliefDetailsAnswers,
      exemptionAndLossesAnswers = exemptionAndLossesAnswers,
      triageAnswers = completeSingleDisposalTriageAnswers.copy(disposalDate =
        disposalDate
          .copy(taxYear = taxYear.copy(maxLettingsReliefAmount = maxLettingsReliefValue))
      )
    )

    val journey = fillingOutReturn.copy(
      draftReturn          = draftReturn,
      agentReferenceNumber = setAgentReferenceNumber(userType),
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      )
    )
    (
      SessionData.empty.copy(
        userType      = Some(userType),
        journeyStatus = Some(journey)
      ),
      journey,
      draftReturn
    )
  }

  def sessionWithReliefDetailsAnswers(
    reliefDetailsAnswers: ReliefDetailsAnswers,
    userType: UserType
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) =
    sessionWithReliefDetailsAnswers(Some(reliefDetailsAnswers), userType)

  "ReliefDetailsController" when {

    "handling requests to display the private residence relief page for an individual" must {

      def performAction(): Future[Result] = controller.privateResidentsRelief()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithReliefDetailsAnswers(None, UserType.Individual)._1)
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("privateResidentsRelief.title"))
        }

        "the user has answered the question before but has " +
          "not completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(12.34))
                ),
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"privateResidentsRelief.title"),
            doc => doc.select("#privateResidentsReliefValue").attr("value") shouldBe "12.34"
          )
        }

        "the user has answered the question before but has " +
          "completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers].copy(privateResidentsRelief = AmountInPence.fromPounds(12.34)),
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("privateResidentsRelief.title"),
            doc => doc.select("#privateResidentsReliefValue").attr("value") shouldBe "12.34"
          )
        }

      }

    }

    "handling requests to display the private residence relief page for an agent" must {

      def performAction(): Future[Result] = controller.privateResidentsRelief()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithReliefDetailsAnswers(None, UserType.Agent)._1)
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("privateResidentsRelief.agent.title"))
        }

        "the user has answered the question before but has " +
          "not completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(12.34))
                ),
                UserType.Agent
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"privateResidentsRelief.agent.title"),
            doc => doc.select("#privateResidentsReliefValue").attr("value") shouldBe "12.34"
          )
        }

        "the user has answered the question before but has " +
          "completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers].copy(privateResidentsRelief = AmountInPence.fromPounds(12.34)),
                UserType.Agent
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("privateResidentsRelief.agent.title"),
            doc => doc.select("#privateResidentsReliefValue").attr("value") shouldBe "12.34"
          )
        }

      }

    }

    "handling requests to display the private residence relief page for a trust" must {

      def performAction(): Future[Result] = controller.privateResidentsRelief()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithReliefDetailsAnswers(None, UserType.Organisation)._1)
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("privateResidentsRelief.trust.title"))
        }

        "the user has answered the question before but has " +
          "not completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(12.34))
                ),
                UserType.Organisation
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"privateResidentsRelief.trust.title"),
            doc => doc.select("#privateResidentsReliefValue").attr("value") shouldBe "12.34"
          )
        }

        "the user has answered the question before but has " +
          "completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers].copy(privateResidentsRelief = AmountInPence.fromPounds(12.34)),
                UserType.Organisation
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("privateResidentsRelief.trust.title"),
            doc => doc.select("#privateResidentsReliefValue").attr("value") shouldBe "12.34"
          )
        }

      }

    }

    "handling submitted answers to the private residence relief page for an individual" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.privateResidentsReliefSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      def updateDraftReturn(d: DraftSingleDisposalReturn, newAnswers: ReliefDetailsAnswers) =
        d.copy(
          reliefDetailsAnswers       = Some(newAnswers),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers],
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey("privateResidentsRelief.title"),
            doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios("privateResidentsReliefValue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = ("privateResidentsRelief" -> "0") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }
      }

      "show an error page" when {

        val currentAnswers =
          sample[CompleteReliefDetailsAnswers].copy(privateResidentsRelief = AmountInPence.fromPounds(1d))
        val (session, journey, draftReturn) = sessionWithReliefDetailsAnswers(currentAnswers, UserType.Individual)
        val newPrivateResidentsRelief       = AmountInPence.fromPounds(10d)
        val newDraftReturn = updateDraftReturn(
          draftReturn,
          IncompleteReliefDetailsAnswers(Some(newPrivateResidentsRelief), None, currentAnswers.otherReliefs)
        )

        "there is an error updating the draft return" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.subscribedDetails.cgtReference, journey.agentReferenceNumber)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(
            performAction(Seq("privateResidentsRelief" -> "0", "privateResidentsReliefValue" -> "10"))
          )
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.subscribedDetails.cgtReference, journey.agentReferenceNumber)(
              Right(())
            )
            mockStoreSession(
              session.copy(
                journeyStatus = Some(
                  journey.copy(
                    draftReturn = newDraftReturn
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(Seq("privateResidentsRelief" -> "0", "privateResidentsReliefValue" -> "10"))
          )

        }

      }

      "redirect to the cya page" when {

        "the user hasn't ever answered the relief details question " +
          "and the draft return and session data has been successfully updated" in {
          val (newPrivateResidentsRelief, newPrivateResidentsReliefValue) = "0" -> 10d
          val oldDraftReturn = sample[DraftSingleDisposalReturn].copy(
            reliefDetailsAnswers = None
          )
          val newDraftReturn =
            updateDraftReturn(
              oldDraftReturn,
              IncompleteReliefDetailsAnswers.empty.copy(
                privateResidentsRelief = Some(AmountInPence.fromPounds(newPrivateResidentsReliefValue))
              )
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction(
              Seq(
                "privateResidentsRelief"      -> newPrivateResidentsRelief,
                "privateResidentsReliefValue" -> newPrivateResidentsReliefValue.toString
              )
            ),
            oldDraftReturn,
            newDraftReturn
          )
        }

        "the user has not answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
          val (newPrivateResidentsRelief, newPrivateResidentsReliefValue) = "0" -> 1d
          val oldAnswers                                                  = sample[IncompleteReliefDetailsAnswers].copy(privateResidentsRelief = None)

          val oldDraftReturn = sample[DraftSingleDisposalReturn].copy(reliefDetailsAnswers = Some(oldAnswers))
          val newDraftReturn =
            updateDraftReturn(
              oldDraftReturn,
              oldAnswers.copy(
                privateResidentsRelief = Some(AmountInPence.fromPounds(newPrivateResidentsReliefValue)),
                lettingsRelief         = None
              )
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction(
              Seq(
                "privateResidentsRelief"      -> newPrivateResidentsRelief,
                "privateResidentsReliefValue" -> newPrivateResidentsReliefValue.toString
              )
            ),
            oldDraftReturn,
            newDraftReturn
          )
        }

        "not update the draft return or the session data" when {

          "the answer given has not changed from a previous one" in {
            val currentAnswers =
              sample[CompleteReliefDetailsAnswers].copy(privateResidentsRelief = AmountInPence.fromPounds(1))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithReliefDetailsAnswers(currentAnswers, UserType.Individual)._1)
            }

            checkIsRedirect(
              performAction(Seq("privateResidentsRelief" -> "0", "privateResidentsReliefValue" -> "1")),
              controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
            )

          }

        }

        "the lettings is reset" when {
          "the private residence relief value changes" in {
            forAll { c: CompleteReliefDetailsAnswers =>
              val completeAnswers = c.copy(privateResidentsRelief = AmountInPence.fromPounds(5))
              val (session, journey, draftReturn) = sessionWithReliefDetailsAnswers(
                completeAnswers,
                UserType.Individual
              )

              val newAnswers = IncompleteReliefDetailsAnswers(
                Some(AmountInPence.fromPounds(1)),
                None,
                completeAnswers.otherReliefs
              )
              val updatedDraftReturn = updateDraftReturn(draftReturn, newAnswers)
              val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)
              val updatedSession     = session.copy(journeyStatus = Some(updatedJourney))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  session
                )
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Right(()))
                mockStoreSession(updatedSession)(Right(()))
              }

              checkIsRedirect(
                performAction(Seq("privateResidentsRelief" -> "0", "privateResidentsReliefValue" -> "1")),
                controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
              )

            }
          }
        }
      }
    }

    "handling requests to display the lettings relief page" must {

      def performAction(): Future[Result] = controller.lettingsRelief()(FakeRequest())

      val requiredPreviousAnswers =
        IncompleteReliefDetailsAnswers.empty.copy(privateResidentsRelief = Some(AmountInPence.fromPounds(1)))

      behave like redirectToStartBehaviour(performAction)

      behave like noPrivateResidentsReliefBehaviour(performAction)

      "display the page" when {

        "the user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                requiredPreviousAnswers.copy(lettingsRelief = Some(AmountInPence.fromPounds(2))),
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("lettingsRelief.title"))
        }

        "the user has answered the question before but has " +
          "not completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(1.34)),
                  lettingsRelief         = Some(AmountInPence.fromPounds(12.34))
                ),
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("lettingsRelief.title"),
            doc => doc.select("#lettingsReliefValue").attr("value") shouldBe "12.34"
          )
        }

        "the user has answered the question before but has " +
          "completed the relief detail section" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers].copy(
                  privateResidentsRelief = AmountInPence.fromPounds(1.34),
                  lettingsRelief         = AmountInPence.fromPounds(12.34)
                ),
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("lettingsRelief.title"),
            doc => doc.select("#lettingsReliefValue").attr("value") shouldBe "12.34"
          )
        }

      }
      "redirect to the residents page" when {
        "the user has not answered residents relief and not answered lettings relief" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = None,
                  lettingsRelief         = None,
                  otherReliefs           = None
                ),
                UserType.Individual
              )._1
            )
          }
          checkIsRedirect(
            performAction(),
            controllers.returns.reliefdetails.routes.ReliefDetailsController.privateResidentsRelief()
          )

        }
      }
    }

    "handling submitted answers to the lettings relief page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.lettingsReliefSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      def updateDraftReturn(d: DraftSingleDisposalReturn, newAnswers: ReliefDetailsAnswers) =
        d.copy(
          reliefDetailsAnswers       = Some(newAnswers),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like noPrivateResidentsReliefBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers],
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey("lettingsRelief.title"),
            doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        def testWithResidentsRelief(residentsRelief: AmountInPence, data: (String, String)*)(
          expectedErrorMessageKey: String
        ) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers].copy(privateResidentsRelief = residentsRelief),
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey("lettingsRelief.title"),
            doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios("lettingsReliefValue").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = ("lettingsRelief" -> "0") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

        "the data is more than lettings relief limit" in {
          val valueGreaterThanLettingsRelief =
            (maxLettingsReliefValue ++ AmountInPence.fromPounds(10000)).inPounds().toString()
          test(
            "lettingsRelief"      -> "0",
            "lettingsReliefValue" -> valueGreaterThanLettingsRelief
          )(Messages("lettingsReliefValue.error.amountOverLimit", maxLettingsReliefValue.inPounds().toString()))

        }

        "the data is more than private residence relief limit" in {
          testWithResidentsRelief(AmountInPence.fromPounds(5), "lettingsRelief" -> "0", "lettingsReliefValue" -> "10")(
            "lettingsReliefValue.error.amountOverPrivateResidenceRelief"
          )

        }
      }

      "show an error page" when {

        val currentAnswers                  = sample[CompleteReliefDetailsAnswers].copy(lettingsRelief = AmountInPence.fromPounds(1d))
        val (session, journey, draftReturn) = sessionWithReliefDetailsAnswers(currentAnswers, UserType.Individual)
        val newLettingsRelief               = AmountInPence.fromPounds(2d)
        val newDraftReturn = updateDraftReturn(
          draftReturn,
          currentAnswers.copy(lettingsRelief = newLettingsRelief)
        )

        "there is an error updating the draft return" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.subscribedDetails.cgtReference, journey.agentReferenceNumber)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(
            performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> newLettingsRelief.inPounds().toString))
          )
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.subscribedDetails.cgtReference, journey.agentReferenceNumber)(
              Right(())
            )
            mockStoreSession(
              session.copy(journeyStatus =
                Some(
                  journey.copy(
                    draftReturn = newDraftReturn
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> newLettingsRelief.inPounds().toString))
          )

        }

      }

      "redirect to the cya page" when {

        "the user has not answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers = sample[IncompleteReliefDetailsAnswers]
            .copy(privateResidentsRelief = Some(sample[AmountInPence]), lettingsRelief = None)
          val newLettingsRelief = 2d

          val triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
          val oldDraftReturn = sample[DraftSingleDisposalReturn]
            .copy(triageAnswers = triageAnswers, reliefDetailsAnswers = Some(currentAnswers))
          val newDraftReturn =
            updateDraftReturn(
              oldDraftReturn,
              currentAnswers.copy(lettingsRelief = Some(AmountInPence.fromPounds(newLettingsRelief)))
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> newLettingsRelief.toString)),
            oldDraftReturn,
            newDraftReturn
          )
        }

        "the user has answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
          forAll { c: CompleteReliefDetailsAnswers =>
            val currentAnswers    = c.copy(lettingsRelief = AmountInPence.fromPounds(1d))
            val newLettingsRelief = 2d
            val triageAnswers     = sample[CompleteSingleDisposalTriageAnswers]
            val oldDraftReturn = sample[DraftSingleDisposalReturn]
              .copy(reliefDetailsAnswers = Some(currentAnswers), triageAnswers = triageAnswers)

            val newDraftReturn =
              updateDraftReturn(
                oldDraftReturn,
                currentAnswers.copy(
                  lettingsRelief = AmountInPence.fromPounds(newLettingsRelief)
                )
              )

            testSuccessfulUpdatesAfterSubmit(
              performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> newLettingsRelief.toString)),
              oldDraftReturn,
              newDraftReturn
            )
          }
        }
      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          val currentAnswers = sample[CompleteReliefDetailsAnswers].copy(lettingsRelief = AmountInPence.fromPounds(1d))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithReliefDetailsAnswers(currentAnswers, UserType.Individual)._1)
          }

          checkIsRedirect(
            performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> "1")),
            controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
          )

        }

      }

      "accept submitted values with commas" in {
        val currentAnswers =
          sample[CompleteReliefDetailsAnswers].copy(lettingsRelief = AmountInPence.fromPounds(1000d))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithReliefDetailsAnswers(currentAnswers, UserType.Individual)._1)
        }

        checkIsRedirect(
          performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> "1,000")),
          controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
        )
      }

      "accept submitted values with pound signs" in {
        val currentAnswers = sample[CompleteReliefDetailsAnswers].copy(lettingsRelief = AmountInPence.fromPounds(1d))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionWithReliefDetailsAnswers(currentAnswers, UserType.Individual)._1)
        }

        checkIsRedirect(
          performAction(Seq("lettingsRelief" -> "0", "lettingsReliefValue" -> "£1")),
          controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
        )
      }

    }

    "handling requests to display the other reliefs page" must {

      def performAction(): Future[Result] = controller.otherReliefs()(FakeRequest())

      val requiredPreviousAnswers =
        IncompleteReliefDetailsAnswers.empty.copy(
          privateResidentsRelief = Some(AmountInPence.fromPounds(1)),
          lettingsRelief         = Some(AmountInPence.fromPounds(2))
        )

      behave like redirectToStartBehaviour(performAction)

      behave like noLettingsReliefBehaviour(performAction)

      val otherReliefs = OtherReliefs("ReliefName", AmountInPence.fromPounds(13.34))
      "redirect to lettings relief page" when {

        "the user has residents relief greater than zero but lettings relief not answered yet" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(11.34)),
                  lettingsRelief         = None,
                  otherReliefs           = None
                ),
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            controllers.returns.reliefdetails.routes.ReliefDetailsController.lettingsRelief()
          )

        }
      }
      "display the page" when {

        "the user has not answered the question before" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                requiredPreviousAnswers.copy(otherReliefs = Some(otherReliefs)),
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(performAction(), messageFromMessageKey("otherReliefs.title"))

        }

        "the user has answered the question before but has " +
          "not completed the relief detail section" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(11.34)),
                  lettingsRelief         = Some(AmountInPence.fromPounds(12.34)),
                  otherReliefs           = Some(otherReliefs)
                ),
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("otherReliefs.title"),
            doc => doc.select("#otherReliefsAmount").attr("value") shouldBe "13.34"
          )

        }

        "the user has answered the question before but has " +
          "completed the relief detail section" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers].copy(
                  privateResidentsRelief = AmountInPence.fromPounds(1.34),
                  lettingsRelief         = AmountInPence.fromPounds(12.34),
                  otherReliefs           = Some(otherReliefs)
                ),
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("otherReliefs.title"),
            doc => doc.select("#otherReliefsAmount").attr("value") shouldBe "13.34"
          )

        }
      }

    }

    "handling submitted answers to the other reliefs page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.otherReliefsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like noLettingsReliefBehaviour(() => performAction(Seq.empty))

      "show a form error for amount" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers],
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey("otherReliefs.title"),
            doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          amountOfMoneyErrorScenarios("otherReliefsAmount").foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = ("otherReliefs" -> "0") :: ("otherReliefsName" -> "ReliefsName") :: scenario.formData
              test(data: _*)(scenario.expectedErrorMessageKey)
            }
          }
        }

      }

      "show a form error for name and amount" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String*) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers],
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey("otherReliefs.title"),
            doc =>
              expectedErrorMessageKey.toList match {
                case Nil =>
                case errorKey :: Nil =>
                  doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                    errorKey
                  )
                case errorKeys =>
                  val errors =
                    (1 to errorKeys.length).map(i =>
                      doc.select(s"#error-summary-display > ul > li:nth-child($i) > a").text()
                    )
                  expectedErrorMessageKey
                    .map(messageFromMessageKey(_))
                    .foreach(message => errors should contain(message))
              },
            BAD_REQUEST
          )
        }

        "nothing is submitted" in {
          test("otherReliefs" -> "0")("otherReliefsName.error.required", "otherReliefsAmount.error.required")
        }

        "invalid characters are submitted for name and nothing submitted to amount" in {
          test("otherReliefs" -> "0", "otherReliefsName" -> "£££££")(
            "otherReliefsName.error.invalid",
            "otherReliefsAmount.error.required"
          )
        }

        "empty other reliefs name" in {
          test("otherReliefs" -> "0", "otherReliefsName" -> "    ", "otherReliefsAmount" -> "£1")(
            "otherReliefsName.error.required"
          )
        }

        val otherReliefsNameTooLong =
          "The other reliefs name is too long. The other reliefs name is too long. The other reliefs name is too long."

        "other reliefs name too long " in {
          test("otherReliefs" -> "0", "otherReliefsName" -> otherReliefsNameTooLong, "otherReliefsAmount" -> "£1")(
            "otherReliefsName.error.tooLong"
          )
        }

      }

      "show an error page" when {

        val currentAnswers = sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(NoOtherReliefs))
        val currentDraftReturn = sample[DraftSingleDisposalReturn].copy(
          reliefDetailsAnswers       = Some(currentAnswers),
          exemptionAndLossesAnswers  = Some(sample[CompleteExemptionAndLossesAnswers]),
          yearToDateLiabilityAnswers = Some(sample[YearToDateLiabilityAnswers]),
          uploadSupportingDocuments  = Some(sample[UploadSupportingEvidenceAnswers])
        )
        val currentJourney = sample[FillingOutReturn].copy(draftReturn = currentDraftReturn)

        val currentSession  = SessionData.empty.copy(journeyStatus = Some(currentJourney))
        val newOtherReliefs = OtherReliefs("ReliefName", AmountInPence.fromPounds(2))
        val newDraftReturn = currentDraftReturn.copy(
          reliefDetailsAnswers = Some(
            currentAnswers.copy(
              otherReliefs = Some(newOtherReliefs)
            )
          ),
          exemptionAndLossesAnswers  = None,
          yearToDateLiabilityAnswers = None,
          uploadSupportingDocuments  = None
        )

        "there is an error updating the draft return" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
            mockStoreDraftReturn(
              newDraftReturn,
              currentJourney.subscribedDetails.cgtReference,
              currentJourney.agentReferenceNumber
            )(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(
            performAction(
              Seq(
                "otherReliefs"       -> "0",
                "otherReliefsName"   -> newOtherReliefs.name,
                "otherReliefsAmount" -> newOtherReliefs.amount.inPounds().toString
              )
            )
          )
        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
            mockStoreDraftReturn(
              newDraftReturn,
              currentJourney.subscribedDetails.cgtReference,
              currentJourney.agentReferenceNumber
            )(
              Right(())
            )
            mockStoreSession(
              currentSession.copy(journeyStatus =
                Some(
                  currentJourney.copy(
                    draftReturn = newDraftReturn
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              Seq(
                "otherReliefs"       -> "0",
                "otherReliefsName"   -> newOtherReliefs.name,
                "otherReliefsAmount" -> newOtherReliefs.amount.inPounds().toString
              )
            )
          )

        }

      }
      "redirect to lettings page" when {
        "the user has residents relief greater than zero but lettings relief not answered yet" in {

          val otherReliefs = OtherReliefs("ReliefName", AmountInPence.fromPounds(2d))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(11.34)),
                  lettingsRelief         = None,
                  otherReliefs           = Some(otherReliefs)
                ),
                UserType.Individual
              )._1
            )
          }
          checkIsRedirect(
            performAction(
              Seq(
                "otherReliefs"       -> "0",
                "otherReliefsName"   -> otherReliefs.name,
                "otherReliefsAmount" -> otherReliefs.amount.inPounds().toString
              )
            ),
            controllers.returns.reliefdetails.routes.ReliefDetailsController.lettingsRelief()
          )

        }
      }

      "redirect to the cya page" when {

        "the user has not answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
          val currentAnswers = sample[IncompleteReliefDetailsAnswers]
            .copy(
              privateResidentsRelief = Some(sample[AmountInPence]),
              lettingsRelief         = Some(sample[AmountInPence]),
              otherReliefs           = None
            )
          val newOtherReliefs = OtherReliefs("ReliefName", AmountInPence.fromPounds(3d))
          val updatedAnswers = currentAnswers.copy(
            otherReliefs = Some(newOtherReliefs)
          )

          val oldDraftReturn = sample[DraftSingleDisposalReturn].copy(
            reliefDetailsAnswers       = Some(currentAnswers),
            yearToDateLiabilityAnswers = Some(sample[YearToDateLiabilityAnswers])
          )
          val newDraftReturn =
            oldDraftReturn.copy(
              reliefDetailsAnswers       = Some(updatedAnswers),
              exemptionAndLossesAnswers  = None,
              yearToDateLiabilityAnswers = None,
              uploadSupportingDocuments  = None
            )

          testSuccessfulUpdatesAfterSubmit(
            performAction(
              Seq(
                "otherReliefs"       -> "0",
                "otherReliefsName"   -> newOtherReliefs.name,
                "otherReliefsAmount" -> newOtherReliefs.amount.inPounds().toString
              )
            ),
            oldDraftReturn,
            newDraftReturn
          )
        }

        "the user has answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
          forAll { c: CompleteReliefDetailsAnswers =>
            val otherReliefs    = OtherReliefs("ReliefName1", AmountInPence.fromPounds(1d))
            val newOtherReliefs = OtherReliefs("ReliefName2", AmountInPence.fromPounds(2d))

            val currentAnswers = c.copy(otherReliefs              = Some(otherReliefs))
            val updatedAnswers = currentAnswers.copy(otherReliefs = Some(newOtherReliefs))
            val oldDraftReturn = sample[DraftSingleDisposalReturn].copy(
              reliefDetailsAnswers = Some(currentAnswers)
            )
            val newDraftReturn =
              oldDraftReturn.copy(
                reliefDetailsAnswers = Some(updatedAnswers),
                yearToDateLiabilityAnswers =
                  oldDraftReturn.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
              )

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                Seq(
                  "otherReliefs"       -> "0",
                  "otherReliefsName"   -> newOtherReliefs.name,
                  "otherReliefsAmount" -> newOtherReliefs.amount.inPounds().toString
                )
              ),
              oldDraftReturn,
              newDraftReturn
            )
          }
        }

      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          val otherReliefs   = OtherReliefs("ReliefName1", AmountInPence.fromPounds(1d))
          val currentAnswers = sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(otherReliefs))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                currentAnswers,
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(
            performAction(
              Seq(
                "otherReliefs"       -> "0",
                "otherReliefsName"   -> otherReliefs.name,
                "otherReliefsAmount" -> otherReliefs.amount.inPounds().toString
              )
            ),
            controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
          )

        }

      }

      "accept submitted values with commas" in {

        val otherReliefs = OtherReliefs("ReliefName1", AmountInPence.fromPounds(1000d))
        val currentAnswers =
          sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(otherReliefs))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithReliefDetailsAnswers(
              currentAnswers,
              UserType.Individual
            )._1
          )
        }

        checkIsRedirect(
          performAction(
            Seq(
              "otherReliefs"       -> "0",
              "otherReliefsName"   -> "ReliefName1",
              "otherReliefsAmount" -> "1,000"
            )
          ),
          controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
        )

      }

      "accept submitted values with pound signs" in {

        val otherReliefs = OtherReliefs("ReliefName1", AmountInPence.fromPounds(1000d))
        val currentAnswers =
          sample[CompleteReliefDetailsAnswers].copy(otherReliefs = Some(otherReliefs))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithReliefDetailsAnswers(
              currentAnswers,
              UserType.Individual
            )._1
          )
        }

        checkIsRedirect(
          performAction(
            Seq(
              "otherReliefs"       -> "0",
              "otherReliefsName"   -> "ReliefName1",
              "otherReliefsAmount" -> "£1,000"
            )
          ),
          controllers.returns.reliefdetails.routes.ReliefDetailsController.checkYourAnswers()
        )
      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

      val completeAnswers = CompleteReliefDetailsAnswers(
        sample[AmountInPence],
        sample[AmountInPence],
        Some(sample[OtherReliefs])
      )

      val allQuestionsAnswered = IncompleteReliefDetailsAnswers(
        Some(completeAnswers.privateResidentsRelief),
        Some(completeAnswers.lettingsRelief),
        completeAnswers.otherReliefs
      )

      behave like redirectToStartBehaviour(performAction)

      "redirect to the private residence relief page" when {

        "there are no relief details answers in session" in {
          inSequence {

            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                None,
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.ReliefDetailsController.privateResidentsRelief())
        }

        "there are relief details in session but no answer for the private residence relief question" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                allQuestionsAnswered.copy(privateResidentsRelief = None),
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.ReliefDetailsController.privateResidentsRelief())

        }
      }

      "redirect to the lettings relief page" when {
        "the user has not answered that question and the amount of private residence relief is greater them zero" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                allQuestionsAnswered
                  .copy(privateResidentsRelief = Some(AmountInPence(20)), lettingsRelief = None, otherReliefs = None),
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.ReliefDetailsController.lettingsRelief())

        }

      }

      "redirect to the other reliefs page" when {

        "the user has not answered that question" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                allQuestionsAnswered.copy(otherReliefs = None),
                UserType.Individual
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.ReliefDetailsController.otherReliefs())

        }
        "the user has not answered lettings relief question as the user selected No for residents relief " in {
          val sessionData = sessionWithReliefDetailsAnswers(
            IncompleteReliefDetailsAnswers(
              Some(AmountInPence(0)),
              None,
              None
            ),
            UserType.Individual
          )._1

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkIsRedirect(
            performAction(),
            routes.ReliefDetailsController.otherReliefs()
          )
        }
      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          val (session, journey, draftReturn) = sessionWithReliefDetailsAnswers(
            allQuestionsAnswered,
            UserType.Individual
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              draftReturn.copy(
                reliefDetailsAnswers = Some(completeAnswers)
              ),
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "there is an error updating the session" in {
          val (session, journey, draftReturn) = sessionWithReliefDetailsAnswers(
            allQuestionsAnswered,
            UserType.Individual
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(
              draftReturn.copy(
                reliefDetailsAnswers = Some(completeAnswers)
              ),
              journey.subscribedDetails.cgtReference,
              journey.agentReferenceNumber
            )(Right(()))
            mockStoreSession(
              session.copy(
                journeyStatus = Some(
                  journey.copy(draftReturn =
                    draftReturn.copy(
                      reliefDetailsAnswers = Some(completeAnswers)
                    )
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "show the page" when {

        "the user has just answered all the questions and all updates are successful" in {
          val (session, journey, draftReturn) = sessionWithReliefDetailsAnswers(
            allQuestionsAnswered,
            UserType.Individual
          )
          val newDraftReturn = draftReturn.copy(reliefDetailsAnswers = Some(completeAnswers))
          val updatedJourney = journey.copy(draftReturn              = newDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newDraftReturn, journey.subscribedDetails.cgtReference, journey.agentReferenceNumber)(
              Right(())
            )
            mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("reliefDetails.cya.title"),
            doc =>
              doc.select("#content > article > form").attr("action") shouldBe routes.ReliefDetailsController
                .checkYourAnswersSubmit()
                .url
          )
        }

        "the user has already answered all the questions" in {
          forAll { completeAnswers: CompleteReliefDetailsAnswers =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithReliefDetailsAnswers(
                  completeAnswers,
                  UserType.Individual
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("reliefDetails.cya.title"), { doc =>
                validateReliefDetailsCheckYourAnswersPage(completeAnswers, doc)
                doc.select("#content > article > form").attr("action") shouldBe routes.ReliefDetailsController
                  .checkYourAnswersSubmit()
                  .url
              }
            )
          }
        }
        "the user has set residential relief to no" in {
          val completeAnswersWithoutResidentialRelief = CompleteReliefDetailsAnswers(
            AmountInPence(0),
            AmountInPence(0),
            Some(sample[OtherReliefs])
          )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                completeAnswersWithoutResidentialRelief,
                UserType.Individual
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("reliefDetails.cya.title"), { doc =>
              validateReliefDetailsCheckYourAnswersPage(completeAnswersWithoutResidentialRelief, doc)
              doc.select("#content > article > form").attr("action") shouldBe routes.ReliefDetailsController
                .checkYourAnswersSubmit()
                .url
            }
          )
        }
      }

    }

    "handling submitted answers to the check your answers page" must {
      def performAction(): Future[Result] = controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the task list page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithReliefDetailsAnswers(
              sample[CompleteReliefDetailsAnswers],
              UserType.Individual
            )._1
          )
        }

        checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
      }

    }
  }

  def noPrivateResidentsReliefBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the what was your private residence relief page" when {

      "there is no private residence relief " in {
        val sessionData = sessionWithReliefDetailsAnswers(
          IncompleteReliefDetailsAnswers(
            None,
            Some(sample[AmountInPence]),
            Some(sample[OtherReliefsOption])
          ),
          UserType.Individual
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        checkIsRedirect(
          performAction(),
          routes.ReliefDetailsController.privateResidentsRelief()
        )
      }
    }

  def noLettingsReliefBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the what was your lettings relief page" when {

      "there is no lettings relief " in {
        val sessionData = sessionWithReliefDetailsAnswers(
          IncompleteReliefDetailsAnswers(
            Some(sample[AmountInPence]),
            None,
            Some(sample[OtherReliefsOption])
          ),
          UserType.Individual
        )._1

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        checkIsRedirect(
          performAction(),
          routes.ReliefDetailsController.lettingsRelief()
        )
      }
    }

  def testSuccessfulUpdatesAfterSubmit(
    result: => Future[Result],
    oldDraftReturn: DraftSingleDisposalReturn,
    newDraftReturn: DraftSingleDisposalReturn
  ): Unit = {
    val journey = sample[FillingOutReturn].copy(draftReturn = oldDraftReturn)
    val session = SessionData.empty.copy(journeyStatus      = Some(journey))

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreDraftReturn(newDraftReturn, journey.subscribedDetails.cgtReference, journey.agentReferenceNumber)(
        Right(())
      )
      mockStoreSession(
        session.copy(
          journeyStatus = Some(journey.copy(draftReturn = newDraftReturn))
        )
      )(Right(()))
    }

    checkIsRedirect(result, routes.ReliefDetailsController.checkYourAnswers())
  }

}

object ReliefDetailsControllerSpec extends Matchers {
  def validateReliefDetailsCheckYourAnswersPage(
    reliefDetailsAnswers: CompleteReliefDetailsAnswers,
    doc: Document
  ): Unit = {

    if (reliefDetailsAnswers.privateResidentsRelief.isZero) {
      doc.select("#privateResidentsReliefValue-answer").text shouldBe "No"
    } else {
      doc.select("#privateResidentsRelief-answer").text shouldBe "Yes"
      doc.select("#privateResidentsReliefValue-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
        reliefDetailsAnswers.privateResidentsRelief.inPounds()
      )
    }

    if (reliefDetailsAnswers.privateResidentsRelief.isPositive) {
      if (reliefDetailsAnswers.lettingsRelief.isZero) {
        doc.select("#lettingsReliefValue-answer").text shouldBe "No"
      } else {
        doc.select("#lettingsRelief-answer").text shouldBe "Yes"
        doc.select("#lettingsReliefValue-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
          reliefDetailsAnswers.lettingsRelief.inPounds()
        )
      }
    } else {
      doc.select("#lettingsReliefValue-answer").hasText shouldBe false
      doc.select("#lettingsRelief-answer").hasText      shouldBe false
    }

    reliefDetailsAnswers.otherReliefs.foreach {
      case a: OtherReliefsOption.OtherReliefs =>
        doc.select("#otherReliefs-answer").text     shouldBe "Yes"
        doc.select("#otherReliefsName-answer").text shouldBe a.name
        doc.select("#otherReliefsAmount-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
          a.amount.inPounds().bigDecimal
        )
      case OtherReliefsOption.NoOtherReliefs => doc.select("#otherReliefs-answer").text shouldBe "No"

    }

  }
}
