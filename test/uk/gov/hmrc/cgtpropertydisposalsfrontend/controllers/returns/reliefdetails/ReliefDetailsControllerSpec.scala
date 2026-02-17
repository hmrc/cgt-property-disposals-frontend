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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails

import org.jsoup.nodes.Document
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.reliefdetails.ReliefDetailsControllerSpec.validateReliefDetailsCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExemptionsAndLossesAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.FileUploadGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReliefDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TaxYearGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.YearToDateLiabilityAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExemptionAndLossesAnswers.CompleteExemptionAndLossesAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.OtherReliefsOption.{NoOtherReliefs, OtherReliefs}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.{CompleteReliefDetailsAnswers, IncompleteReliefDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SupportingEvidenceAnswers.CompleteSupportingEvidenceAnswers
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
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  private val mockUUIDGenerator = mock[UUIDGenerator]

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    )

  private lazy val controller = instanceOf[ReliefDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  private val maxLettingsReliefValue = AmountInPence.fromPounds(40000)

  private def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      () => performAction(),
      {
        case _: FillingOutReturn      => true
        case _: StartingToAmendReturn => true
        case _                        => false
      }
    )

  def userMessageKey(
    individualUserType: IndividualUserType,
    userType: UserType
  ): String =
    (individualUserType, userType) match {
      case (Capacitor, _)                                          => ".capacitor"
      case (PersonalRepresentative, _)                             => ".personalRep"
      case (PersonalRepresentativeInPeriodOfAdmin, UserType.Agent) => ".personalRepInPeriodOfAdmin.agent"
      case (PersonalRepresentativeInPeriodOfAdmin, _)              => ".personalRepInPeriodOfAdmin"
      case (_, UserType.Individual)                                => ""
      case (_, UserType.Organisation)                              => ".trust"
      case (_, UserType.Agent)                                     => ".agent"
      case other                                                   => sys.error(s"User type '$other' not handled")
    }

  def setAgentReferenceNumber(
    userType: UserType
  ): Option[AgentReferenceNumber] =
    userType match {
      case UserType.Agent => Some(sample[AgentReferenceNumber])
      case _              => None
    }

  def setNameForUserType(
    userType: UserType
  ): Either[TrustName, IndividualName] =
    userType match {
      case UserType.Organisation => Left(sample[TrustName])
      case _                     => Right(sample[IndividualName])
    }

  def sessionWithReliefDetailsAnswers(
    reliefDetailsAnswers: Option[ReliefDetailsAnswers],
    userType: UserType,
    individualUserType: IndividualUserType,
    isAmend: Boolean
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {

    val draftReturn = sample[DraftSingleDisposalReturn].copy(
      reliefDetailsAnswers = reliefDetailsAnswers,
      triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
        disposalDate = sample[DisposalDate].copy(
          taxYear = sample[TaxYear].copy(
            maxLettingsReliefAmount = maxLettingsReliefValue
          )
        ),
        individualUserType = Some(individualUserType)
      ),
      representeeAnswers = individualUserType match {
        case Self => None
        case _    => Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = true))
      }
    )

    val journey = sample[FillingOutReturn].copy(
      agentReferenceNumber = setAgentReferenceNumber(userType),
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      ),
      amendReturnData = if (isAmend) Some(sample[AmendReturnData]) else None,
      previousSentReturns = None
    )

    val sessionData = SessionData.empty.copy(
      userType = Some(userType),
      journeyStatus = Some(journey)
    )

    (sessionData, journey, draftReturn)
  }

  def sessionWithReliefDetailsAnswers(
    reliefDetailsAnswers: ReliefDetailsAnswers,
    userType: UserType,
    individualUserType: IndividualUserType,
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) =
    sessionWithReliefDetailsAnswers(
      Some(reliefDetailsAnswers),
      userType,
      individualUserType,
      isAmend
    )

  val acceptedUserTypeGen: Gen[UserType] =
    Gen.oneOf(UserType.Agent, UserType.Organisation, UserType.Individual)

  val acceptedIndividualUserTypeGen: Gen[IndividualUserType] =
    Gen.oneOf(Self, Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin)

  val acceptedIndividualUserTypeForLettingsRelief: Gen[IndividualUserType] =
    Gen.oneOf(Self, Capacitor, PersonalRepresentative)

  def isPeriodOfAdmin(individualUserType: IndividualUserType): Boolean =
    if (individualUserType === PersonalRepresentativeInPeriodOfAdmin) true else false

  "ReliefDetailsController" when {

    "handling requests to display the private residence relief page" must {

      def performAction(): Future[Result] =
        controller.privateResidentsRelief()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.privateResidentsRelief(),
        mockUUIDGenerator
      )

      val key      = "privateResidentsRelief"
      val valueKey = "privateResidentsReliefValue"

      "display the page" when {

        "the user has not answered the question before" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    None,
                    userType,
                    individualUserType,
                    isAmend = false
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key.h1"),
                doc => doc.select("legend").text() shouldBe messageFromMessageKey(s"$key$userKey.title")
              )
          }
        }

        "the user has answered the question before but has " +
          "not completed the relief detail section" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithReliefDetailsAnswers(
                      IncompleteReliefDetailsAnswers.empty.copy(
                        privateResidentsRelief = Some(AmountInPence.fromPounds(12.34))
                      ),
                      userType,
                      individualUserType
                    )._1
                  )
                }

                val userKey = userMessageKey(individualUserType, userType)

                checkPageIsDisplayed(
                  performAction(),
                  messageFromMessageKey(s"$key.h1"),
                  doc => {
                    doc.select(s"#$valueKey").attr("value") shouldBe "12.34"
                    doc.select("legend").text()             shouldBe messageFromMessageKey(s"$key$userKey.title")
                  }
                )
            }
          }

        "the user has answered the question before but has " +
          "completed the relief detail section" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithReliefDetailsAnswers(
                      sample[CompleteReliefDetailsAnswers]
                        .copy(privateResidentsRelief = AmountInPence.fromPounds(12.34)),
                      userType,
                      individualUserType
                    )._1
                  )
                }

                val userKey = userMessageKey(individualUserType, userType)

                checkPageIsDisplayed(
                  performAction(),
                  messageFromMessageKey(s"$key.h1"),
                  doc => {
                    doc.select(s"#$valueKey").attr("value") shouldBe "12.34"
                    doc.select("legend").text()             shouldBe messageFromMessageKey(s"$key$userKey.title")
                  }
                )
            }
          }

      }

    }

    "handling submitted answers to the private residence relief page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.privateResidentsReliefSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: ReliefDetailsAnswers,
        isFurtherOrAmendReturn: Boolean
      ): DraftSingleDisposalReturn =
        d.copy(
          reliefDetailsAnswers = Some(newAnswers),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails()),
          gainOrLossAfterReliefs = None,
          exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else d.exemptionAndLossesAnswers
        )

      val key      = "privateResidentsRelief"
      val valueKey = "privateResidentsReliefValue"

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.privateResidentsReliefSubmit(),
        mockUUIDGenerator
      )

      "show a form error" when {

        def test(data: (String, String)*)(
          expectedErrorMessageKey: String
        )(
          userType: UserType,
          individualUserType: IndividualUserType,
          userKey: String
        ): Unit = {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers],
                userType,
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey(s"$key.h1"),
            doc => {
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text()                   shouldBe messageFromMessageKey(expectedErrorMessageKey)
              doc.select("legend").text() shouldBe messageFromMessageKey(s"$key$userKey.title")
            },
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              amountOfMoneyErrorScenarios(
                key = valueKey,
                errorContext = Some(s"$valueKey$userKey")
              ).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  val data = (key -> "0") :: scenario.formData
                  test(data*)(scenario.expectedErrorMessageKey)(
                    userType,
                    individualUserType,
                    userKey
                  )
                }
              }
          }
        }

      }

      "show an error page" when {

        def getSessionJourneyAndDraftReturn(
          userType: UserType,
          individualUserType: IndividualUserType
        ): (SessionData, FillingOutReturn, DraftReturn) = {

          val lettingRelief =
            if (isPeriodOfAdmin(individualUserType)) Some(AmountInPence.zero) else None

          val currentAnswers = sample[IncompleteReliefDetailsAnswers].copy(
            privateResidentsRelief = Some(AmountInPence.fromPounds(1d)),
            lettingsRelief = lettingRelief
          )

          val (session, journey, draftReturn) =
            sessionWithReliefDetailsAnswers(
              currentAnswers,
              userType,
              individualUserType
            )

          val newPrivateResidentsRelief      = AmountInPence.fromPounds(10d)
          val incompleteReliefDetailsAnswers =
            IncompleteReliefDetailsAnswers(
              Some(newPrivateResidentsRelief),
              lettingRelief,
              currentAnswers.otherReliefs
            )

          val newDraftReturn =
            updateDraftReturn(draftReturn, incompleteReliefDetailsAnswers, isFurtherOrAmendReturn = false)

          (session, journey, newDraftReturn)
        }

        "there is an error updating the draft return" in {

          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, newDraftReturn) =
                getSessionJourneyAndDraftReturn(userType, individualUserType)
              val newJourney                         = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(Seq(key -> "0", valueKey -> "10"))
              )
          }

        }

        "there is an error updating the session data" in {

          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, newDraftReturn) =
                getSessionJourneyAndDraftReturn(userType, individualUserType)
              val newJourney                         = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(
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
                performAction(Seq(key -> "0", valueKey -> "10"))
              )
          }

        }

      }

      "redirect to the cya page" when {

        "the user hasn't ever answered the relief details question " +
          "and the draft return and session data has been successfully updated" in {
            val (newPrivateResidentsRelief, newPrivateResidentsReliefValue) =
              "0" -> 10d
            val oldDraftReturn = sample[DraftSingleDisposalReturn].copy(
              reliefDetailsAnswers = None
            )
            val newDraftReturn =
              updateDraftReturn(
                oldDraftReturn,
                IncompleteReliefDetailsAnswers.empty.copy(
                  privateResidentsRelief = Some(AmountInPence.fromPounds(newPrivateResidentsReliefValue))
                ),
                isFurtherOrAmendReturn = true
              )

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                Seq(
                  key      -> newPrivateResidentsRelief,
                  valueKey -> newPrivateResidentsReliefValue.toString
                )
              ),
              oldDraftReturn,
              newDraftReturn
            )
          }

        "the user has not answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {

            val (newPrivateResidentsRelief, newPrivateResidentsReliefValue) =
              "0" -> 1d
            val oldAnswers = sample[IncompleteReliefDetailsAnswers]
              .copy(privateResidentsRelief = None)

            val oldDraftReturn = sample[DraftSingleDisposalReturn]
              .copy(reliefDetailsAnswers = Some(oldAnswers))

            val lettingsRelief =
              if (oldDraftReturn.triageAnswers.isPeriodOfAdmin) Some(AmountInPence.zero) else None

            val newDraftReturn =
              updateDraftReturn(
                oldDraftReturn,
                oldAnswers.copy(
                  privateResidentsRelief = Some(
                    AmountInPence.fromPounds(newPrivateResidentsReliefValue)
                  ),
                  lettingsRelief = lettingsRelief
                ),
                isFurtherOrAmendReturn = true
              )

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                Seq(
                  key      -> newPrivateResidentsRelief,
                  valueKey -> newPrivateResidentsReliefValue.toString
                )
              ),
              oldDraftReturn,
              newDraftReturn
            )
          }

        "not update the draft return or the session data" when {

          "the answer given has not changed from a previous one" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                val currentAnswers = sample[CompleteReliefDetailsAnswers].copy(
                  privateResidentsRelief = AmountInPence.fromPounds(1)
                )

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithReliefDetailsAnswers(
                      currentAnswers,
                      userType,
                      individualUserType
                    )._1
                  )
                }

                checkIsRedirect(
                  performAction(Seq(key -> "0", valueKey -> "1")),
                  controllers.returns.reliefdetails.routes.ReliefDetailsController
                    .checkYourAnswers()
                )
            }
          }

        }

        "the lettings is reset" when {

          "the private residence relief value changes" in {

            forAll { (ic: IncompleteReliefDetailsAnswers) =>
              forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
                (userType: UserType, individualUserType: IndividualUserType) =>
                  val lettingRelief =
                    if (isPeriodOfAdmin(individualUserType)) Some(AmountInPence.zero) else None

                  val completeAnswers                 = ic.copy(
                    privateResidentsRelief = Some(AmountInPence.fromPounds(5)),
                    lettingsRelief = lettingRelief
                  )
                  val (session, journey, draftReturn) =
                    sessionWithReliefDetailsAnswers(
                      completeAnswers,
                      userType,
                      individualUserType
                    )

                  val newAnswers         = IncompleteReliefDetailsAnswers(
                    Some(AmountInPence.fromPounds(1)),
                    lettingRelief,
                    completeAnswers.otherReliefs
                  )
                  val updatedDraftReturn =
                    updateDraftReturn(draftReturn, newAnswers, isFurtherOrAmendReturn = false)
                  val updatedJourney     =
                    journey.copy(draftReturn = updatedDraftReturn)
                  val updatedSession     =
                    session.copy(journeyStatus = Some(updatedJourney))

                  inSequence {
                    mockAuthWithNoRetrievals()
                    mockGetSession(
                      session
                    )
                    mockStoreDraftReturn(updatedJourney)(Right(()))
                    mockStoreSession(updatedSession)(Right(()))
                  }

                  checkIsRedirect(
                    performAction(Seq(key -> "0", valueKey -> "1")),
                    controllers.returns.reliefdetails.routes.ReliefDetailsController
                      .checkYourAnswers()
                  )
              }
            }

          }

        }

      }

    }

    "handling requests to display the lettings relief page" must {

      val key      = "lettingsRelief"
      val valueKey = "lettingsReliefValue"

      def performAction(): Future[Result] =
        controller.lettingsRelief()(FakeRequest())

      val requiredPreviousAnswers = IncompleteReliefDetailsAnswers.empty.copy(
        privateResidentsRelief = Some(AmountInPence.fromPounds(1))
      )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.lettingsRelief(),
        mockUUIDGenerator
      )

      behave like noPrivateResidentsReliefBehaviour(() => performAction())

      "display the page" when {

        "the user has not answered the question before" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    requiredPreviousAnswers.copy(lettingsRelief = Some(AmountInPence.fromPounds(2))),
                    userType,
                    individualUserType
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key.h1"),
                doc => doc.select("legend").text() shouldBe messageFromMessageKey(s"$key$userKey.title")
              )
          }
        }

        "the user has answered the question before but has " +
          "not completed the relief detail section" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithReliefDetailsAnswers(
                      IncompleteReliefDetailsAnswers.empty.copy(
                        privateResidentsRelief = Some(AmountInPence.fromPounds(1.34)),
                        lettingsRelief = Some(AmountInPence.fromPounds(12.34))
                      ),
                      userType,
                      individualUserType
                    )._1
                  )
                }

                val userKey = userMessageKey(individualUserType, userType)

                checkPageIsDisplayed(
                  performAction(),
                  messageFromMessageKey(s"$key.h1"),
                  doc => {
                    doc.select(s"#$valueKey").attr("value") shouldBe "12.34"
                    doc.select("legend").text()             shouldBe messageFromMessageKey(s"$key$userKey.title")
                  }
                )
            }
          }

        "the user has answered the question before but has " +
          "completed the relief detail section" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithReliefDetailsAnswers(
                      sample[CompleteReliefDetailsAnswers].copy(
                        privateResidentsRelief = AmountInPence.fromPounds(1.34),
                        lettingsRelief = AmountInPence.fromPounds(12.34)
                      ),
                      userType,
                      individualUserType
                    )._1
                  )
                }

                val userKey = userMessageKey(individualUserType, userType)

                checkPageIsDisplayed(
                  performAction(),
                  messageFromMessageKey(s"$key.h1"),
                  doc => {
                    doc.select(s"#$valueKey").attr("value") shouldBe "12.34"
                    doc.select("legend").text()             shouldBe messageFromMessageKey(s"$key$userKey.title")
                  }
                )
            }
          }

      }

      "redirect to the residents page" when {
        "the user has not answered residents relief and not answered lettings relief" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    IncompleteReliefDetailsAnswers.empty.copy(
                      privateResidentsRelief = None,
                      lettingsRelief = None,
                      otherReliefs = None
                    ),
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                controllers.returns.reliefdetails.routes.ReliefDetailsController
                  .privateResidentsRelief()
              )
          }
        }
      }

    }

    "handling submitted answers to the lettings relief page" must {

      val key      = "lettingsRelief"
      val valueKey = "lettingsReliefValue"

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.lettingsReliefSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: ReliefDetailsAnswers,
        isFurtherOrAmendReturn: Boolean
      ): DraftSingleDisposalReturn =
        d.copy(
          reliefDetailsAnswers = Some(newAnswers),
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails()),
          exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else d.exemptionAndLossesAnswers
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.lettingsReliefSubmit(),
        mockUUIDGenerator
      )

      behave like noPrivateResidentsReliefBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        def test(data: (String, String)*)(
          expectedErrorMessageKey: String,
          args: List[String]
        )(
          userType: UserType,
          individualUserType: IndividualUserType,
          userKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers].copy(privateResidentsRelief = AmountInPence(Long.MaxValue)),
                userType,
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey(s"$key.h1"),
            doc => {
              doc.select("legend").text()                              shouldBe messageFromMessageKey(s"$key$userKey.title")
              doc.select("[data-spec='errorSummaryDisplay'] a").text() shouldBe Messages(
                expectedErrorMessageKey,
                args*
              )
            },
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              amountOfMoneyErrorScenarios(valueKey).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  val data = (key -> "0") :: scenario.formData
                  test(data*)(scenario.expectedErrorMessageKey, Nil)(
                    userType,
                    individualUserType,
                    userKey
                  )
                }
              }
          }
        }

        "the data is more than lettings relief limit" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey                        = userMessageKey(individualUserType, userType)
              val valueGreaterThanLettingsRelief =
                (maxLettingsReliefValue ++ AmountInPence.fromPounds(10000))
                  .inPounds()
                  .toString()
              test(key -> "0", valueKey -> valueGreaterThanLettingsRelief)(
                s"$valueKey.error.amountOverLimit",
                List(maxLettingsReliefValue.inPounds().toString())
              )(userType, individualUserType, userKey)
          }
        }

        "the data is more than private residence relief limit" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey         = userMessageKey(individualUserType, userType)
              val residentsRelief = AmountInPence.fromPounds(5)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    sample[CompleteReliefDetailsAnswers]
                      .copy(privateResidentsRelief = residentsRelief),
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(
                  Seq(key -> "0", valueKey -> "10")
                ),
                messageFromMessageKey(s"$key.h1"),
                doc => {
                  doc
                    .select("[data-spec='errorSummaryDisplay'] a")
                    .text()                   shouldBe messageFromMessageKey(
                    s"$valueKey.error.amountOverPrivateResidenceRelief"
                  )
                  doc.select("legend").text() shouldBe messageFromMessageKey(s"$key$userKey.title")
                },
                BAD_REQUEST
              )
          }
        }

      }

      "show an error page" when {

        val currentAnswers    = sample[CompleteReliefDetailsAnswers].copy(
          privateResidentsRelief = AmountInPence(Long.MaxValue),
          lettingsRelief = AmountInPence.fromPounds(1d)
        )
        val newLettingsRelief = AmountInPence.fromPounds(2d)

        def getSessionDataJourneyAndDraftReturn(
          userType: UserType,
          individualUserType: IndividualUserType
        ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
          val (session, journey, draftReturn) =
            sessionWithReliefDetailsAnswers(
              currentAnswers,
              userType,
              individualUserType
            )
          val newDraftReturn                  = updateDraftReturn(
            draftReturn,
            currentAnswers.copy(lettingsRelief = newLettingsRelief),
            isFurtherOrAmendReturn = false
          )

          (session, journey, newDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, newDraftReturn) =
                getSessionDataJourneyAndDraftReturn(
                  userType,
                  individualUserType
                )
              val newJourney                         = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(
                  Left(Error(""))
                )
              }

              checkIsTechnicalErrorPage(
                performAction(
                  Seq(
                    key      -> "0",
                    valueKey -> newLettingsRelief.inPounds().toString
                  )
                )
              )
          }
        }

        "there is an error updating the session data" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, newDraftReturn) =
                getSessionDataJourneyAndDraftReturn(
                  userType,
                  individualUserType
                )
              val newJourney                         = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(
                  Right(())
                )
                mockStoreSession(
                  session.copy(journeyStatus = Some(newJourney))
                )(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(
                  Seq(
                    key      -> "0",
                    valueKey -> newLettingsRelief.inPounds().toString
                  )
                )
              )
          }
        }

      }

      "redirect to the cya page" when {

        "the user has not answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
            val currentAnswers = sample[IncompleteReliefDetailsAnswers].copy(
              privateResidentsRelief = Some(AmountInPence(Long.MaxValue)),
              lettingsRelief = None
            )

            val newLettingsRelief = 2d

            val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              disposalDate = sample[DisposalDate].copy(
                taxYear = sample[TaxYear].copy(
                  maxLettingsReliefAmount = maxLettingsReliefValue
                )
              )
            )

            val oldDraftReturn = sample[DraftSingleDisposalReturn].copy(
              triageAnswers = triageAnswers,
              reliefDetailsAnswers = Some(currentAnswers)
            )
            val newDraftReturn = updateDraftReturn(
              oldDraftReturn,
              currentAnswers.copy(
                lettingsRelief = Some(AmountInPence.fromPounds(newLettingsRelief))
              ),
              isFurtherOrAmendReturn = true
            )

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                Seq(key -> "0", valueKey -> newLettingsRelief.toString)
              ),
              oldDraftReturn,
              newDraftReturn
            )
          }

        "the user has answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
            forAll { (c: CompleteReliefDetailsAnswers) =>
              val currentAnswers    =
                c.copy(
                  privateResidentsRelief = AmountInPence(Long.MaxValue),
                  lettingsRelief = AmountInPence.fromPounds(1d)
                )
              val newLettingsRelief = 2d
              val triageAnswers     = sample[CompleteSingleDisposalTriageAnswers].copy(
                disposalDate = sample[DisposalDate].copy(
                  taxYear = sample[TaxYear].copy(
                    maxLettingsReliefAmount = maxLettingsReliefValue
                  )
                )
              )
              val oldDraftReturn    = sample[DraftSingleDisposalReturn].copy(
                reliefDetailsAnswers = Some(currentAnswers),
                triageAnswers = triageAnswers
              )

              val newDraftReturn =
                updateDraftReturn(
                  oldDraftReturn,
                  currentAnswers.copy(
                    lettingsRelief = AmountInPence.fromPounds(newLettingsRelief)
                  ),
                  isFurtherOrAmendReturn = true
                )

              testSuccessfulUpdatesAfterSubmit(
                performAction(
                  Seq(key -> "0", valueKey -> newLettingsRelief.toString)
                ),
                oldDraftReturn,
                newDraftReturn
              )
            }
          }
      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val currentAnswers = sample[CompleteReliefDetailsAnswers].copy(
                lettingsRelief = AmountInPence.fromPounds(1d)
              )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    currentAnswers,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(Seq(key -> "0", valueKey -> "1")),
                controllers.returns.reliefdetails.routes.ReliefDetailsController
                  .checkYourAnswers()
              )
          }
        }

      }

      "accept submitted values with commas" in {
        forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
          (userType: UserType, individualUserType: IndividualUserType) =>
            val currentAnswers = sample[CompleteReliefDetailsAnswers].copy(
              lettingsRelief = AmountInPence.fromPounds(1000d)
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithReliefDetailsAnswers(
                  currentAnswers,
                  userType,
                  individualUserType
                )._1
              )
            }

            checkIsRedirect(
              performAction(Seq(key -> "0", valueKey -> "1,000")),
              controllers.returns.reliefdetails.routes.ReliefDetailsController
                .checkYourAnswers()
            )
        }
      }

      "accept submitted values with pound signs" in {
        forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
          (userType: UserType, individualUserType: IndividualUserType) =>
            val currentAnswers = sample[CompleteReliefDetailsAnswers].copy(
              lettingsRelief = AmountInPence.fromPounds(1d)
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithReliefDetailsAnswers(
                  currentAnswers,
                  userType,
                  individualUserType
                )._1
              )
            }

            checkIsRedirect(
              performAction(Seq(key -> "0", valueKey -> "£1")),
              controllers.returns.reliefdetails.routes.ReliefDetailsController
                .checkYourAnswers()
            )
        }
      }

    }

    "handling requests to display the other reliefs page" must {

      def performAction(): Future[Result] =
        controller.otherReliefs()(FakeRequest())

      val requiredPreviousAnswers = IncompleteReliefDetailsAnswers.empty.copy(
        privateResidentsRelief = Some(AmountInPence.fromPounds(1)),
        lettingsRelief = Some(AmountInPence.fromPounds(2))
      )

      val key      = "otherReliefs"
      val valueKey = "otherReliefsAmount"

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.otherReliefs(),
        mockUUIDGenerator
      )

      behave like noLettingsReliefBehaviour(() => performAction())

      val otherReliefs =
        OtherReliefs("ReliefName", AmountInPence.fromPounds(13.34))

      "redirect to lettings relief page" when {

        "the user has residents relief greater than zero but lettings relief not answered yet" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    IncompleteReliefDetailsAnswers.empty.copy(
                      privateResidentsRelief = Some(AmountInPence.fromPounds(11.34)),
                      lettingsRelief = None,
                      otherReliefs = None
                    ),
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                controllers.returns.reliefdetails.routes.ReliefDetailsController
                  .lettingsRelief()
              )
          }
        }

      }

      "display the page" when {

        "the user has not answered the question before" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    requiredPreviousAnswers.copy(otherReliefs = Some(otherReliefs)),
                    userType,
                    individualUserType
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key.h1"),
                doc => doc.select("legend").text() shouldBe messageFromMessageKey(s"$key$userKey.title")
              )
          }
        }

        "the user has answered the question before but has " +
          "not completed the relief detail section" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithReliefDetailsAnswers(
                      IncompleteReliefDetailsAnswers.empty.copy(
                        privateResidentsRelief = Some(AmountInPence.fromPounds(11.34)),
                        lettingsRelief = Some(AmountInPence.fromPounds(12.34)),
                        otherReliefs = Some(otherReliefs)
                      ),
                      userType,
                      individualUserType
                    )._1
                  )
                }

                val userKey = userMessageKey(individualUserType, userType)

                checkPageIsDisplayed(
                  performAction(),
                  messageFromMessageKey(s"$key.h1"),
                  doc => {
                    doc.select(s"#$valueKey").attr("value") shouldBe "13.34"
                    doc.select("legend").text()             shouldBe messageFromMessageKey(s"$key$userKey.title")
                  }
                )
            }
          }

        "the user has answered the question before but has " +
          "completed the relief detail section" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithReliefDetailsAnswers(
                      sample[CompleteReliefDetailsAnswers].copy(
                        privateResidentsRelief = AmountInPence.fromPounds(1.34),
                        lettingsRelief = AmountInPence.fromPounds(12.34),
                        otherReliefs = Some(otherReliefs)
                      ),
                      userType,
                      individualUserType
                    )._1
                  )
                }

                val userKey = userMessageKey(individualUserType, userType)

                checkPageIsDisplayed(
                  performAction(),
                  messageFromMessageKey(s"$key.h1"),
                  doc => {
                    doc.select(s"#$valueKey").attr("value") shouldBe "13.34"
                    doc.select("legend").text()             shouldBe messageFromMessageKey(s"$key$userKey.title")
                  }
                )
            }
          }

      }

    }

    "handling submitted answers to the other reliefs page" must {

      val key      = "otherReliefs"
      val valueKey = "otherReliefsAmount"
      val nameKey  = "otherReliefsName"

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.otherReliefsSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.otherReliefsSubmit(),
        mockUUIDGenerator
      )

      behave like noLettingsReliefBehaviour(() => performAction(Seq.empty))

      "show a form error for amount" when {

        def test(data: (String, String)*)(
          expectedErrorMessageKey: String
        )(
          userType: UserType,
          individualUserType: IndividualUserType,
          userKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers],
                userType,
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey(s"$key.h1"),
            doc => {
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text()                   shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )
              doc.select("legend").text() shouldBe messageFromMessageKey(s"$key$userKey.title")
            },
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              amountOfMoneyErrorScenarios(
                key = valueKey,
                errorContext = Some(s"$valueKey$userKey")
              ).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  val data =
                    (key -> "0") :: (nameKey -> "ReliefsName") :: scenario.formData
                  test(data*)(scenario.expectedErrorMessageKey)(
                    userType,
                    individualUserType,
                    userKey
                  )
                }
              }
          }
        }

      }

      "show a form error for name and amount" when {

        def test(data: (String, String)*)(
          expectedErrorMessageKey: String*
        )(
          userType: UserType,
          individualUserType: IndividualUserType,
          userKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithReliefDetailsAnswers(
                sample[CompleteReliefDetailsAnswers],
                userType,
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey(s"$key.h1"),
            doc => {
              expectedErrorMessageKey.toList match {
                case Nil             =>
                case errorKey :: Nil =>
                  doc
                    .select("[data-spec='errorSummaryDisplay'] a")
                    .text() shouldBe messageFromMessageKey(
                    errorKey
                  )
                case errorKeys       =>
                  val errors =
                    (1 to errorKeys.length).map(i =>
                      doc
                        .select(
                          s"[data-spec='errorSummaryDisplay'] ul > li:nth-child($i) > a"
                        )
                        .text()
                    )
                  expectedErrorMessageKey
                    .map(messageFromMessageKey(_))
                    .foreach(message => errors should contain(message))
              }
              doc.select("legend").text() shouldBe messageFromMessageKey(s"$key$userKey.title")
            },
            BAD_REQUEST
          )
        }

        "nothing is submitted" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test(key -> "0")(
                s"$nameKey$userKey.error.required",
                s"$valueKey$userKey.error.required"
              )(
                userType,
                individualUserType,
                userKey
              )
          }
        }

        "invalid characters are submitted for name and nothing submitted to amount" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test(key -> "0", nameKey -> "£££££")(
                s"$nameKey$userKey.error.invalid",
                s"$valueKey$userKey.error.required"
              )(userType, individualUserType, userKey)
          }
        }

        "empty other reliefs name" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test(key -> "0", nameKey -> "    ", valueKey -> "£1")(
                s"$nameKey$userKey.error.required"
              )(
                userType,
                individualUserType,
                userKey
              )
          }
        }

        val otherReliefsNameTooLong =
          "The other reliefs name is too long. The other reliefs name is too long. The other reliefs name is too long."

        "other reliefs name too long " in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test(
                key      -> "0",
                nameKey  -> otherReliefsNameTooLong,
                valueKey -> "£1"
              )(
                s"$nameKey$userKey.error.tooLong"
              )(userType, individualUserType, userKey)
          }
        }

      }

      "show an error page" when {

        val currentAnswers     = sample[CompleteReliefDetailsAnswers].copy(
          otherReliefs = Some(NoOtherReliefs),
          lettingsRelief = AmountInPence.zero
        )
        val currentDraftReturn = sample[DraftSingleDisposalReturn].copy(
          reliefDetailsAnswers = Some(currentAnswers),
          exemptionAndLossesAnswers = Some(sample[CompleteExemptionAndLossesAnswers]),
          yearToDateLiabilityAnswers = Some(sample[YearToDateLiabilityAnswers]),
          supportingEvidenceAnswers = Some(sample[CompleteSupportingEvidenceAnswers])
        )
        val currentJourney     = sample[FillingOutReturn].copy(
          draftReturn = currentDraftReturn,
          amendReturnData = None
        )

        val currentSession  = SessionData.empty.copy(
          journeyStatus = Some(currentJourney)
        )
        val newOtherReliefs =
          OtherReliefs("ReliefName", AmountInPence.fromPounds(2))
        val newDraftReturn  = currentDraftReturn.copy(
          reliefDetailsAnswers = Some(
            currentAnswers.copy(
              otherReliefs = Some(newOtherReliefs)
            )
          ),
          exemptionAndLossesAnswers = None,
          yearToDateLiabilityAnswers = None,
          supportingEvidenceAnswers = None
        )
        val newJourney      = currentJourney.copy(draftReturn = newDraftReturn)

        "there is an error updating the draft return" in {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
            mockStoreDraftReturn(newJourney)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(
            performAction(
              Seq(
                key      -> "0",
                nameKey  -> newOtherReliefs.name,
                valueKey -> newOtherReliefs.amount.inPounds().toString
              )
            )
          )

        }

        "there is an error updating the session data" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(currentSession)
            mockStoreDraftReturn(newJourney)(
              Right(())
            )
            mockStoreSession(
              currentSession.copy(journeyStatus = Some(newJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              Seq(
                "otherReliefs"       -> "0",
                "otherReliefsName"   -> newOtherReliefs.name,
                "otherReliefsAmount" -> newOtherReliefs.amount
                  .inPounds()
                  .toString
              )
            )
          )

        }

      }

      "redirect to lettings page" when {

        "the user has residents relief greater than zero but lettings relief not answered yet" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val otherReliefs =
                OtherReliefs("ReliefName", AmountInPence.fromPounds(2d))
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    IncompleteReliefDetailsAnswers.empty.copy(
                      privateResidentsRelief = Some(AmountInPence.fromPounds(11.34)),
                      lettingsRelief = None,
                      otherReliefs = Some(otherReliefs)
                    ),
                    userType,
                    individualUserType
                  )._1
                )
              }
              checkIsRedirect(
                performAction(
                  Seq(
                    key      -> "0",
                    nameKey  -> otherReliefs.name,
                    valueKey -> otherReliefs.amount.inPounds().toString
                  )
                ),
                controllers.returns.reliefdetails.routes.ReliefDetailsController
                  .lettingsRelief()
              )
          }
        }

      }

      "redirect to the cya page" when {

        "the user has not answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
            val currentAnswers  = sample[IncompleteReliefDetailsAnswers].copy(
              privateResidentsRelief = Some(AmountInPence.zero),
              lettingsRelief = Some(AmountInPence.zero),
              otherReliefs = None
            )
            val newOtherReliefs =
              OtherReliefs("ReliefName", AmountInPence.fromPounds(3d))
            val updatedAnswers  = currentAnswers.copy(
              otherReliefs = Some(newOtherReliefs)
            )

            val oldDraftReturn = sample[DraftSingleDisposalReturn].copy(
              reliefDetailsAnswers = Some(currentAnswers),
              yearToDateLiabilityAnswers = Some(sample[YearToDateLiabilityAnswers])
            )
            val newDraftReturn =
              oldDraftReturn.copy(
                reliefDetailsAnswers = Some(updatedAnswers),
                exemptionAndLossesAnswers = None,
                yearToDateLiabilityAnswers = None,
                supportingEvidenceAnswers = None
              )

            testSuccessfulUpdatesAfterSubmit(
              performAction(
                Seq(
                  "otherReliefs"       -> "0",
                  "otherReliefsName"   -> newOtherReliefs.name,
                  "otherReliefsAmount" -> newOtherReliefs.amount
                    .inPounds()
                    .toString
                )
              ),
              oldDraftReturn,
              newDraftReturn
            )

          }

        "the user has answered all of the relief details questions " +
          "and the draft return and session data has been successfully updated" in {
            forAll { (c: CompleteReliefDetailsAnswers) =>
              val otherReliefs    =
                OtherReliefs("ReliefName1", AmountInPence.fromPounds(1d))
              val newOtherReliefs =
                OtherReliefs("ReliefName2", AmountInPence.fromPounds(2d))

              val currentAnswers = c.copy(
                lettingsRelief = AmountInPence.zero,
                otherReliefs = Some(otherReliefs)
              )
              val updatedAnswers =
                currentAnswers.copy(otherReliefs = Some(newOtherReliefs))
              val oldDraftReturn = sample[DraftSingleDisposalReturn].copy(
                reliefDetailsAnswers = Some(currentAnswers)
              )
              val newDraftReturn =
                oldDraftReturn.copy(
                  reliefDetailsAnswers = Some(updatedAnswers),
                  exemptionAndLossesAnswers = None,
                  yearToDateLiabilityAnswers = oldDraftReturn.yearToDateLiabilityAnswers.flatMap(
                    _.unsetAllButIncomeDetails()
                  )
                )

              testSuccessfulUpdatesAfterSubmit(
                performAction(
                  Seq(
                    "otherReliefs"       -> "0",
                    "otherReliefsName"   -> newOtherReliefs.name,
                    "otherReliefsAmount" -> newOtherReliefs.amount
                      .inPounds()
                      .toString
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
          val otherReliefs   =
            OtherReliefs("ReliefName1", AmountInPence.fromPounds(1d))
          val currentAnswers = sample[CompleteReliefDetailsAnswers]
            .copy(otherReliefs = Some(otherReliefs))
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    currentAnswers,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(
                  Seq(
                    key      -> "0",
                    nameKey  -> otherReliefs.name,
                    valueKey -> otherReliefs.amount.inPounds().toString
                  )
                ),
                controllers.returns.reliefdetails.routes.ReliefDetailsController
                  .checkYourAnswers()
              )
          }
        }

      }

      "accept submitted values with commas" in {

        val otherReliefs   =
          OtherReliefs("ReliefName1", AmountInPence.fromPounds(1000d))
        val currentAnswers = sample[CompleteReliefDetailsAnswers].copy(
          otherReliefs = Some(otherReliefs)
        )

        forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
          (userType: UserType, individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithReliefDetailsAnswers(
                  currentAnswers,
                  userType,
                  individualUserType
                )._1
              )
            }

            checkIsRedirect(
              performAction(
                Seq(key -> "0", nameKey -> "ReliefName1", valueKey -> "1,000")
              ),
              controllers.returns.reliefdetails.routes.ReliefDetailsController
                .checkYourAnswers()
            )
        }
      }

      "accept submitted values with pound signs" in {

        val otherReliefs   =
          OtherReliefs("ReliefName1", AmountInPence.fromPounds(1000d))
        val currentAnswers =
          sample[CompleteReliefDetailsAnswers]
            .copy(otherReliefs = Some(otherReliefs))

        forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
          (userType: UserType, individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithReliefDetailsAnswers(
                  currentAnswers,
                  userType,
                  individualUserType
                )._1
              )
            }

            checkIsRedirect(
              performAction(
                Seq(key -> "0", nameKey -> "ReliefName1", valueKey -> "£1,000")
              ),
              controllers.returns.reliefdetails.routes.ReliefDetailsController
                .checkYourAnswers()
            )
        }
      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

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

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswers(),
        mockUUIDGenerator
      )

      "redirect to the private residence relief page" when {

        "there are no relief details answers in session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {

                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    None,
                    userType,
                    individualUserType,
                    isAmend = false
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.ReliefDetailsController.privateResidentsRelief()
              )
          }
        }

        "there are relief details in session but no answer for the private residence relief question" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    allQuestionsAnswered.copy(privateResidentsRelief = None),
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.ReliefDetailsController.privateResidentsRelief()
              )
          }

        }
      }

      "redirect to the lettings relief page" when {

        "the user has not answered that question and the amount of private residence relief is greater than zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    allQuestionsAnswered.copy(
                      privateResidentsRelief = Some(AmountInPence(20)),
                      lettingsRelief = None,
                      otherReliefs = None
                    ),
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.ReliefDetailsController.lettingsRelief()
              )
          }
        }

      }

      "redirect to the other CGT reliefs page" when {

        "the user has not answered that question and the amount of private residence relief is zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    allQuestionsAnswered.copy(
                      privateResidentsRelief = Some(AmountInPence(0)),
                      lettingsRelief = None,
                      otherReliefs = None
                    ),
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.ReliefDetailsController.otherReliefs()
              )
          }
        }

      }

      "redirect to the other reliefs page" when {

        "the user has not answered that question" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    allQuestionsAnswered.copy(otherReliefs = None),
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.ReliefDetailsController.otherReliefs()
              )
          }
        }

        "the user has not answered lettings relief question as the user selected No for residents relief " in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val sessionData = sessionWithReliefDetailsAnswers(
                IncompleteReliefDetailsAnswers(
                  Some(AmountInPence(0)),
                  None,
                  None
                ),
                userType,
                individualUserType
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

      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, draftReturn) =
                sessionWithReliefDetailsAnswers(
                  allQuestionsAnswered,
                  userType,
                  individualUserType
                )

              val newDraftReturn = draftReturn.copy(
                reliefDetailsAnswers = Some(completeAnswers)
              )
              val newJourney     = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, draftReturn) =
                sessionWithReliefDetailsAnswers(
                  allQuestionsAnswered,
                  userType,
                  individualUserType
                )

              val newDraftReturn = draftReturn.copy(reliefDetailsAnswers = Some(completeAnswers))
              val newJourney     = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(Right(()))
                mockStoreSession(
                  session.copy(journeyStatus = Some(newJourney))
                )(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
          }
        }

      }

      "show the page" when {

        "the user has just answered all the questions and all updates are successful" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, draftReturn) = sessionWithReliefDetailsAnswers(
                allQuestionsAnswered,
                userType,
                individualUserType
              )
              val newDraftReturn                  = draftReturn.copy(
                reliefDetailsAnswers = Some(completeAnswers)
              )
              val updatedJourney                  = journey.copy(
                draftReturn = newDraftReturn
              )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(
                  Right(())
                )
                mockStoreSession(
                  session.copy(journeyStatus = Some(updatedJourney))
                )(Right(()))
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("reliefDetails.cya.title"),
                doc =>
                  doc
                    .select("#main-content form")
                    .attr("action") shouldBe routes.ReliefDetailsController
                    .checkYourAnswersSubmit()
                    .url
              )
          }
        }

        "the user has already answered all the questions" in {
          forAll { (completeAnswers: CompleteReliefDetailsAnswers) =>
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithReliefDetailsAnswers(
                      completeAnswers,
                      userType,
                      individualUserType
                    )._1
                  )
                }

                val isPOA = isPeriodOfAdmin(individualUserType)

                checkPageIsDisplayed(
                  performAction(),
                  messageFromMessageKey("reliefDetails.cya.title"),
                  { doc =>
                    validateReliefDetailsCheckYourAnswersPage(
                      completeAnswers,
                      doc,
                      isPOA
                    )
                    doc
                      .select("#content > article > form, #main-content form")
                      .attr("action") shouldBe routes.ReliefDetailsController
                      .checkYourAnswersSubmit()
                      .url
                  }
                )
            }
          }
        }

        "the user has set residential relief to no" in {
          val completeAnswersWithoutResidentialRelief =
            CompleteReliefDetailsAnswers(
              AmountInPence(0),
              AmountInPence(0),
              Some(sample[OtherReliefs])
            )

          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithReliefDetailsAnswers(
                    completeAnswersWithoutResidentialRelief,
                    userType,
                    individualUserType
                  )._1
                )
              }

              val isPOA = isPeriodOfAdmin(individualUserType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("reliefDetails.cya.title"),
                { doc =>
                  validateReliefDetailsCheckYourAnswersPage(
                    completeAnswersWithoutResidentialRelief,
                    doc,
                    isPOA
                  )
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action") shouldBe routes.ReliefDetailsController
                    .checkYourAnswersSubmit()
                    .url
                }
              )
          }
        }

      }

    }

    "handling submitted answers to the check your answers page" must {
      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswersSubmit(),
        mockUUIDGenerator
      )

      "redirect to the task list page" in {
        forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
          (userType: UserType, individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithReliefDetailsAnswers(
                  sample[CompleteReliefDetailsAnswers],
                  userType,
                  individualUserType
                )._1
              )
            }

            checkIsRedirect(
              performAction(),
              controllers.returns.routes.TaskListController.taskList()
            )
        }
      }

    }

  }

  def noPrivateResidentsReliefBehaviour(
    performAction: () => Future[Result]
  ): Unit =
    "redirect to the what was your private residence relief page" when {

      "there is no private residence relief " in {
        forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
          (userType: UserType, individualUserType: IndividualUserType) =>
            val sessionData = sessionWithReliefDetailsAnswers(
              IncompleteReliefDetailsAnswers(
                None,
                Some(sample[AmountInPence]),
                Some(sample[OtherReliefsOption])
              ),
              userType,
              individualUserType
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

    }

  def noLettingsReliefBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the what was your lettings relief page" when {

      "there is no lettings relief " in {
        forAll(acceptedUserTypeGen, acceptedIndividualUserTypeForLettingsRelief) {
          (userType: UserType, individualUserType: IndividualUserType) =>
            val sessionData = sessionWithReliefDetailsAnswers(
              IncompleteReliefDetailsAnswers(
                Some(sample[AmountInPence]),
                None,
                Some(sample[OtherReliefsOption])
              ),
              userType,
              individualUserType
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

    }

  def testSuccessfulUpdatesAfterSubmit(
    result: => Future[Result],
    oldDraftReturn: DraftSingleDisposalReturn,
    newDraftReturn: DraftSingleDisposalReturn
  ): Unit = {
    val journey    = sample[FillingOutReturn].copy(
      draftReturn = oldDraftReturn,
      amendReturnData = Some(sample[AmendReturnData])
    )
    val session    = SessionData.empty.copy(journeyStatus = Some(journey))
    val newJourney = journey.copy(draftReturn = newDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreDraftReturn(newJourney)(
        Right(())
      )
      mockStoreSession(
        session.copy(journeyStatus = Some(newJourney))
      )(Right(()))
    }

    checkIsRedirect(result, routes.ReliefDetailsController.checkYourAnswers())
  }

}

object ReliefDetailsControllerSpec extends Matchers {
  def validateReliefDetailsCheckYourAnswersPage(
    reliefDetailsAnswers: CompleteReliefDetailsAnswers,
    doc: Document,
    isPeriodOfAdmin: Boolean = false
  ): Unit = {

    if (reliefDetailsAnswers.privateResidentsRelief.isZero) {
      doc.select("#privateResidentsReliefValue-answer").text shouldBe "No"
    } else {
      doc.select("#privateResidentsRelief-answer").text shouldBe "Yes"
      doc
        .select("#privateResidentsReliefValue-answer")
        .text                                           shouldBe formatAmountOfMoneyWithPoundSign(
        reliefDetailsAnswers.privateResidentsRelief.inPounds()
      )
    }

    if (!isPeriodOfAdmin) {
      if (reliefDetailsAnswers.privateResidentsRelief.isPositive) {
        if (reliefDetailsAnswers.lettingsRelief.isZero) {
          doc.select("#lettingsReliefValue-answer").text shouldBe "No"
        } else {
          doc.select("#lettingsRelief-answer").text shouldBe "Yes"
          doc
            .select("#lettingsReliefValue-answer")
            .text                                   shouldBe formatAmountOfMoneyWithPoundSign(
            reliefDetailsAnswers.lettingsRelief.inPounds()
          )
        }
      } else {
        doc.select("#lettingsReliefValue-answer").hasText shouldBe false
        doc.select("#lettingsRelief-answer").hasText      shouldBe false
      }
    }

    reliefDetailsAnswers.otherReliefs.foreach {
      case a: OtherReliefsOption.OtherReliefs =>
        doc.select("#otherReliefs-answer").text     shouldBe "Yes"
        doc.select("#otherReliefsName-answer").text shouldBe a.name
        doc
          .select("#otherReliefsAmount-answer")
          .text                                     shouldBe formatAmountOfMoneyWithPoundSign(
          a.amount.inPounds().bigDecimal
        )
      case OtherReliefsOption.NoOtherReliefs  =>
        doc.select("#otherReliefs-answer").text shouldBe "No"

    }

  }

}
