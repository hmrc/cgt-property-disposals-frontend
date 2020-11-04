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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage

import java.time.{Clock, LocalDate}

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.Configuration
import play.api.i18n.{Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TaxYearGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData, StartingNewDraftReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsTriageAnswers, IncompleteMultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{CompleteReturnWithSummary, Error, JourneyStatus, SessionData, TaxYear, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee.{routes => representeeRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage.{routes => homePageRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.Residential
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.{CompleteMultipleDisposalsReturn, CompleteSingleDisposalReturn, CompleteSingleMixedUseDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.IncompleteReliefDetailsAnswers

import scala.concurrent.Future

class CommonTriageQuestionsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with ReturnsServiceSupport
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  val mockUUIDGenerator = mock[UUIDGenerator]

  override lazy val additionalConfig = Configuration(
    "amend-returns.enabled" -> true
  )

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    )

  lazy val controller = instanceOf[CommonTriageQuestionsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit val messages: Messages = MessagesImpl(lang, messagesApi)

  def isValidJourney(journeyStatus: JourneyStatus): Boolean =
    journeyStatus match {
      case _: StartingNewDraftReturn | _: FillingOutReturn | _: StartingToAmendReturn => true
      case _                                                                          => false
    }

  def setNameForUserType(
    userType: UserType
  ): Either[TrustName, IndividualName] =
    userType match {
      case UserType.Organisation => Left(sample[TrustName])
      case _                     => Right(sample[IndividualName])
    }

  def setAgentReferenceNumber(
    userType: UserType
  ): Option[AgentReferenceNumber] =
    userType match {
      case UserType.Agent => Some(sample[AgentReferenceNumber])
      case _              => None
    }

  def userMessageKey(userType: UserType): String =
    userType match {
      case UserType.Individual   => ""
      case UserType.Organisation => ".trust"
      case UserType.Agent        => ".agent"
      case other                 => sys.error(s"User type '$other' not handled")
    }

  def sessionDataWithStartingNewDraftReturn(
    triageAnswers: Either[
      MultipleDisposalsTriageAnswers,
      SingleDisposalTriageAnswers
    ],
    name: Either[TrustName, IndividualName],
    userType: UserType = UserType.Individual,
    representeeAnswers: Option[RepresenteeAnswers] = None,
    previousSentReturns: Option[PreviousReturnData] = None
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn =
      sample[StartingNewDraftReturn].copy(
        subscribedDetails = sample[SubscribedDetails].copy(name = name),
        newReturnTriageAnswers = triageAnswers,
        agentReferenceNumber = setAgentReferenceNumber(userType),
        representeeAnswers = representeeAnswers,
        previousSentReturns = previousSentReturns
      )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(startingNewDraftReturn),
      userType = Some(userType)
    )

    sessionData -> startingNewDraftReturn
  }

  def sessionDataWithFillingOutReturn(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    userType: UserType = UserType.Individual,
    previousReturns: Option[PreviousReturnData] = None,
    amendReturnData: Option[AmendReturnData] = None,
    representeeAnswers: Option[RepresenteeAnswers] = None
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn      = sample[DraftSingleDisposalReturn].copy(
      triageAnswers = singleDisposalTriageAnswers,
      gainOrLossAfterReliefs = None,
      exemptionAndLossesAnswers = None,
      reliefDetailsAnswers =
        if (singleDisposalTriageAnswers.isPeriodOfAdmin())
          Some(IncompleteReliefDetailsAnswers.empty)
        else None,
      representeeAnswers = representeeAnswers
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      previousSentReturns = previousReturns,
      amendReturnData = amendReturnData
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn),
      userType = Some(userType)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  def sessionDataWithFillingOutReturnForSingleIndirectDisposal(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    userType: UserType = UserType.Individual,
    previousReturns: Option[PreviousReturnData] = None,
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) = {
    val draftReturn      = sample[DraftSingleIndirectDisposalReturn].copy(
      triageAnswers = singleDisposalTriageAnswers,
      gainOrLossAfterReliefs = None,
      exemptionAndLossesAnswers = None
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      previousSentReturns = previousReturns,
      amendReturnData =
        if (isAmend)
          Some(sample[AmendReturnData])
        else None
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn),
      userType = Some(userType)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  def sessionDataWithFillingOutReturnForSingleMixedUseDisposal(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    userType: UserType = UserType.Individual,
    previousReturns: Option[PreviousReturnData] = None,
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) = {
    val draftReturn      = sample[DraftSingleMixedUseDisposalReturn].copy(
      triageAnswers = singleDisposalTriageAnswers,
      gainOrLossAfterReliefs = None,
      exemptionAndLossesAnswers = None
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      previousSentReturns = previousReturns,
      amendReturnData =
        if (isAmend)
          Some(sample[AmendReturnData])
        else None
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn),
      userType = Some(userType)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  def sessionDataWithFillingOutReturnForMultpleDisposals(
    multipleDisposalsTriageAnswers: MultipleDisposalsTriageAnswers,
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn      = sample[DraftMultipleDisposalsReturn].copy(
      triageAnswers = multipleDisposalsTriageAnswers
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
      amendReturnData =
        if (isAmend)
          Some(sample[AmendReturnData])
        else None,
      previousSentReturns = None
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  def sessionDataWithFillingOutReturnForMultpleIndirectDisposals(
    multipleDisposalsTriageAnswers: MultipleDisposalsTriageAnswers,
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftMultipleIndirectDisposalsReturn) = {
    val draftReturn      = sample[DraftMultipleIndirectDisposalsReturn].copy(
      triageAnswers = multipleDisposalsTriageAnswers
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
      amendReturnData =
        if (isAmend)
          Some(sample[AmendReturnData])
        else None
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  "CommonTriageQuestionsController" when {

    "handling requests to display the who is individual representing page" must {

      def performAction(): Future[Result] =
        controller.whoIsIndividualRepresenting()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whoIsIndividualRepresenting(),
        mockUUIDGenerator
      )

      "redirect to the how many properties page" when {

        "the user is a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(IncompleteSingleDisposalTriageAnswers.empty),
                Left(sample[TrustName])
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.CommonTriageQuestionsController.howManyProperties()
          )
        }

      }

      "display the page" when {

        "the user is starting a new draft return and" when {

          "the user has not answered any triage questions yet" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(IncompleteSingleDisposalTriageAnswers.empty),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("who-are-you-reporting-for.title"),
              { doc =>
                doc
                  .select("#content > article > form")
                  .attr(
                    "action"
                  ) shouldBe routes.CommonTriageQuestionsController
                  .whoIsIndividualRepresentingSubmit()
                  .url

                doc
                  .select("#individualUserType > div:nth-child(2) > label")
                  .text()                          shouldBe messageFromMessageKey(
                  s"individualUserType.${IndividualUserType.Self}"
                )
                doc
                  .select("#individualUserType > div:nth-child(3) > label")
                  .html()                          shouldBe messageFromMessageKey(
                  s"individualUserType.${IndividualUserType.Capacitor}"
                )
                doc
                  .select("#individualUserType > div:nth-child(4) > label")
                  .html()                          shouldBe messageFromMessageKey(
                  s"individualUserType.${IndividualUserType.PersonalRepresentative}"
                )
                doc
                  .select("#individualUserType > div:nth-child(5) > label")
                  .html()                          shouldBe messageFromMessageKey(
                  s"individualUserType.${IndividualUserType.PersonalRepresentativeInPeriodOfAdmin}"
                )
                doc.select("#submitButton").text() shouldBe messageFromMessageKey("button.continue")
              }
            )
          }

          "the user is on the single disposal journey and has already answered the question" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturn(
                  IncompleteSingleDisposalTriageAnswers.empty.copy(
                    individualUserType = Some(IndividualUserType.Self)
                  )
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("who-are-you-reporting-for.title"),
              { doc =>
                doc
                  .select("#content > article > form")
                  .attr(
                    "action"
                  )                shouldBe routes.CommonTriageQuestionsController
                  .whoIsIndividualRepresentingSubmit()
                  .url
                doc
                  .select("#individualUserType-0")
                  .attr("checked") shouldBe "checked"
              }
            )
          }

          "the user is on the multiple disposals journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(
                    IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                      individualUserType = Some(IndividualUserType.Capacitor)
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("who-are-you-reporting-for.title"),
              { doc =>
                doc
                  .select("#content > article > form")
                  .attr(
                    "action"
                  )                                shouldBe routes.CommonTriageQuestionsController
                  .whoIsIndividualRepresentingSubmit()
                  .url
                doc
                  .select("#individualUserType-1")
                  .attr("checked")                 shouldBe "checked"
                doc.select("#submitButton").text() shouldBe messageFromMessageKey("button.continue")
              }
            )
          }

          "the user is an agent representing an individual" in {
            List(
              IndividualUserType.Self,
              IndividualUserType.PersonalRepresentative
            ).zipWithIndex.foreach { case (value, index) =>
              val (session, journey) =
                sessionDataWithStartingNewDraftReturn(
                  Right(
                    IncompleteSingleDisposalTriageAnswers.empty
                  ),
                  Right(sample[IndividualName])
                )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  session.copy(
                    userType = Some(UserType.Agent),
                    journeyStatus = Some(
                      journey.copy(
                        agentReferenceNumber = Some(sample[AgentReferenceNumber]),
                        newReturnTriageAnswers = Right(
                          IncompleteSingleDisposalTriageAnswers.empty.copy(
                            individualUserType = Some(value)
                          )
                        )
                      )
                    )
                  )
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("who-are-you-reporting-for.title"),
                { doc =>
                  doc
                    .select("#content > article > form")
                    .attr(
                      "action"
                    ) shouldBe routes.CommonTriageQuestionsController
                    .whoIsIndividualRepresentingSubmit()
                    .url

                  doc
                    .select(s"#individualUserType-$index")
                    .attr("checked")                 shouldBe "checked"
                  doc
                    .select("#individualUserType > div:nth-child(2) > label")
                    .text()                          shouldBe messageFromMessageKey(
                    s"individualUserType.agent.${IndividualUserType.Self}"
                  )
                  doc
                    .select("#individualUserType > div:nth-child(3) > label")
                    .html()                          shouldBe messageFromMessageKey(
                    s"individualUserType.agent.${IndividualUserType.PersonalRepresentative}"
                  )
                  doc.body().text() shouldNot include(
                    messageFromMessageKey(
                      s"individualUserType.${IndividualUserType.Capacitor}"
                    )
                  )
                  doc.select("#submitButton").text() shouldBe messageFromMessageKey("button.continue")
                }
              )
            }
          }

        }

      }

    }

    "handling submitted answers to the who is individual representing page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.whoIsIndividualRepresentingSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*)
        )

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        isValidJourney
      )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whoIsIndividualRepresentingSubmit(),
        mockUUIDGenerator
      )

      "redirect to the how many properties page" when {

        "the user is a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(IncompleteSingleDisposalTriageAnswers.empty),
                Left(sample[TrustName])
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.CommonTriageQuestionsController.howManyProperties()
          )
        }

      }

      "show a form error" when {

        def test(
          formData: (String, String)*
        )(expectedErrorKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                    individualUserType = Some(IndividualUserType.Capacitor)
                  )
                ),
                Right(sample[IndividualName])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey("who-are-you-reporting-for.title"),
            { doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text()                          shouldBe messageFromMessageKey(expectedErrorKey)
              doc.title()                          should startWith("Error:")
              doc.select("#submitButton").text() shouldBe messageFromMessageKey("button.continue")
            },
            BAD_REQUEST
          )
        }

        "nothing has been submitted" in {
          test()("individualUserType.error.required")
        }

        "the option submitted has not been recognised" in {
          test("individualUserType" -> "10")("individualUserType.error.invalid")
        }

      }

      "show an error page" when {

        val formData                                 = "individualUserType" -> "0"
        val (session, fillingOutReturn, draftReturn) =
          sessionDataWithFillingOutReturn(
            IncompleteSingleDisposalTriageAnswers.empty
          )
        val updatedJourney                           = fillingOutReturn.copy(
          draftReturn = draftReturn.copy(
            triageAnswers = IncompleteSingleDisposalTriageAnswers.empty.copy(
              individualUserType = Some(IndividualUserType.Self)
            ),
            representeeAnswers = None,
            propertyAddress = None,
            disposalDetailsAnswers = None,
            acquisitionDetailsAnswers = None,
            reliefDetailsAnswers = None,
            yearToDateLiabilityAnswers = None,
            initialGainOrLoss = None,
            supportingEvidenceAnswers = None
          )
        )

        "there is an error updating a draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

      }

      "handle valid data" when {

        "the user is starting a new draft return and" when {

          "the user had not answered any questions in the triage section yet" in {
            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "0"),
              Right(IncompleteSingleDisposalTriageAnswers.empty),
              Right(sample[IndividualName])
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty
                  .copy(individualUserType = Some(IndividualUserType.Self))
              ),
              routes.SingleDisposalsTriageController.checkYourAnswers(),
              updateRepresenteeAnswers = _ => None
            )
          }

          "the user is on a single disposal journey and they haven't completed the triage section" in {
            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "1"),
              Right(
                IncompleteSingleDisposalTriageAnswers.empty
                  .copy(individualUserType = Some(IndividualUserType.Self))
              ),
              Right(sample[IndividualName])
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty
                  .copy(individualUserType = Some(IndividualUserType.Capacitor))
              ),
              routes.SingleDisposalsTriageController.checkYourAnswers(),
              updateRepresenteeAnswers = _ => None
            )
          }

          "the user is on a single disposal journey and they have completed the triage section" in {
            val answers =
              sample[CompleteSingleDisposalTriageAnswers]
                .copy(individualUserType = Some(IndividualUserType.Self))

            val newAnswers =
              IncompleteSingleDisposalTriageAnswers
                .fromCompleteAnswers(answers)
                .copy(
                  wasAUKResident = None,
                  countryOfResidence = None,
                  assetType = None,
                  disposalDate = None,
                  tooEarlyDisposalDate = None,
                  completionDate = None,
                  individualUserType = Some(IndividualUserType.PersonalRepresentative)
                )

            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "2"),
              Right(answers),
              Right(sample[IndividualName])
            )(
              Right(newAnswers),
              routes.SingleDisposalsTriageController.checkYourAnswers(),
              updateRepresenteeAnswers = _ => None
            )
          }

          "the user is on a multiple disposals journey and they haven't completed the triage section" in {
            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "1"),
              Left(
                IncompleteMultipleDisposalsTriageAnswers.empty
                  .copy(individualUserType = Some(IndividualUserType.Self))
              ),
              Right(sample[IndividualName])
            )(
              Left(
                IncompleteMultipleDisposalsTriageAnswers.empty
                  .copy(individualUserType = Some(IndividualUserType.Capacitor))
              ),
              routes.MultipleDisposalsTriageController.checkYourAnswers(),
              updateRepresenteeAnswers = _ => None
            )
          }

          "the user is on a multiple disposals journey and they have completed the triage section" in {
            val answers =
              sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(IndividualUserType.Self))

            val newAnswers =
              IncompleteMultipleDisposalsTriageAnswers
                .fromCompleteAnswers(answers)
                .copy(
                  wasAUKResident = None,
                  countryOfResidence = None,
                  assetTypes = None,
                  wereAllPropertiesResidential = None,
                  taxYear = None,
                  taxYearAfter6April2020 = None,
                  completionDate = None,
                  individualUserType = Some(IndividualUserType.PersonalRepresentative)
                )

            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("individualUserType" -> "2"),
              Left(answers),
              Right(sample[IndividualName])
            )(
              Left(newAnswers),
              routes.MultipleDisposalsTriageController.checkYourAnswers(),
              updateRepresenteeAnswers = _ => None
            )

          }
        }

        "the user is filling in a return and" when {

          "the user is on a single disposal journey and they have completed the triage section" in {
            val answers = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = Some(IndividualUserType.Self)
            )

            val newAnswers =
              IncompleteSingleDisposalTriageAnswers
                .fromCompleteAnswers(answers)
                .copy(
                  wasAUKResident = None,
                  countryOfResidence = None,
                  assetType = None,
                  disposalDate = None,
                  tooEarlyDisposalDate = None,
                  completionDate = None,
                  individualUserType = Some(IndividualUserType.PersonalRepresentative)
                )

            testSuccessfulUpdateFillingOutReturn(
              performAction("individualUserType" -> "2"),
              answers,
              amendReturnData = None
            )(
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn.copy(draftReturn =
                  draftReturn.copy(
                    triageAnswers = newAnswers,
                    representeeAnswers = None,
                    propertyAddress = None,
                    disposalDetailsAnswers = None,
                    acquisitionDetailsAnswers = None,
                    reliefDetailsAnswers = None,
                    yearToDateLiabilityAnswers = None,
                    initialGainOrLoss = None,
                    supportingEvidenceAnswers = None
                  )
                ),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a multiple disposal journey and they have completed the triage section" in {
            val answers =
              sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(IndividualUserType.Self))

            val newAnswers =
              IncompleteMultipleDisposalsTriageAnswers
                .fromCompleteAnswers(answers)
                .copy(
                  wasAUKResident = None,
                  countryOfResidence = None,
                  assetTypes = None,
                  wereAllPropertiesResidential = None,
                  taxYear = None,
                  taxYearAfter6April2020 = None,
                  completionDate = None,
                  individualUserType = Some(IndividualUserType.PersonalRepresentative)
                )

            testSuccessfulUpdateFillingOutReturn(
              performAction("individualUserType" -> "2"),
              answers
            )(
              d =>
                d.copy(
                  triageAnswers = newAnswers,
                  representeeAnswers = None,
                  examplePropertyDetailsAnswers = None,
                  yearToDateLiabilityAnswers = None,
                  supportingEvidenceAnswers = None,
                  exemptionAndLossesAnswers = None,
                  gainOrLossAfterReliefs = None
                ),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

      }

      "not do any updates" when {

        "the user has submitted the same answer they have previously entered before" in {
          val answers =
            IncompleteSingleDisposalTriageAnswers.empty
              .copy(individualUserType = Some(IndividualUserType.Self))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(answers),
                Right(sample[IndividualName])
              )._1
            )
          }

          checkIsRedirect(
            performAction("individualUserType" -> "0"),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

        "the user changes from self to other " in {
          testRedirectedWithoutUpdateAmendFillingOutReturn(
            performAction("individualUserType" -> "1"),
            IncompleteSingleDisposalTriageAnswers.empty
              .copy(individualUserType = Some(IndividualUserType.Self))
          )(
            routes.CommonTriageQuestionsController.amendWhoAreYouSubmittingFor()
          )

        }

      }

    }

    "handling requests to display the number of properties page" must {

      def performAction(): Future[Result] =
        controller.howManyProperties()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.howManyProperties(),
        mockUUIDGenerator
      )

      "redirect to the further return number of properties page" when {

        "the user is on a further return journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(IncompleteSingleDisposalTriageAnswers.empty),
                Left(sample[TrustName]),
                previousSentReturns = Some(
                  sample[PreviousReturnData].copy(
                    summaries = List(sample[ReturnSummary])
                  )
                )
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.CommonTriageQuestionsController.howManyPropertiesFurtherReturn())
        }

      }

      "display the page" when {

        "the user has not answered the question yet" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(IncompleteSingleDisposalTriageAnswers.empty),
                Left(sample[TrustName])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("numberOfProperties.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe ""
              doc
                .select("#content > article > form")
                .attr("action")                shouldBe routes.CommonTriageQuestionsController
                .howManyPropertiesSubmit()
                .url

            }
          )
        }

        "the user is on the single disposal journey, selected 'self' individual user type and" +
          "has already answered the question" in {
            val sessionData      = sessionDataWithFillingOutReturn(
              IncompleteSingleDisposalTriageAnswers.empty.copy(
                individualUserType = Some(IndividualUserType.Self),
                hasConfirmedSingleDisposal = true
              )
            )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionData._1
              )
            }
            val expectedBacklink =
              routes.CommonTriageQuestionsController.whoIsIndividualRepresenting().url

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("numberOfProperties.title"),
              { doc =>
                doc
                  .select("#back")
                  .attr("href")    shouldBe expectedBacklink
                doc
                  .select("#content > article > form")
                  .attr("action")  shouldBe routes.CommonTriageQuestionsController
                  .howManyPropertiesSubmit()
                  .url
                doc
                  .select("#numberOfProperties-0")
                  .attr("checked") shouldBe "checked"
              }
            )
          }

        "the user is on the single disposal journey, selected 'capacitor' individual user type and" +
          "has already answered the question" in {
            val sessionData      = sessionDataWithFillingOutReturn(
              IncompleteSingleDisposalTriageAnswers.empty.copy(
                individualUserType = Some(IndividualUserType.Capacitor),
                hasConfirmedSingleDisposal = true
              )
            )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionData._1
              )
            }
            val expectedBacklink = representeeRoutes.RepresenteeController.checkYourAnswers()

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("numberOfProperties.title"),
              { doc =>
                doc
                  .select("#back")
                  .attr(
                    "href"
                  )                shouldBe expectedBacklink.url
                doc
                  .select("#content > article > form")
                  .attr("action")  shouldBe routes.CommonTriageQuestionsController
                  .howManyPropertiesSubmit()
                  .url
                doc
                  .select("#numberOfProperties-0")
                  .attr("checked") shouldBe "checked"
              }
            )
          }

        "the user is on the single disposal journey, selected 'personal representative' individual user type and" +
          "has already answered the question" in {
            val sessionData      = sessionDataWithFillingOutReturn(
              IncompleteSingleDisposalTriageAnswers.empty.copy(
                individualUserType = Some(IndividualUserType.PersonalRepresentative),
                hasConfirmedSingleDisposal = true
              )
            )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionData._1
              )
            }
            val expectedBacklink =
              returns.representee.routes.RepresenteeController.checkYourAnswers()

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("numberOfProperties.title"),
              { doc =>
                doc
                  .select("#back")
                  .attr(
                    "href"
                  )                shouldBe expectedBacklink.url
                doc
                  .select("#content > article > form")
                  .attr("action")  shouldBe routes.CommonTriageQuestionsController
                  .howManyPropertiesSubmit()
                  .url
                doc
                  .select("#numberOfProperties-0")
                  .attr("checked") shouldBe "checked"
              }
            )
          }

        "the user is on the multiple disposals journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                    individualUserType = None
                  )
                ),
                Left(sample[TrustName])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("numberOfProperties.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe ""
              doc
                .select("#content > article > form")
                .attr("action")                shouldBe routes.CommonTriageQuestionsController
                .howManyPropertiesSubmit()
                .url
              doc
                .select("#numberOfProperties-1")
                .attr("checked")               shouldBe "checked"
            }
          )
        }

        "the user is on the single disposals journey and has completed the triage section " +
          "it has not started a saved a draft return yet" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(sample[CompleteSingleDisposalTriageAnswers]),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("numberOfProperties.title"),
              { doc =>
                doc
                  .select("#back")
                  .attr("href")    shouldBe routes.SingleDisposalsTriageController
                  .checkYourAnswers()
                  .url
                doc
                  .select("#content > article > form")
                  .attr("action")  shouldBe routes.CommonTriageQuestionsController
                  .howManyPropertiesSubmit()
                  .url
                doc
                  .select("#numberOfProperties-0")
                  .attr("checked") shouldBe "checked"
              }
            )
          }

        "the user is on the single disposals journey and has completed the triage section " +
          "it hsa not started and has saved a draft return yet" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturn(
                  sample[CompleteSingleDisposalTriageAnswers]
                )._1
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("numberOfProperties.title"),
              { doc =>
                doc
                  .select("#back")
                  .attr("href")    shouldBe routes.SingleDisposalsTriageController
                  .checkYourAnswers()
                  .url
                doc
                  .select("#content > article > form")
                  .attr("action")  shouldBe routes.CommonTriageQuestionsController
                  .howManyPropertiesSubmit()
                  .url
                doc
                  .select("#numberOfProperties-0")
                  .attr("checked") shouldBe "checked"
              }
            )
          }

      }

    }

    "handling submitted answers to the number of properties page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.howManyPropertiesSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*)
        )

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        isValidJourney
      )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.howManyPropertiesSubmit(),
        mockUUIDGenerator
      )

      "redirect to the further return number of properties page" when {

        "the user is on a further return journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(IncompleteSingleDisposalTriageAnswers.empty),
                Left(sample[TrustName]),
                previousSentReturns = Some(
                  sample[PreviousReturnData].copy(
                    summaries = List(sample[ReturnSummary])
                  )
                )
              )._1
            )
          }

          checkIsRedirect(
            performAction("numberOfProperties" -> "0"),
            routes.CommonTriageQuestionsController.howManyPropertiesFurtherReturn()
          )
        }

      }

      "show a form error" when {

        def test(
          formData: (String, String)*
        )(expectedErrorKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                    individualUserType = Some(IndividualUserType.Capacitor)
                  )
                ),
                Right(sample[IndividualName])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey("numberOfProperties.title"),
            { doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(expectedErrorKey)

              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.CommonTriageQuestionsController
                .howManyPropertiesSubmit()
                .url
            },
            BAD_REQUEST
          )
        }

        "nothing has been submitted" in {
          test()("numberOfProperties.error.required")
        }

        "the option submitted has not been recognised" in {
          test("numberOfProperties" -> "2")("numberOfProperties.error.invalid")

        }

      }

      "show an error page" when {

        "there is an error updating a draft return" in {
          val formData                        = "numberOfProperties" -> "0"
          val (session, journey, draftReturn) = sessionDataWithFillingOutReturnForMultpleDisposals(
            IncompleteMultipleDisposalsTriageAnswers.empty.copy(
              individualUserType = Some(IndividualUserType.Self)
            )
          )

          val updatedDraftReturn =
            DraftSingleDisposalReturn.newDraftReturn(
              draftReturn.id,
              IncompleteSingleDisposalTriageAnswers.empty.copy(
                individualUserType = Some(IndividualUserType.Self),
                hasConfirmedSingleDisposal = true
              ),
              draftReturn.representeeAnswers
            )

          val updatedJourney = journey.copy(draftReturn = updatedDraftReturn)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }

        "there is an error updating the session" in {
          val formData           = "numberOfProperties" -> "0"
          val (session, journey) = sessionDataWithStartingNewDraftReturn(
            Right(
              IncompleteSingleDisposalTriageAnswers.empty.copy(
                individualUserType = Some(IndividualUserType.Self)
              )
            ),
            Right(sample[IndividualName])
          )
          val updatedJourney     = journey.copy(
            newReturnTriageAnswers = Right(
              IncompleteSingleDisposalTriageAnswers.empty.copy(
                individualUserType = Some(IndividualUserType.Self),
                hasConfirmedSingleDisposal = true
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction(formData))
        }
      }

      "handle valid data" when {

        "the user is starting a new draft return and" when {

          "the user is on a single disposal journey and hasn't answered the question yet and " +
            "they haven't completed the triage section" in {
              testSuccessfulUpdateStartingNewDraftReturn(
                performAction("numberOfProperties" -> "0"),
                Right(
                  IncompleteSingleDisposalTriageAnswers.empty
                ),
                Left(sample[TrustName])
              )(
                Right(
                  IncompleteSingleDisposalTriageAnswers.empty.copy(
                    hasConfirmedSingleDisposal = true
                  )
                ),
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            }

          "the user is on a single disposal journey and they haven't completed the triage section" in {
            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("numberOfProperties" -> "1"),
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(
                  individualUserType = Some(IndividualUserType.Self),
                  hasConfirmedSingleDisposal = true
                )
              ),
              Right(sample[IndividualName])
            )(
              Left(
                IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                  individualUserType = Some(IndividualUserType.Self)
                )
              ),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a single disposal journey and they have completed the triage section" in {
            val answers = sample[CompleteSingleDisposalTriageAnswers]

            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("numberOfProperties" -> "1"),
              Right(answers),
              Right(sample[IndividualName])
            )(
              Left(
                IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                  individualUserType = answers.individualUserType
                )
              ),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a multiple disposals journey and they haven't completed the triage section" in {
            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("numberOfProperties" -> "0"),
              Left(
                IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                  individualUserType = None
                )
              ),
              Left(sample[TrustName])
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(
                  individualUserType = None,
                  hasConfirmedSingleDisposal = true
                )
              ),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a multiple disposals journey and they have completed the triage section" in {
            val answers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
              individualUserType = Some(IndividualUserType.Self)
            )

            testSuccessfulUpdateStartingNewDraftReturn(
              performAction("numberOfProperties" -> "0"),
              Left(answers),
              Right(sample[IndividualName])
            )(
              Right(
                IncompleteSingleDisposalTriageAnswers.empty.copy(
                  individualUserType = answers.individualUserType,
                  hasConfirmedSingleDisposal = true
                )
              ),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }
        }

        "the user is filling in a return and" when {

          "they are on a multiple disposals journey and change the answer to one property" in {
            forAll { c: CompleteMultipleDisposalsTriageAnswers =>
              val answers = c.copy(
                individualUserType = Some(IndividualUserType.Self)
              )

              testSuccessfulUpdateFillingOutReturn(
                performAction("numberOfProperties" -> "0"),
                answers
              )(
                d =>
                  DraftSingleDisposalReturn.newDraftReturn(
                    d.id,
                    IncompleteSingleDisposalTriageAnswers.empty.copy(
                      individualUserType = Some(IndividualUserType.Self),
                      hasConfirmedSingleDisposal = true
                    ),
                    d.representeeAnswers
                  ),
                routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            }
          }

        }

      }

      "not do any updates" when {

        "the user has submitted the same answer they have previously entered on the single disposal journey" in {
          val answers =
            IncompleteSingleDisposalTriageAnswers.empty.copy(
              individualUserType = None,
              hasConfirmedSingleDisposal = true
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(answers),
                Left(sample[TrustName])
              )._1
            )
          }

          checkIsRedirect(
            performAction("numberOfProperties" -> "0"),
            routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        }

        "the user has submitted the same answer they have previously entered on the multiple disposals journey" in {
          val answers =
            IncompleteMultipleDisposalsTriageAnswers.empty.copy(
              individualUserType = Some(IndividualUserType.Self)
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(answers),
                Right(sample[IndividualName])
              )._1
            )
          }

          checkIsRedirect(
            performAction("numberOfProperties" -> "1"),
            routes.MultipleDisposalsTriageController.checkYourAnswers()
          )

        }

      }

    }

    "handling requests to display the further return number of properties page" must {

      def performAction(): Future[Result] =
        controller.howManyPropertiesFurtherReturn()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.howManyProperties(),
        mockUUIDGenerator
      )

      "redirect to the return number of properties page" when {

        "the user is not on a further return journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(IncompleteSingleDisposalTriageAnswers.empty),
                Left(sample[TrustName]),
                previousSentReturns = None
              )._1
            )
          }

          checkIsRedirect(performAction(), routes.CommonTriageQuestionsController.howManyProperties())
        }

      }

      "display the page" when {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithFillingOutReturn(
              IncompleteSingleDisposalTriageAnswers.empty,
              Left(sample[TrustName]),
              UserType.Organisation,
              amendReturnData = Some(sample[AmendReturnData])
            )._1
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("numberOfProperties.title"),
          { doc =>
            doc.select("#back").attr("href") shouldBe ""
            doc
              .select("#content > article > form")
              .attr("action")                shouldBe routes.CommonTriageQuestionsController
              .howManyPropertiesFurtherReturnSubmit()
              .url

          }
        )
      }

    }

    "handling submitted answers to the further return number of properties page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.howManyPropertiesFurtherReturnSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*)
        )

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        isValidJourney
      )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.howManyPropertiesSubmit(),
        mockUUIDGenerator
      )

      "redirect to the number of properties page" when {

        "the user is not on a further return journey" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(IncompleteSingleDisposalTriageAnswers.empty),
                Left(sample[TrustName]),
                previousSentReturns = None
              )._1
            )
          }

          checkIsRedirect(
            performAction("numberOfProperties" -> "0"),
            routes.CommonTriageQuestionsController.howManyProperties()
          )
        }

      }

      "show a form error" when {

        def test(
          formData: (String, String)*
        )(expectedErrorKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                    individualUserType = Some(IndividualUserType.Capacitor)
                  )
                ),
                Right(sample[IndividualName]),
                representeeAnswers = Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = false))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(formData: _*),
            messageFromMessageKey("numberOfProperties.title"),
            { doc =>
              doc
                .select("#error-summary-display > ul > li > a")
                .text() shouldBe messageFromMessageKey(expectedErrorKey)

              doc
                .select("#content > article > form")
                .attr("action") shouldBe routes.CommonTriageQuestionsController
                .howManyPropertiesFurtherReturnSubmit()
                .url
            },
            BAD_REQUEST
          )
        }

        "nothing has been submitted" in {
          test()("numberOfProperties.error.required")
        }

        "the option submitted has not been recognised" in {
          test("numberOfProperties" -> "2")("numberOfProperties.error.invalid")

        }

      }

      "handle valid data" when {

        "the user is on an amend return journey" in {
          forAll { c: SingleDisposalTriageAnswers =>
            val answers = c.fold(
              _.copy(individualUserType = Some(IndividualUserType.Self)),
              _.copy(
                individualUserType = Some(IndividualUserType.Self)
              )
            )

            testSuccessfulUpdateFillingOutReturn(
              performAction("numberOfProperties" -> "1"),
              answers,
              amendReturnData = Some(sample[AmendReturnData])
            )(
              (fillingOutReturn, draftReturn) =>
                fillingOutReturn
                  .copy(
                    draftReturn = DraftMultipleDisposalsReturn.newDraftReturn(
                      draftReturn.id,
                      IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                        individualUserType = Some(IndividualUserType.Self)
                      ),
                      draftReturn.representeeAnswers
                    )
                  )
                  .withForceDisplayGainOrLossAfterReliefsForAmends,
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }
        }

      }

    }

    "handling requests to display the disposal date too early page" must {

      def performAction(): Future[Result] =
        controller.disposalDateTooEarly()(FakeRequest())

      val singleDisposalRequiredPreviousAnswers =
        IncompleteSingleDisposalTriageAnswers.empty.copy(
          individualUserType = Some(IndividualUserType.Self),
          hasConfirmedSingleDisposal = true,
          disposalMethod = Some(DisposalMethod.Sold),
          wasAUKResident = Some(true),
          countryOfResidence = None,
          assetType = Some(AssetType.Residential)
        )

      val multipleDisposalsRequiredPreviousAnswers =
        IncompleteMultipleDisposalsTriageAnswers.empty.copy(
          individualUserType = Some(IndividualUserType.Self),
          numberOfProperties = Some(2),
          wasAUKResident = Some(true),
          countryOfResidence = None,
          assetTypes = Some(List(AssetType.Residential)),
          wereAllPropertiesResidential = Some(true),
          taxYearAfter6April2020 = Some(false)
        )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.disposalDateTooEarly(),
        mockUUIDGenerator
      )

      "redirect to the relevant check you answers endpoint" when {

        def test(sessionData: SessionData, expectedRedirect: Call): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkIsRedirect(performAction(), expectedRedirect)
        }

        "the section is incomplete and the was a uk resident question has not been answered yet and" when {

          "the user is starting a new draft return and they are on a single disposal journey" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                Right(
                  singleDisposalRequiredPreviousAnswers
                    .copy(wasAUKResident = None)
                ),
                Right(sample[IndividualName])
              )._1,
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is filling in a draft return and they are on a single disposal journey" in {
            test(
              sessionDataWithFillingOutReturn(
                singleDisposalRequiredPreviousAnswers.copy(wasAUKResident = None),
                Right(sample[IndividualName])
              )._1,
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is starting a new draft return and they are on a multiple disposals journey" in {
            test(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  multipleDisposalsRequiredPreviousAnswers
                    .copy(wasAUKResident = None)
                ),
                Right(sample[IndividualName])
              )._1,
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

      }

      "display the page" when {

        "the user was a uk resident and" when {

          def test(result: Future[Result], expectedBackLink: Call): Unit =
            checkPageIsDisplayed(
              result,
              messageFromMessageKey("disposalDateTooEarly.uk.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe expectedBackLink.url
                doc
                  .select("#content > article > p:nth-child(3)")
                  .text()                        shouldBe messageFromMessageKey(
                  "disposalDateTooEarly.uk.p1"
                )
                doc
                  .select("#content > article > p:nth-child(4)")
                  .html()                        shouldBe messageFromMessageKey(
                  "disposalDateTooEarly.uk.p2",
                  viewConfig.cgtLegacyUrl
                )
              }
            )

          "they are on a single disposal journey" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(singleDisposalRequiredPreviousAnswers),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(
              performAction(),
              routes.SingleDisposalsTriageController.whenWasDisposalDate()
            )
          }

          "they are on a mutliple disposals journey" in {

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(multipleDisposalsRequiredPreviousAnswers),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(
              performAction(),
              routes.MultipleDisposalsTriageController
                .whenWereContractsExchanged()
            )
          }
        }

        "the user was not a uk resident and" when {

          def test(result: Future[Result], expectedBackLink: Call): Unit =
            checkPageIsDisplayed(
              result,
              messageFromMessageKey("disposalDateTooEarly.non-uk.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe expectedBackLink.url
                doc
                  .select("#content > article > p:nth-child(3)")
                  .text()                        shouldBe messageFromMessageKey(
                  "disposalDateTooEarly.non-uk.p1"
                )
                doc
                  .select("#content > article > p:nth-child(4)")
                  .text()                        shouldBe messageFromMessageKey(
                  "disposalDateTooEarly.non-uk.p2"
                )
                doc
                  .select("#content > article > p:nth-child(6)")
                  .html()                        shouldBe messageFromMessageKey(
                  "disposalDateTooEarly.non-uk.p3",
                  viewConfig.nrcgtReturn
                )
              }
            )

          "they are on a single disposal journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(
                    singleDisposalRequiredPreviousAnswers.copy(
                      wasAUKResident = Some(false),
                      countryOfResidence = Some(sample[Country])
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(
              performAction(),
              routes.SingleDisposalsTriageController.whenWasDisposalDate()
            )
          }

          "they are on a multiple disposals journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(
                    multipleDisposalsRequiredPreviousAnswers.copy(
                      wasAUKResident = Some(false),
                      countryOfResidence = Some(sample[Country])
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(
              performAction(),
              routes.MultipleDisposalsTriageController
                .whenWereContractsExchanged()
            )
          }

        }
      }

    }

    "handling requests to display the uk residents can only dispose residential properties page" must {

      def performAction(): Future[Result] =
        controller.ukResidentCanOnlyDisposeResidential()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.ukResidentCanOnlyDisposeResidential(),
        mockUUIDGenerator
      )

      "redirect to the check your answers page" when {

        "the user was not a uk resident and" when {

          "the user is on a single disposal journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(
                    IncompleteSingleDisposalTriageAnswers.empty.copy(
                      individualUserType = Some(IndividualUserType.Self),
                      hasConfirmedSingleDisposal = true,
                      disposalMethod = Some(DisposalMethod.Sold),
                      wasAUKResident = Some(false),
                      countryOfResidence = Some(sample[Country]),
                      assetType = Some(AssetType.NonResidential)
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkIsRedirect(
              performAction(),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a multiple disposals journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(
                    IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                      individualUserType = Some(IndividualUserType.Self),
                      numberOfProperties = Some(2),
                      wasAUKResident = Some(false),
                      countryOfResidence = Some(sample[Country]),
                      assetTypes = Some(List(AssetType.NonResidential))
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkIsRedirect(
              performAction(),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }
        }

        "the user was a uk resident and they disposed of a residential and" when {

          "the user is on a single disposal journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(
                    IncompleteSingleDisposalTriageAnswers.empty.copy(
                      individualUserType = Some(IndividualUserType.Self),
                      hasConfirmedSingleDisposal = true,
                      disposalMethod = Some(DisposalMethod.Sold),
                      wasAUKResident = Some(true),
                      assetType = Some(AssetType.Residential)
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkIsRedirect(
              performAction(),
              routes.SingleDisposalsTriageController.checkYourAnswers()
            )
          }

          "the user is on a multiple disposals journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(
                    IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                      individualUserType = Some(IndividualUserType.Self),
                      numberOfProperties = Some(2),
                      wasAUKResident = Some(true),
                      assetTypes = Some(List(AssetType.Residential))
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            checkIsRedirect(
              performAction(),
              routes.MultipleDisposalsTriageController.checkYourAnswers()
            )
          }

        }

      }

      "display the page" when {

        def singleDisposalTriageAnswers(individualUserType: Option[IndividualUserType]) =
          IncompleteSingleDisposalTriageAnswers.empty.copy(
            individualUserType = individualUserType,
            hasConfirmedSingleDisposal = true,
            disposalMethod = Some(DisposalMethod.Sold),
            wasAUKResident = Some(true),
            assetType = Some(AssetType.NonResidential)
          )

        "the user was a uk resident and they disposed of a non-residential property and" when {

          def test(expectedBackLink: Call, expectedP1Key: String): Unit =
            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("ukResidentCanOnlyReportResidential.title"),
              { doc =>
                doc.select("#back").attr("href")                         shouldBe expectedBackLink.url
                doc.select("#content > article > p:nth-child(3)").text() shouldBe messageFromMessageKey(expectedP1Key)
              }
            )

          "the user is on a single disposal journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(singleDisposalTriageAnswers(Some(Self))),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(
              routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty(),
              "ukResidentCanOnlyReportResidential.p1"
            )
          }

          "the user is an agent" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(singleDisposalTriageAnswers(Some(Self))),
                  Right(sample[IndividualName]),
                  UserType.Agent
                )._1
              )
            }

            test(
              routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty(),
              "ukResidentCanOnlyReportResidential.agent.p1"
            )
          }

          "the user is a trust" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(singleDisposalTriageAnswers(None)),
                  Left(sample[TrustName]),
                  UserType.Organisation
                )._1
              )
            }

            test(
              routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty(),
              "ukResidentCanOnlyReportResidential.trust.p1"
            )
          }

          "the user is a capacitor" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(singleDisposalTriageAnswers(Some(Capacitor))),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(
              routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty(),
              "ukResidentCanOnlyReportResidential.capacitor.p1"
            )
          }

          "the user is a personal representative" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(singleDisposalTriageAnswers(Some(PersonalRepresentative))),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(
              routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty(),
              "ukResidentCanOnlyReportResidential.personalRep.p1"
            )
          }

          "the user is a personal representative in a period of admin" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(singleDisposalTriageAnswers(Some(PersonalRepresentativeInPeriodOfAdmin))),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(
              routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty(),
              "ukResidentCanOnlyReportResidential.personalRepInPeriodOfAdmin.p1"
            )
          }

          "the user is an agent of a personal representative in a period of admin" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Right(singleDisposalTriageAnswers(Some(PersonalRepresentativeInPeriodOfAdmin))),
                  Right(sample[IndividualName]),
                  UserType.Agent
                )._1
              )
            }

            test(
              routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty(),
              "ukResidentCanOnlyReportResidential.personalRepInPeriodOfAdmin.agent.p1"
            )
          }

          "the user is on a multiple disposals journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithStartingNewDraftReturn(
                  Left(
                    IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                      individualUserType = Some(IndividualUserType.Self),
                      numberOfProperties = Some(3),
                      wasAUKResident = Some(true),
                      assetTypes = Some(List(AssetType.NonResidential)),
                      wereAllPropertiesResidential = Some(false)
                    )
                  ),
                  Right(sample[IndividualName])
                )._1
              )
            }

            test(
              routes.MultipleDisposalsTriageController.wereAllPropertiesResidential(),
              "ukResidentCanOnlyReportResidential.p1"
            )
          }

        }

        "the user is on an amend returns journey where they originally said they were a non-uk resident and" when {

          def test(expectedBackLink: Call, expectedWarningKey: String): Unit =
            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("cannotAmendResidentialStatusForAssetType.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe expectedBackLink.url
                doc.select("#warning").text()    shouldBe messageFromMessageKey(expectedWarningKey)
              }
            )

          def amendReturnData(completeReturn: CompleteReturn) =
            sample[AmendReturnData].copy(
              originalReturn = sample[CompleteReturnWithSummary].copy(
                completeReturn = completeReturn
              )
            )

          "the user is an individual" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturn(
                  singleDisposalTriageAnswers(Some(Self)),
                  Right(sample[IndividualName]),
                  amendReturnData = Some(
                    amendReturnData(
                      sample[CompleteSingleDisposalReturn].copy(
                        triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                          assetType = AssetType.NonResidential
                        )
                      )
                    )
                  )
                )._1
              )
            }

            test(
              routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty(),
              "cannotAmendResidentialStatusForAssetType.warning"
            )
          }

          "the user is an agent" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturn(
                  singleDisposalTriageAnswers(Some(Self)),
                  Right(sample[IndividualName]),
                  userType = UserType.Agent,
                  amendReturnData = Some(
                    amendReturnData(
                      sample[CompleteMultipleDisposalsReturn].copy(
                        triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                          assetTypes = List(AssetType.NonResidential)
                        )
                      )
                    )
                  )
                )._1
              )
            }

            test(
              routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty(),
              "cannotAmendResidentialStatusForAssetType.agent.warning"
            )
          }

          "the user is a trust" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturn(
                  singleDisposalTriageAnswers(None),
                  Left(sample[TrustName]),
                  userType = UserType.Organisation,
                  amendReturnData = Some(
                    amendReturnData(
                      sample[CompleteSingleMixedUseDisposalReturn].copy(
                        triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                          assetType = AssetType.MixedUse
                        )
                      )
                    )
                  )
                )._1
              )
            }

            test(
              routes.SingleDisposalsTriageController.didYouDisposeOfAResidentialProperty(),
              "cannotAmendResidentialStatusForAssetType.trust.warning"
            )
          }

        }

      }

    }

    "handling requests to display the previous return has same completion date exit page" must {

      def previousReturnData(id: String, date: LocalDate) =
        sample[PreviousReturnData]
          .copy(summaries = List(sample[ReturnSummary].copy(submissionId = id, completionDate = date)))

      def performAction(): Future[Result] =
        controller.previousReturnExistsWithSameCompletionDate()(FakeRequest())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.previousReturnExistsWithSameCompletionDate(),
        mockUUIDGenerator
      )

      "display the page" when {

        "the user is an individual" in {
          val date = sample[CompletionDate]
          val id   = sample[String]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  sample[IncompleteMultipleDisposalsTriageAnswers].copy(
                    assetTypes = Some(List(AssetType.Residential)),
                    completionDate = Some(date)
                  )
                ),
                Right(sample[IndividualName]),
                UserType.Individual,
                previousSentReturns = Some(previousReturnData(id, date.value))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("previousReturnExistsWithSameCompletionDate.title"),
            { doc =>
              doc.select("#back").attr("href")                         shouldBe routes.MultipleDisposalsTriageController.completionDate().url
              doc.select("#content > article > p:nth-child(3)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.p1",
                TimeUtils.govDisplayFormat(date.value)
              )
              doc.select("#content > article > p:nth-child(4)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.p2"
              )
              doc.select(s"#viewSentReturn-$id").attr("href")          shouldBe homePageRoutes.HomePageController
                .viewSentReturn(id)
                .url
            }
          )
        }

        "the user is a trust" in {
          val date = sample[CompletionDate]
          val id   = sample[String]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(
                  sample[IncompleteSingleDisposalTriageAnswers].copy(
                    assetType = Some(AssetType.Residential),
                    completionDate = Some(date)
                  )
                ),
                Left(sample[TrustName]),
                UserType.Organisation,
                previousSentReturns = Some(previousReturnData(id, date.value))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("previousReturnExistsWithSameCompletionDate.title"),
            { doc =>
              doc.select("#back").attr("href")                         shouldBe routes.SingleDisposalsTriageController
                .whenWasCompletionDate()
                .url
              doc.select("#content > article > p:nth-child(3)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.p1",
                TimeUtils.govDisplayFormat(date.value)
              )
              doc.select("#content > article > p:nth-child(4)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.p2"
              )
              doc.select(s"#viewSentReturn-$id").attr("href")          shouldBe homePageRoutes.HomePageController
                .viewSentReturn(id)
                .url
            }
          )
        }

        "the user is an agent" in {
          val date = sample[CompletionDate]
          val id   = sample[String]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(
                  sample[IncompleteSingleDisposalTriageAnswers].copy(
                    assetType = Some(AssetType.Residential),
                    completionDate = Some(date)
                  )
                ),
                Left(sample[TrustName]),
                UserType.Agent,
                previousSentReturns = Some(previousReturnData(id, date.value))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("previousReturnExistsWithSameCompletionDate.title"),
            { doc =>
              doc.select("#back").attr("href")                         shouldBe routes.SingleDisposalsTriageController
                .whenWasCompletionDate()
                .url
              doc.select("#content > article > p:nth-child(3)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.p1",
                TimeUtils.govDisplayFormat(date.value)
              )
              doc.select("#content > article > p:nth-child(4)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.p2"
              )
              doc.select(s"#viewSentReturn-$id").attr("href")          shouldBe homePageRoutes.HomePageController
                .viewSentReturn(id)
                .url
            }
          )
        }

        "the user is an individual amending" in {
          val completionDate = sample[CompletionDate]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                sample[CompleteSingleDisposalTriageAnswers].copy(
                  disposalDate = sample[DisposalDate],
                  completionDate = completionDate,
                  assetType = Residential
                ),
                Right(sample[IndividualName]),
                UserType.Individual,
                amendReturnData = Some(sample[AmendReturnData]),
                previousReturns = Some(previousReturnData("id", completionDate.value))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "previousReturnExistsWithSameCompletionDate.amend.title",
              TimeUtils.govDisplayFormat(completionDate.value)
            ),
            { doc =>
              doc.select("#back").attr("href")                         shouldBe routes.SingleDisposalsTriageController
                .whenWasCompletionDate()
                .url
              doc.select("#content > article > p:nth-child(3)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.amend.p1",
                viewConfig.contactHmrc
              )
            }
          )
        }

        "the user is a trust amending" in {
          val completionDate = sample[CompletionDate]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                sample[CompleteSingleDisposalTriageAnswers].copy(
                  disposalDate = sample[DisposalDate],
                  completionDate = completionDate,
                  assetType = Residential
                ),
                Left(sample[TrustName]),
                UserType.Organisation,
                amendReturnData = Some(sample[AmendReturnData])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "previousReturnExistsWithSameCompletionDate.amend.title",
              TimeUtils.govDisplayFormat(completionDate.value)
            ),
            { doc =>
              doc.select("#back").attr("href")                         shouldBe routes.SingleDisposalsTriageController
                .whenWasCompletionDate()
                .url
              doc.select("#content > article > p:nth-child(3)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.amend.trust.p1",
                viewConfig.contactHmrc
              )
            }
          )
        }

        "the user is an agent amending" in {
          val completionDate = sample[CompletionDate]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                sample[CompleteSingleDisposalTriageAnswers].copy(
                  disposalDate = sample[DisposalDate],
                  completionDate = completionDate,
                  assetType = Residential
                ),
                amendReturnData = Some(sample[AmendReturnData])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "previousReturnExistsWithSameCompletionDate.amend.title",
              TimeUtils.govDisplayFormat(completionDate.value)
            ),
            { doc =>
              doc.select("#back").attr("href")                         shouldBe routes.SingleDisposalsTriageController
                .whenWasCompletionDate()
                .url
              doc.select("#content > article > p:nth-child(3)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.amend.p1",
                viewConfig.contactHmrc
              )
            }
          )
        }

        "the user is on a single indirect disposal journey" in {
          val completionDate = sample[CompletionDate]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  sample[IncompleteMultipleDisposalsTriageAnswers].copy(
                    assetTypes = Some(List(AssetType.IndirectDisposal)),
                    completionDate = Some(completionDate)
                  )
                ),
                Right(sample[IndividualName]),
                UserType.Individual,
                previousSentReturns = Some(previousReturnData("id", completionDate.value))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("previousReturnExistsWithSameCompletionDate.title"),
            _.select("#back").attr("href") shouldBe routes.MultipleDisposalsTriageController.disposalDateOfShares().url
          )
        }

        "the user is on a multiple indirect disposals journey" in {
          val completionDate = sample[CompletionDate]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(
                  sample[IncompleteSingleDisposalTriageAnswers].copy(
                    assetType = Some(AssetType.IndirectDisposal),
                    completionDate = Some(completionDate)
                  )
                ),
                Right(sample[IndividualName]),
                UserType.Individual,
                previousSentReturns = Some(previousReturnData("id", completionDate.value))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("previousReturnExistsWithSameCompletionDate.title"),
            _.select("#back").attr("href") shouldBe routes.SingleDisposalsTriageController.disposalDateOfShares().url
          )
        }

      }

    }

    "handling requests to display the amend return disposaldate different taxyear page" must {

      def performAction(): Future[Result] =
        controller.amendReturnDisposalDateDifferentTaxYear()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        performAction,
        isValidJourney
      )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.ukResidentCanOnlyDisposeResidential(),
        mockUUIDGenerator
      )

      "display the page" when {

        "the user enters disposaldate in different taxyear and" when {

          def test(expectedBackLink: Call): Unit =
            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("disposalDateInDifferentTaxYear.title"),
              doc => doc.select("#back").attr("href") shouldBe expectedBackLink.url
            )

          "the user is on a single disposal journey" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturn(
                  sample[CompleteSingleDisposalTriageAnswers].copy(
                    disposalDate = sample[DisposalDate]
                  ),
                  amendReturnData = Some(sample[AmendReturnData])
                )._1
              )

              test(
                routes.SingleDisposalsTriageController.whenWasDisposalDate()
              )
            }
          }

          "the user is on a multiple disposals journey" in {
            val today   = LocalDate.now(Clock.systemUTC())
            val taxYear = sample[TaxYear].copy(
              startDateInclusive = LocalDate.of(today.getYear, 4, 6),
              endDateExclusive = LocalDate.of(today.getYear + 1, 4, 6)
            )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturnForMultpleDisposals(
                  sample[CompleteMultipleDisposalsTriageAnswers].copy(
                    taxYear = taxYear
                  ),
                  isAmend = true
                )._1
              )

              test(
                routes.MultipleDisposalsTriageController.whenWereContractsExchanged()
              )
            }
          }

          "the user is on a single indirect disposal journey" in {
            val disposalDate                                = sample[DisposalDate].copy(
              value = LocalDate.of(2020, 4, 10)
            )
            val completeTriageQuestions                     =
              CompleteSingleDisposalTriageAnswers(
                Some(IndividualUserType.Self),
                DisposalMethod.Sold,
                Country.uk,
                assetType = AssetType.Residential,
                disposalDate,
                sample[CompletionDate]
              )
            val completeTriageQuestionsWithIndirectDisposal = completeTriageQuestions.copy(
              assetType = AssetType.IndirectDisposal,
              countryOfResidence = Country("TR"),
              disposalDate = disposalDate.copy(
                value = LocalDate.of(2019, 4, 10)
              )
            )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturnForSingleIndirectDisposal(
                  completeTriageQuestionsWithIndirectDisposal,
                  isAmend = true
                )._1
              )

              test(
                routes.SingleDisposalsTriageController.disposalDateOfShares()
              )
            }
          }

          "the user is on a multiple indirect disposals journey" in {
            val today   = LocalDate.now(Clock.systemUTC())
            val taxYear = sample[TaxYear].copy(
              startDateInclusive = LocalDate.of(today.getYear, 4, 6),
              endDateExclusive = LocalDate.of(today.getYear + 1, 4, 6)
            )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturnForMultpleIndirectDisposals(
                  sample[CompleteMultipleDisposalsTriageAnswers].copy(
                    countryOfResidence = Country("NZ"),
                    assetTypes = List(AssetType.IndirectDisposal),
                    taxYear = taxYear
                  ),
                  isAmend = true
                )._1
              )

              test(
                routes.MultipleDisposalsTriageController.disposalDateOfShares()
              )
            }
          }

          "the user is on a single mideduse disposal journey" in {
            val disposalDate                                = sample[DisposalDate].copy(
              value = LocalDate.of(2020, 4, 10)
            )
            val completeTriageQuestions                     =
              CompleteSingleDisposalTriageAnswers(
                Some(IndividualUserType.Self),
                DisposalMethod.Sold,
                Country.uk,
                assetType = AssetType.Residential,
                disposalDate,
                sample[CompletionDate]
              )
            val completeTriageQuestionsWithIndirectDisposal = completeTriageQuestions.copy(
              assetType = AssetType.MixedUse,
              countryOfResidence = Country("TR"),
              disposalDate = disposalDate.copy(
                value = LocalDate.of(2019, 4, 10)
              )
            )
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionDataWithFillingOutReturnForSingleMixedUseDisposal(
                  completeTriageQuestionsWithIndirectDisposal,
                  isAmend = true
                )._1
              )

              test(
                routes.SingleDisposalsTriageController.whenWasDisposalDate()
              )
            }
          }

        }

      }

    }

    "handling requests to display the amend 'who are you representing' exit page" must {

      def performAction(): Future[Result] =
        controller.amendWhoAreYouSubmittingFor()(FakeRequest())

      "display the page" in {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionDataWithFillingOutReturn(
              sample[CompleteSingleDisposalTriageAnswers].copy(
                disposalDate = sample[DisposalDate]
              ),
              amendReturnData = Some(sample[AmendReturnData])
            )._1
          )
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("who-are-you-reporting-for-exit-page.title"),
          doc =>
            doc.select("#back").attr("href") shouldBe routes.CommonTriageQuestionsController
              .whoIsIndividualRepresenting()
              .url
        )
      }
    }

  }

  def testSuccessfulUpdateStartingNewDraftReturn(
    performAction: => Future[Result],
    answers: Either[
      MultipleDisposalsTriageAnswers,
      SingleDisposalTriageAnswers
    ],
    name: Either[TrustName, IndividualName]
  )(
    updatedAnswers: Either[
      MultipleDisposalsTriageAnswers,
      SingleDisposalTriageAnswers
    ],
    expectedRedirect: Call,
    updateRepresenteeAnswers: Option[RepresenteeAnswers] => Option[
      RepresenteeAnswers
    ] = identity
  ): Unit = {
    val (session, journey) =
      sessionDataWithStartingNewDraftReturn(answers, name)
    val updatedJourney     = journey.copy(
      newReturnTriageAnswers = updatedAnswers,
      representeeAnswers = updateRepresenteeAnswers(journey.representeeAnswers)
    )

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(
        Right(())
      )
    }

    checkIsRedirect(performAction, expectedRedirect)
  }

  def testSuccessfulUpdateFillingOutReturn(
    performAction: => Future[Result],
    answers: SingleDisposalTriageAnswers,
    amendReturnData: Option[AmendReturnData]
  )(
    updateJourney: (FillingOutReturn, DraftSingleDisposalReturn) => FillingOutReturn,
    expectedRedirect: Call
  ): Unit = {
    val (session, journey, draftReturn) = sessionDataWithFillingOutReturn(
      answers,
      amendReturnData = amendReturnData
    )
    val updatedJourney                  =
      updateJourney(journey, draftReturn)

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreDraftReturn(updatedJourney)(Right(()))
      mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(
        Right(())
      )
    }

    checkIsRedirect(performAction, expectedRedirect)
  }

  def testRedirectedWithoutUpdateAmendFillingOutReturn(
    performAction: => Future[Result],
    answers: SingleDisposalTriageAnswers
  )(
    expectedRedirect: Call
  ): Unit = {
    val session = sessionDataWithFillingOutReturn(
      answers,
      amendReturnData = Some(sample[AmendReturnData])
    )._1

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
    }

    checkIsRedirect(performAction, expectedRedirect)
  }

  def testSuccessfulUpdateFillingOutReturn(
    performAction: => Future[Result],
    answers: MultipleDisposalsTriageAnswers
  )(
    updatedDraftReturn: DraftMultipleDisposalsReturn => DraftReturn,
    expectedRedirect: Call
  ): Unit = {
    val (session, journey, draftReturn) = sessionDataWithFillingOutReturnForMultpleDisposals(
      answers
    )
    val updatedJourney                  =
      journey.copy(draftReturn = updatedDraftReturn(draftReturn))

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
      mockStoreDraftReturn(updatedJourney)(Right(()))
      mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(
        Right(())
      )
    }

    checkIsRedirect(performAction, expectedRedirect)
  }

}

class CommonTriageQuestionsControllerDisabledNewCompletionDatePageFlatSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with ReturnsServiceSupport
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  val mockUUIDGenerator = mock[UUIDGenerator]

  override lazy val additionalConfig = Configuration(
    "amend-returns.enabled" -> false
  )

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    )

  lazy val controller = instanceOf[CommonTriageQuestionsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit val messages: Messages = MessagesImpl(lang, messagesApi)

  def isValidJourney(journeyStatus: JourneyStatus): Boolean =
    journeyStatus match {
      case _: StartingNewDraftReturn | _: FillingOutReturn | _: StartingToAmendReturn => true
      case _                                                                          => false
    }

  def setNameForUserType(
    userType: UserType
  ): Either[TrustName, IndividualName] =
    userType match {
      case UserType.Organisation => Left(sample[TrustName])
      case _                     => Right(sample[IndividualName])
    }

  def setAgentReferenceNumber(
    userType: UserType
  ): Option[AgentReferenceNumber] =
    userType match {
      case UserType.Agent => Some(sample[AgentReferenceNumber])
      case _              => None
    }

  def userMessageKey(userType: UserType): String =
    userType match {
      case UserType.Individual   => ""
      case UserType.Organisation => ".trust"
      case UserType.Agent        => ".agent"
      case other                 => sys.error(s"User type '$other' not handled")
    }

  def sessionDataWithStartingNewDraftReturn(
    triageAnswers: Either[
      MultipleDisposalsTriageAnswers,
      SingleDisposalTriageAnswers
    ],
    name: Either[TrustName, IndividualName],
    userType: UserType = UserType.Individual,
    representeeAnswers: Option[IncompleteRepresenteeAnswers] = None,
    previousSentReturns: Option[PreviousReturnData] = None
  ): (SessionData, StartingNewDraftReturn) = {
    val startingNewDraftReturn =
      sample[StartingNewDraftReturn].copy(
        subscribedDetails = sample[SubscribedDetails].copy(name = name),
        newReturnTriageAnswers = triageAnswers,
        agentReferenceNumber = setAgentReferenceNumber(userType),
        representeeAnswers = representeeAnswers,
        previousSentReturns = previousSentReturns
      )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(startingNewDraftReturn),
      userType = Some(userType)
    )

    sessionData -> startingNewDraftReturn
  }

  def sessionDataWithFillingOutReturn(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    userType: UserType = UserType.Individual,
    previousReturns: Option[PreviousReturnData] = None,
    amendReturnData: Option[AmendReturnData] = None
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
    val draftReturn      = sample[DraftSingleDisposalReturn].copy(
      triageAnswers = singleDisposalTriageAnswers,
      gainOrLossAfterReliefs = None,
      exemptionAndLossesAnswers = None,
      reliefDetailsAnswers =
        if (singleDisposalTriageAnswers.isPeriodOfAdmin())
          Some(IncompleteReliefDetailsAnswers.empty)
        else None
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      previousSentReturns = previousReturns,
      amendReturnData = amendReturnData
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn),
      userType = Some(userType)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  def sessionDataWithFillingOutReturnForSingleIndirectDisposal(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    userType: UserType = UserType.Individual,
    previousReturns: Option[PreviousReturnData] = None,
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) = {
    val draftReturn      = sample[DraftSingleIndirectDisposalReturn].copy(
      triageAnswers = singleDisposalTriageAnswers,
      gainOrLossAfterReliefs = None,
      exemptionAndLossesAnswers = None
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      previousSentReturns = previousReturns,
      amendReturnData =
        if (isAmend)
          Some(sample[AmendReturnData])
        else None
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn),
      userType = Some(userType)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  def sessionDataWithFillingOutReturnForSingleMixedUseDisposal(
    singleDisposalTriageAnswers: SingleDisposalTriageAnswers,
    name: Either[TrustName, IndividualName] = Right(sample[IndividualName]),
    userType: UserType = UserType.Individual,
    previousReturns: Option[PreviousReturnData] = None,
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftSingleMixedUseDisposalReturn) = {
    val draftReturn      = sample[DraftSingleMixedUseDisposalReturn].copy(
      triageAnswers = singleDisposalTriageAnswers,
      gainOrLossAfterReliefs = None,
      exemptionAndLossesAnswers = None
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      previousSentReturns = previousReturns,
      amendReturnData =
        if (isAmend)
          Some(sample[AmendReturnData])
        else None
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn),
      userType = Some(userType)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  def sessionDataWithFillingOutReturnForMultpleDisposals(
    multipleDisposalsTriageAnswers: MultipleDisposalsTriageAnswers,
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftMultipleDisposalsReturn) = {
    val draftReturn      = sample[DraftMultipleDisposalsReturn].copy(
      triageAnswers = multipleDisposalsTriageAnswers
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
      amendReturnData =
        if (isAmend)
          Some(sample[AmendReturnData])
        else None
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  def sessionDataWithFillingOutReturnForMultpleIndirectDisposals(
    multipleDisposalsTriageAnswers: MultipleDisposalsTriageAnswers,
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftMultipleIndirectDisposalsReturn) = {
    val draftReturn      = sample[DraftMultipleIndirectDisposalsReturn].copy(
      triageAnswers = multipleDisposalsTriageAnswers
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = Right(sample[IndividualName])),
      amendReturnData =
        if (isAmend)
          Some(sample[AmendReturnData])
        else None
    )

    val sessionData = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  "CommonTriageQuestionsControllerDisabledNewCompletionDatePageFlatSpec" when {

    "handling requests to display the previous return has same completion date exit page" must {

      def previousReturnData(id: String, date: LocalDate) =
        sample[PreviousReturnData]
          .copy(summaries = List(sample[ReturnSummary].copy(submissionId = id, completionDate = date)))

      def performAction(): Future[Result] =
        controller.previousReturnExistsWithSameCompletionDate()(FakeRequest())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.previousReturnExistsWithSameCompletionDate(),
        mockUUIDGenerator
      )

      "display the page" when {

        "the user is an individual" in {
          val date = sample[CompletionDate]
          val id   = sample[String]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  sample[IncompleteMultipleDisposalsTriageAnswers].copy(
                    assetTypes = Some(List(AssetType.Residential)),
                    completionDate = Some(date)
                  )
                ),
                Right(sample[IndividualName]),
                UserType.Individual,
                previousSentReturns = Some(previousReturnData(id, date.value))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("previousReturnExistsWithSameCompletionDate.amend.title"),
            { doc =>
              doc.select("#back").attr("href")                         shouldBe routes.MultipleDisposalsTriageController.completionDate().url
              doc.select("#content > article > p:nth-child(3)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.amend.p1",
                viewConfig.contactHmrc
              )
            }
          )
        }

        "the user is a trust" in {
          val date = sample[CompletionDate]
          val id   = sample[String]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(
                  sample[IncompleteSingleDisposalTriageAnswers].copy(
                    assetType = Some(AssetType.Residential),
                    completionDate = Some(date)
                  )
                ),
                Left(sample[TrustName]),
                UserType.Organisation,
                previousSentReturns = Some(previousReturnData(id, date.value))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("previousReturnExistsWithSameCompletionDate.amend.title"),
            { doc =>
              doc.select("#back").attr("href")                         shouldBe routes.SingleDisposalsTriageController
                .whenWasCompletionDate()
                .url
              doc.select("#content > article > p:nth-child(3)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.amend.p1",
                viewConfig.contactHmrc
              )
            }
          )
        }

        "the user is an agent" in {
          val date = sample[CompletionDate]
          val id   = sample[String]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(
                  sample[IncompleteSingleDisposalTriageAnswers].copy(
                    assetType = Some(AssetType.Residential),
                    completionDate = Some(date)
                  )
                ),
                Left(sample[TrustName]),
                UserType.Agent,
                previousSentReturns = Some(previousReturnData(id, date.value))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("previousReturnExistsWithSameCompletionDate.amend.title"),
            { doc =>
              doc.select("#back").attr("href")                         shouldBe routes.SingleDisposalsTriageController
                .whenWasCompletionDate()
                .url
              doc.select("#content > article > p:nth-child(3)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.amend.p1",
                viewConfig.contactHmrc
              )
            }
          )
        }

        "the user is an individual amending" in {
          val completionDate = sample[CompletionDate]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                sample[CompleteSingleDisposalTriageAnswers].copy(
                  disposalDate = sample[DisposalDate],
                  completionDate = completionDate,
                  assetType = Residential
                ),
                Right(sample[IndividualName]),
                UserType.Individual,
                amendReturnData = Some(sample[AmendReturnData]),
                previousReturns = Some(previousReturnData("id", completionDate.value))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "previousReturnExistsWithSameCompletionDate.amend.title",
              TimeUtils.govDisplayFormat(completionDate.value)
            ),
            { doc =>
              doc.select("#back").attr("href")                         shouldBe routes.SingleDisposalsTriageController
                .whenWasCompletionDate()
                .url
              doc.select("#content > article > p:nth-child(3)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.amend.p1",
                viewConfig.contactHmrc
              )
            }
          )
        }

        "the user is a trust amending" in {
          val completionDate = sample[CompletionDate]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                sample[CompleteSingleDisposalTriageAnswers].copy(
                  disposalDate = sample[DisposalDate],
                  completionDate = completionDate,
                  assetType = Residential
                ),
                Left(sample[TrustName]),
                UserType.Organisation,
                amendReturnData = Some(sample[AmendReturnData])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "previousReturnExistsWithSameCompletionDate.amend.title",
              TimeUtils.govDisplayFormat(completionDate.value)
            ),
            { doc =>
              doc.select("#back").attr("href")                         shouldBe routes.SingleDisposalsTriageController
                .whenWasCompletionDate()
                .url
              doc.select("#content > article > p:nth-child(3)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.amend.trust.p1",
                viewConfig.contactHmrc
              )
            }
          )
        }

        "the user is an agent amending" in {
          val completionDate = sample[CompletionDate]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithFillingOutReturn(
                sample[CompleteSingleDisposalTriageAnswers].copy(
                  disposalDate = sample[DisposalDate],
                  completionDate = completionDate,
                  assetType = Residential
                ),
                amendReturnData = Some(sample[AmendReturnData])
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "previousReturnExistsWithSameCompletionDate.amend.title",
              TimeUtils.govDisplayFormat(completionDate.value)
            ),
            { doc =>
              doc.select("#back").attr("href")                         shouldBe routes.SingleDisposalsTriageController
                .whenWasCompletionDate()
                .url
              doc.select("#content > article > p:nth-child(3)").html() shouldBe messageFromMessageKey(
                "previousReturnExistsWithSameCompletionDate.amend.p1",
                viewConfig.contactHmrc
              )
            }
          )
        }

        "the user is on a single indirect disposal journey" in {
          val completionDate = sample[CompletionDate]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Left(
                  sample[IncompleteMultipleDisposalsTriageAnswers].copy(
                    assetTypes = Some(List(AssetType.IndirectDisposal)),
                    completionDate = Some(completionDate)
                  )
                ),
                Right(sample[IndividualName]),
                UserType.Individual,
                previousSentReturns = Some(previousReturnData("id", completionDate.value))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("previousReturnExistsWithSameCompletionDate.amend.title"),
            _.select("#back").attr("href") shouldBe routes.MultipleDisposalsTriageController.disposalDateOfShares().url
          )
        }

        "the user is on a multiple indirect disposals journey" in {
          val completionDate = sample[CompletionDate]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionDataWithStartingNewDraftReturn(
                Right(
                  sample[IncompleteSingleDisposalTriageAnswers].copy(
                    assetType = Some(AssetType.IndirectDisposal),
                    completionDate = Some(completionDate)
                  )
                ),
                Right(sample[IndividualName]),
                UserType.Individual,
                previousSentReturns = Some(previousReturnData("id", completionDate.value))
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("previousReturnExistsWithSameCompletionDate.amend.title"),
            _.select("#back").attr("href") shouldBe routes.SingleDisposalsTriageController.disposalDateOfShares().url
          )
        }

      }

    }

  }

}
