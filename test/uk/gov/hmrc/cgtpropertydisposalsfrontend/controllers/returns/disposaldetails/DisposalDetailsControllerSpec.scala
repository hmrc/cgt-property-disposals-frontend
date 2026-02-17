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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails

import org.jsoup.nodes.Document
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DisposalDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DisposalMethodGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReliefDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.UserTypeGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class DisposalDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  import DisposalDetailsControllerSpec._

  val mockUUIDGenerator: UUIDGenerator = mock[UUIDGenerator]

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    )

  private lazy val controller = instanceOf[DisposalDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  private def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      () => performAction(),
      {
        case _: FillingOutReturn      => true
        case _: StartingToAmendReturn => true
        case _                        => false
      }
    )

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

  def fillingOutReturn(
    disposalMethod: DisposalMethod,
    userType: UserType,
    individualUserType: Option[IndividualUserType],
    isAmend: Boolean
  ): (FillingOutReturn, DraftSingleDisposalReturn) = {
    val answers            = sample[CompleteSingleDisposalTriageAnswers].copy(
      disposalMethod = disposalMethod,
      individualUserType = individualUserType
    )
    val reliefDetails      =
      if (individualUserType.contains(IndividualUserType.PersonalRepresentativeInPeriodOfAdmin)) {
        sample[CompleteReliefDetailsAnswers].copy(
          lettingsRelief = AmountInPence.zero
        )
      } else {
        sample[CompleteReliefDetailsAnswers]
      }
    val representeeAnswers = individualUserType.flatMap {
      case Self => None
      case _    => Some(sample[CompleteRepresenteeAnswers].copy(isFirstReturn = true))
    }
    val subscribedDetails  = sample[SubscribedDetails].copy(
      name = setNameForUserType(userType)
    )
    val draftReturn        = sample[DraftSingleDisposalReturn].copy(
      triageAnswers = answers,
      representeeAnswers = representeeAnswers,
      reliefDetailsAnswers = Some(reliefDetails)
    )

    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = subscribedDetails,
      agentReferenceNumber = setAgentReferenceNumber(userType),
      amendReturnData = if (isAmend) Some(sample[AmendReturnData]) else None,
      previousSentReturns = None
    )

    fillingOutReturn -> draftReturn
  }

  def sessionWithDisposalDetailsAnswers(
    answers: Option[DisposalDetailsAnswers],
    disposalMethod: DisposalMethod,
    userType: UserType,
    individualUserType: Option[IndividualUserType],
    isAmend: Boolean
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {

    val (journey, draftReturn) = fillingOutReturn(
      disposalMethod,
      userType,
      individualUserType,
      isAmend
    )
    val updatedDraftReturn     = draftReturn.copy(disposalDetailsAnswers = answers)
    val updatedJourney         = journey.copy(draftReturn = updatedDraftReturn)
    val sessionData            = SessionData.empty.copy(
      journeyStatus = Some(updatedJourney),
      userType = Some(userType)
    )

    (sessionData, updatedJourney, updatedDraftReturn)

  }

  def sessionWithDisposalDetailsAnswers(
    answers: DisposalDetailsAnswers,
    disposalMethod: DisposalMethod,
    userType: UserType,
    individualUserType: Option[IndividualUserType],
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) =
    sessionWithDisposalDetailsAnswers(
      Some(answers),
      disposalMethod,
      userType,
      individualUserType,
      isAmend
    )

  def fillingOutIndirectReturn(
    disposalMethod: DisposalMethod,
    userType: UserType,
    individualUserType: Option[IndividualUserType],
    isAmend: Boolean
  ): (FillingOutReturn, DraftSingleIndirectDisposalReturn) = {

    val country = Country("NZ")

    val answers = sample[CompleteSingleDisposalTriageAnswers].copy(
      disposalMethod = disposalMethod,
      individualUserType = individualUserType,
      assetType = AssetType.IndirectDisposal,
      countryOfResidence = country
    )

    val subscribedDetails = sample[SubscribedDetails].copy(
      name = setNameForUserType(userType)
    )
    val draftReturn       = sample[DraftSingleIndirectDisposalReturn].copy(
      triageAnswers = answers
    )

    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = subscribedDetails,
      agentReferenceNumber = setAgentReferenceNumber(userType),
      amendReturnData = if (isAmend) Some(sample[AmendReturnData]) else None
    )

    fillingOutReturn -> draftReturn
  }

  def sessionWithIndirectDisposalDetailsAnswers(
    answers: Option[DisposalDetailsAnswers],
    disposalMethod: DisposalMethod,
    userType: UserType,
    individualUserType: Option[IndividualUserType],
    isAmend: Boolean
  ): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) = {

    val (journey, draftReturn) = fillingOutIndirectReturn(
      disposalMethod,
      userType,
      individualUserType,
      isAmend
    )

    val updatedDraftReturn = draftReturn.copy(
      disposalDetailsAnswers = answers
    )
    val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)
    val sessionData        = SessionData.empty.copy(
      journeyStatus = Some(updatedJourney),
      userType = Some(userType)
    )

    (sessionData, updatedJourney, updatedDraftReturn)

  }

  def sessionWithIndirectDisposalDetailsAnswers(
    answers: DisposalDetailsAnswers,
    disposalMethod: DisposalMethod,
    userType: UserType,
    individualUserType: Option[IndividualUserType],
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) =
    sessionWithIndirectDisposalDetailsAnswers(
      Some(answers),
      disposalMethod,
      userType,
      individualUserType,
      isAmend
    )

  val acceptedUserTypeGen: Gen[UserType] =
    Gen.oneOf(UserType.Agent, UserType.Organisation, UserType.Individual)

  val acceptedIndividualUserType: Gen[IndividualUserType] =
    Gen.oneOf(Self, Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin)

  "DisposalDetailsController" when {

    "handling requests to display the how much did you own page" must {

      def performAction(): Future[Result] =
        controller.howMuchDidYouOwn()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.howMuchDidYouOwn(),
        mockUUIDGenerator
      )

      "display the page" when {

        "the user has not answered the question before" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    None,
                    disposalMethod,
                    userType,
                    Some(individualUserType),
                    isAmend = false
                  )._1
                )
              }

              val userMsgKey = userMessageKey(individualUserType, userType)
              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"shareOfProperty$userMsgKey.title")
              )
          }
        }

        "the user has answered the question before but has not completed the disposal detail section" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    IncompleteDisposalDetailsAnswers.empty.copy(
                      shareOfProperty = Some(ShareOfProperty.Other(12.34))
                    ),
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }
              val userMsgKey = userMessageKey(individualUserType, userType)
              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"shareOfProperty$userMsgKey.title"),
                doc => doc.select("#percentageShare").attr("value") shouldBe "12.34"
              )
          }
        }

        "the user has answered the question before but has completed the disposal detail section" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    sample[CompleteDisposalDetailsAnswers].copy(
                      shareOfProperty = ShareOfProperty.Other(12.34)
                    ),
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }
              val userMsgKey = userMessageKey(individualUserType, userType)
              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"shareOfProperty$userMsgKey.title"),
                doc => doc.select("#percentageShare").attr("value") shouldBe "12.34"
              )
          }
        }

      }

    }

    "handling submitted answers to the how much did you own page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.howMuchDidYouOwnSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: DisposalDetailsAnswers,
        isFurtherOrAmendReturn: Boolean
      ): DraftSingleDisposalReturn =
        d.copy(
          disposalDetailsAnswers = Some(newAnswers),
          acquisitionDetailsAnswers = None,
          initialGainOrLoss = None,
          reliefDetailsAnswers = d.reliefDetailsAnswers
            .map(_.unsetPrrAndLettingRelief(d.triageAnswers.isPeriodOfAdmin)),
          exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else d.exemptionAndLossesAnswers,
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers
            .flatMap(_.unsetAllButIncomeDetails()),
          gainOrLossAfterReliefs = None
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.howMuchDidYouOwnSubmit(),
        mockUUIDGenerator
      )

      "show a form error" when {

        def test(
          data: (String, String)*
        )(
          expectedErrorMessageKey: String
        )(
          disposalMethod: DisposalMethod,
          userType: UserType,
          individualUserType: IndividualUserType
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithDisposalDetailsAnswers(
                sample[CompleteDisposalDetailsAnswers],
                disposalMethod,
                userType,
                Some(individualUserType)
              )._1
            )
          }

          val userMsgKey = userMessageKey(individualUserType, userType)

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey(s"shareOfProperty$userMsgKey.title"),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "nothing is submitted" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val userMsgKey = userMessageKey(individualUserType, userType)
              test()(s"shareOfProperty$userMsgKey.error.required")(
                disposalMethod,
                userType,
                individualUserType
              )
          }
        }

        "other is selected but no percentage is submitted" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val userMsgKey = userMessageKey(individualUserType, userType)
              test("shareOfProperty" -> "2")(
                s"percentageShare$userMsgKey.error.required"
              )(
                disposalMethod,
                userType,
                individualUserType
              )
          }
        }

        "the number is less than zero" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val userMsgKey = userMessageKey(individualUserType, userType)
              test("shareOfProperty" -> "2", "percentageShare" -> "-1")(
                s"percentageShare$userMsgKey.error.tooSmall"
              )(
                disposalMethod,
                userType,
                individualUserType
              )
          }
        }

        "the number is greater than 100" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val userMsgKey = userMessageKey(individualUserType, userType)
              test("shareOfProperty" -> "2", "percentageShare" -> "101")(
                s"percentageShare$userMsgKey.error.tooLarge"
              )(
                disposalMethod,
                userType,
                individualUserType
              )
          }
        }

        "the number has more than two decimal places" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val userMsgKey = userMessageKey(individualUserType, userType)
              test("shareOfProperty" -> "2", "percentageShare" -> "1.234")(
                s"percentageShare$userMsgKey.error.tooManyDecimals"
              )(disposalMethod, userType, individualUserType)
          }
        }

        "the submitted value for shareOfProperty is not an integer" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val userMsgKey = userMessageKey(individualUserType, userType)
              test("shareOfProperty" -> "abc")(
                s"shareOfProperty$userMsgKey.error.invalid"
              )(
                disposalMethod,
                userType,
                individualUserType
              )
          }
        }

        "the submitted value for shareOfProperty is an integer but is not recognised" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val userMsgKey = userMessageKey(individualUserType, userType)
              test("shareOfProperty" -> "3")(
                s"shareOfProperty$userMsgKey.error.invalid"
              )(
                disposalMethod,
                userType,
                individualUserType
              )
          }
        }

      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val currentAnswers                  = sample[CompleteDisposalDetailsAnswers]
                .copy(shareOfProperty = ShareOfProperty.Half)
              val (session, journey, draftReturn) =
                sessionWithDisposalDetailsAnswers(currentAnswers, disposalMethod, userType, Some(individualUserType))
              val newShare                        = ShareOfProperty.Full
              val newDraftReturn                  =
                updateDraftReturn(
                  draftReturn,
                  IncompleteDisposalDetailsAnswers(Some(newShare), None, None),
                  isFurtherOrAmendReturn = false
                )
              val newJourney                      = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(
                  Left(Error(""))
                )
              }

              checkIsTechnicalErrorPage(
                performAction(Seq("shareOfProperty" -> "0"))
              )
          }
        }

        "there is an error updating the session data" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val currentAnswers                  = sample[CompleteDisposalDetailsAnswers]
                .copy(shareOfProperty = ShareOfProperty.Half)
              val (session, journey, draftReturn) =
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )
              val newShare                        = ShareOfProperty.Full
              val newDraftReturn                  =
                updateDraftReturn(
                  draftReturn,
                  IncompleteDisposalDetailsAnswers(Some(newShare), None, None),
                  isFurtherOrAmendReturn = false
                )
              val newJourney                      = journey.copy(draftReturn = newDraftReturn)

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
                performAction(Seq("shareOfProperty" -> "0"))
              )
          }
        }

      }

      "redirect to the cya page" when {

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated" in {
            forAll(
              acceptedUserTypeGen,
              disposalMethodGen,
              acceptedIndividualUserType
            ) {
              (
                userType: UserType,
                disposalMethod: DisposalMethod,
                individualUserType: IndividualUserType
              ) =>
                val (session, journey, draftReturn) =
                  sessionWithDisposalDetailsAnswers(
                    None,
                    disposalMethod,
                    userType,
                    Some(individualUserType),
                    isAmend = true
                  )
                val (newShare, newShareValue)       = ShareOfProperty.Full -> "0"
                val updatedAnswers                  = IncompleteDisposalDetailsAnswers.empty.copy(
                  shareOfProperty = Some(newShare),
                  disposalPrice = None,
                  disposalFees = None
                )
                val newDraftReturn                  =
                  updateDraftReturn(draftReturn, updatedAnswers, isFurtherOrAmendReturn = true)
                val newJourney                      =
                  journey.copy(draftReturn = newDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(newJourney)(
                    Right(())
                  )
                  mockStoreSession(
                    session.copy(
                      journeyStatus = Some(newJourney)
                    )
                  )(Right(()))
                }

                checkIsRedirect(
                  performAction(Seq("shareOfProperty" -> newShareValue)),
                  controllers.returns.disposaldetails.routes.DisposalDetailsController
                    .checkYourAnswers()
                )
            }
          }

        "the user has not answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
            forAll(
              acceptedUserTypeGen,
              disposalMethodGen,
              acceptedIndividualUserType
            ) {
              (
                userType: UserType,
                disposalMethod: DisposalMethod,
                individualUserType: IndividualUserType
              ) =>
                val currentAnswers                  = sample[IncompleteDisposalDetailsAnswers]
                  .copy(shareOfProperty = None)
                val (session, journey, draftReturn) =
                  sessionWithDisposalDetailsAnswers(
                    currentAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )
                val (newShare, newShareValue)       = ShareOfProperty.Half -> "1"
                val updatedAnswers                  = currentAnswers.copy(
                  shareOfProperty = Some(newShare),
                  disposalPrice = None,
                  disposalFees = None
                )
                val newDraftReturn                  =
                  updateDraftReturn(draftReturn, updatedAnswers, isFurtherOrAmendReturn = false)
                val newJourney                      = journey.copy(draftReturn = newDraftReturn)

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

                checkIsRedirect(
                  performAction(Seq("shareOfProperty" -> newShareValue)),
                  controllers.returns.disposaldetails.routes.DisposalDetailsController
                    .checkYourAnswers()
                )
            }
          }

        "the user has answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
            forAll(
              acceptedUserTypeGen,
              disposalMethodGen,
              acceptedIndividualUserType
            ) {
              (
                userType: UserType,
                disposalMethod: DisposalMethod,
                individualUserType: IndividualUserType
              ) =>
                forAll { (c: CompleteDisposalDetailsAnswers) =>
                  val currentAnswers                  =
                    c.copy(shareOfProperty = ShareOfProperty.Full)
                  val percentage                      = 40.23
                  val (session, journey, draftReturn) =
                    sessionWithDisposalDetailsAnswers(
                      currentAnswers,
                      disposalMethod,
                      userType,
                      Some(individualUserType)
                    )

                  val (newShare, newShareValue) =
                    ShareOfProperty.Other(percentage) -> "2"

                  val newDraftReturn = updateDraftReturn(
                    draftReturn,
                    IncompleteDisposalDetailsAnswers(Some(newShare), None, None),
                    isFurtherOrAmendReturn = false
                  )
                  val newJourney     = journey.copy(draftReturn = newDraftReturn)

                  inSequence {
                    mockAuthWithNoRetrievals()
                    mockGetSession(session)
                    mockStoreDraftReturn(newJourney)(
                      Right(())
                    )
                    mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
                  }
                  checkIsRedirect(
                    performAction(
                      Seq(
                        "shareOfProperty" -> newShareValue,
                        "percentageShare" -> percentage.toString
                      )
                    ),
                    controllers.returns.disposaldetails.routes.DisposalDetailsController
                      .checkYourAnswers()
                  )
                }
            }
          }

      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            acceptedIndividualUserType
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val currentAnswers = sample[CompleteDisposalDetailsAnswers]
                .copy(shareOfProperty = ShareOfProperty.Full)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    currentAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(Seq("shareOfProperty" -> "0")),
                controllers.returns.disposaldetails.routes.DisposalDetailsController
                  .checkYourAnswers()
              )
          }
        }
      }

      "convert a submitted option of 'Other' with value 100 to the 'Full' option" in {
        forAll(
          acceptedUserTypeGen,
          disposalMethodGen,
          acceptedIndividualUserType
        ) {
          (
            userType: UserType,
            disposalMethod: DisposalMethod,
            individualUserType: IndividualUserType
          ) =>
            val (session, journey, draftReturn) =
              sessionWithDisposalDetailsAnswers(
                None,
                disposalMethod,
                userType,
                Some(individualUserType),
                isAmend = false
              )
            val newDraftReturn                  =
              updateDraftReturn(
                draftReturn,
                IncompleteDisposalDetailsAnswers(
                  Some(ShareOfProperty.Full),
                  None,
                  None
                ),
                isFurtherOrAmendReturn = false
              )
            val newJourney                      = journey.copy(draftReturn = newDraftReturn)

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

            checkIsRedirect(
              performAction(
                Seq("shareOfProperty" -> "2", "percentageShare" -> "100")
              ),
              controllers.returns.disposaldetails.routes.DisposalDetailsController
                .checkYourAnswers()
            )
        }
      }

      "convert a submitted option of 'Other' with value 50 to the 'Half' option" in {
        forAll(
          acceptedUserTypeGen,
          disposalMethodGen,
          acceptedIndividualUserType
        ) {
          (
            userType: UserType,
            disposalMethod: DisposalMethod,
            individualUserType: IndividualUserType
          ) =>
            val (session, journey, draftReturn) =
              sessionWithDisposalDetailsAnswers(
                None,
                disposalMethod,
                userType,
                Some(individualUserType),
                isAmend = false
              )
            val newDraftReturn                  =
              updateDraftReturn(
                draftReturn,
                IncompleteDisposalDetailsAnswers(
                  Some(ShareOfProperty.Half),
                  None,
                  None
                ),
                isFurtherOrAmendReturn = false
              )
            val newJourney                      = journey.copy(draftReturn = newDraftReturn)

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

            checkIsRedirect(
              performAction(
                Seq("shareOfProperty" -> "2", "percentageShare" -> "50")
              ),
              controllers.returns.disposaldetails.routes.DisposalDetailsController
                .checkYourAnswers()
            )
        }
      }

    }

    "handling requests to display the what was disposal price page" must {

      def performAction(): Future[Result] =
        controller.whatWasDisposalPrice()(FakeRequest())

      val requiredPreviousAnswers =
        IncompleteDisposalDetailsAnswers.empty
          .copy(shareOfProperty = Some(ShareOfProperty.Full))

      def disposalPriceTitleScenarios(
        individualUserType: IndividualUserType,
        userType: UserType
      ): Seq[(DisposalMethod, ShareOfProperty, String)] = {
        val userMsgKey                                                            = userMessageKey(individualUserType, userType)
        def row(disposalMethod: DisposalMethod, shareOfProperty: ShareOfProperty) = {
          val expectedMessage = (disposalMethod, individualUserType, userType) match {
            case (_, Capacitor | PersonalRepresentative, _)                             => "disposalPrice.1.title"
            case (_, PersonalRepresentativeInPeriodOfAdmin, _)                          => "disposalPrice.2.title"
            case (DisposalMethod.Sold, _, UserType.Individual)                          => "disposalPrice.6.title"
            case (DisposalMethod.Other | DisposalMethod.Gifted, _, UserType.Individual) => "disposalPrice.5.title"
            case (_, _, UserType.Organisation)                                          => "disposalPrice.4.title"
            case (_, _, UserType.Agent)                                                 => "disposalPrice.3.title"
            case _                                                                      => "disposalPrice.6.title"
          }
          (disposalMethod, shareOfProperty, expectedMessage)
        }

        List(
          (DisposalMethod.Sold, ShareOfProperty.Full, s"disposalPrice$userMsgKey.SoldOther.title"),
          (DisposalMethod.Gifted, ShareOfProperty.Full, s"disposalPrice$userMsgKey.Gifted.title"),
          (DisposalMethod.Other, ShareOfProperty.Full, s"disposalPrice$userMsgKey.SoldOther.title"),
          row(DisposalMethod.Sold, ShareOfProperty.Half),
          row(DisposalMethod.Sold, ShareOfProperty.Other(1)),
          row(DisposalMethod.Gifted, ShareOfProperty.Half),
          row(DisposalMethod.Gifted, ShareOfProperty.Other(1)),
          row(DisposalMethod.Other, ShareOfProperty.Half),
          row(DisposalMethod.Other, ShareOfProperty.Other(1))
        )
      }

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whatWasDisposalPrice(),
        mockUUIDGenerator
      )

      behave like noDisposalMethodBehaviour(() => performAction())

      behave like noPropertyShareBehaviour(() => performAction())

      "display the page" when {

        "the user has not answered the question before" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserType) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              disposalPriceTitleScenarios(individualUserType, userType)
                .foreach { case (disposalMethod, share, expectedTitleKey) =>
                  withClue(
                    s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                      s"($disposalMethod $share, $expectedTitleKey): "
                  ) {
                    inSequence {
                      mockAuthWithNoRetrievals()
                      mockGetSession(
                        sessionWithDisposalDetailsAnswers(
                          requiredPreviousAnswers.copy(shareOfProperty = Some(share)),
                          disposalMethod,
                          userType,
                          Some(individualUserType)
                        )._1
                      )
                    }

                    checkPageIsDisplayed(
                      performAction(),
                      messageFromMessageKey(expectedTitleKey)
                    )
                  }
                }
          }
        }

        "the user has answered the question before but has not completed the disposal details section" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              _: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              disposalPriceTitleScenarios(individualUserType, userType)
                .foreach { case (disposalMethod, share, expectedTitleKey) =>
                  withClue(
                    s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                      s"($disposalMethod $share, $expectedTitleKey): "
                  ) {
                    inSequence {
                      mockAuthWithNoRetrievals()
                      mockGetSession(
                        sessionWithDisposalDetailsAnswers(
                          requiredPreviousAnswers.copy(
                            shareOfProperty = Some(share),
                            disposalPrice = Some(AmountInPence.fromPounds(12.34))
                          ),
                          disposalMethod,
                          userType,
                          Some(individualUserType)
                        )._1
                      )
                    }

                    checkPageIsDisplayed(
                      performAction(),
                      messageFromMessageKey(expectedTitleKey),
                      doc =>
                        doc
                          .select("#disposalPrice")
                          .attr("value") shouldBe "12.34"
                    )
                  }
                }
          }
        }

        "the user has answered the question before but has completed the disposal details section" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              _: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              disposalPriceTitleScenarios(individualUserType, userType)
                .foreach { case (disposalMethod, share, expectedTitleKey) =>
                  withClue(
                    s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                      s"($disposalMethod $share, $expectedTitleKey): "
                  ) {
                    inSequence {
                      mockAuthWithNoRetrievals()
                      mockGetSession(
                        sessionWithDisposalDetailsAnswers(
                          sample[CompleteDisposalDetailsAnswers].copy(
                            shareOfProperty = share,
                            disposalPrice = AmountInPence.fromPounds(12.34)
                          ),
                          disposalMethod,
                          userType,
                          Some(individualUserType)
                        )._1
                      )
                    }

                    checkPageIsDisplayed(
                      performAction(),
                      messageFromMessageKey(expectedTitleKey),
                      doc =>
                        doc
                          .select("#disposalPrice")
                          .attr("value") shouldBe "12.34"
                    )
                  }
                }
          }
        }

      }
    }

    "handling submitted answers to the what was disposal price page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.whatWasDisposalPriceSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: DisposalDetailsAnswers,
        isFurtherOrAmendReturn: Boolean
      ): DraftSingleDisposalReturn =
        d.copy(
          disposalDetailsAnswers = Some(newAnswers),
          initialGainOrLoss = None,
          reliefDetailsAnswers =
            d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief(d.triageAnswers.isPeriodOfAdmin)),
          gainOrLossAfterReliefs = None,
          exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else d.exemptionAndLossesAnswers,
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whatWasDisposalPriceSubmit(),
        mockUUIDGenerator
      )

      behave like noDisposalMethodBehaviour(() => performAction(Seq.empty))

      behave like noPropertyShareBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        val key = "disposalPrice"

        def disposalMethodErrorKey(disposalMethod: DisposalMethod): String =
          disposalMethod match {
            case DisposalMethod.Gifted => ".Gifted"
            case _                     => ".SoldOther"
          }

        def test(
          data: (String, String)*
        )(
          expectedErrorMessageKey: String
        )(
          disposalMethod: DisposalMethod,
          userType: UserType,
          individualUserType: IndividualUserType
        ): Unit = {

          val disposalMethodKey = disposalMethodErrorKey(disposalMethod)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithDisposalDetailsAnswers(
                sample[CompleteDisposalDetailsAnswers].copy(
                  shareOfProperty = ShareOfProperty.Full
                ),
                disposalMethod,
                userType,
                Some(individualUserType)
              )._1
            )
          }

          val userKey = userMessageKey(individualUserType, userType)

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey(s"$key$userKey$disposalMethodKey.title"),
            doc => {
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text()       shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              )
              doc
                .select("#back, .govuk-back-link")
                .attr("href") shouldBe routes.DisposalDetailsController
                .checkYourAnswers()
                .url
            },
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val disposalMethodKey = disposalMethodErrorKey(disposalMethod)
              val userKey           = userMessageKey(individualUserType, userType)

              amountOfMoneyErrorScenarios(
                key = key,
                errorContext = Some(s"$key$userKey$disposalMethodKey")
              ).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  test(scenario.formData*)(scenario.expectedErrorMessageKey)(
                    disposalMethod,
                    userType,
                    individualUserType
                  )
                }
              }
          }
        }
      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val currentAnswers                  =
                sample[CompleteDisposalDetailsAnswers]
                  .copy(disposalPrice = AmountInPence.fromPounds(1d))
              val (session, journey, draftReturn) =
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )
              val newDisposalPrice                = AmountInPence.fromPounds(2d)
              val newDraftReturn                  = updateDraftReturn(
                draftReturn,
                currentAnswers.copy(disposalPrice = newDisposalPrice),
                isFurtherOrAmendReturn = false
              )
              val newJourney                      = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(
                  Left(Error(""))
                )
              }

              checkIsTechnicalErrorPage(
                performAction(
                  Seq("disposalPrice" -> newDisposalPrice.inPounds().toString)
                )
              )
          }
        }

        "there is an error updating the session data" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val currentAnswers                  =
                sample[CompleteDisposalDetailsAnswers]
                  .copy(disposalPrice = AmountInPence.fromPounds(1d))
              val (session, journey, draftReturn) =
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )
              val newDisposalPrice                = AmountInPence.fromPounds(2d)
              val newDraftReturn                  = updateDraftReturn(
                draftReturn,
                currentAnswers.copy(disposalPrice = newDisposalPrice),
                isFurtherOrAmendReturn = false
              )
              val newJourney                      = journey.copy(draftReturn = newDraftReturn)

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
                  Seq("disposalPrice" -> newDisposalPrice.inPounds().toString)
                )
              )
          }
        }

      }

      "redirect to the cya page" when {

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated" in {
            forAll(
              acceptedUserTypeGen,
              disposalMethodGen,
              individualUserTypeGen
            ) {
              (
                userType: UserType,
                disposalMethod: DisposalMethod,
                individualUserType: IndividualUserType
              ) =>
                val incompleteDisposalDetailsAnswers =
                  IncompleteDisposalDetailsAnswers(
                    Some(ShareOfProperty.Full),
                    None,
                    None
                  )
                val (session, journey, draftReturn)  =
                  sessionWithDisposalDetailsAnswers(
                    incompleteDisposalDetailsAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )

                val newDisposalPrice = 2d
                val updatedAnswers   = incompleteDisposalDetailsAnswers.copy(
                  disposalPrice = Some(AmountInPence.fromPounds(newDisposalPrice))
                )
                val newDraftReturn   =
                  updateDraftReturn(draftReturn, updatedAnswers, isFurtherOrAmendReturn = false)
                val newJourney       = journey.copy(draftReturn = newDraftReturn)

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

                checkIsRedirect(
                  performAction(
                    Seq("disposalPrice" -> newDisposalPrice.toString)
                  ),
                  controllers.returns.disposaldetails.routes.DisposalDetailsController
                    .checkYourAnswers()
                )
            }
          }

        "the user has not answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
            forAll(
              acceptedUserTypeGen,
              disposalMethodGen,
              individualUserTypeGen
            ) {
              (
                userType: UserType,
                disposalMethod: DisposalMethod,
                individualUserType: IndividualUserType
              ) =>
                val currentAnswers                  = sample[IncompleteDisposalDetailsAnswers]
                  .copy(
                    shareOfProperty = Some(sample[ShareOfProperty]),
                    disposalPrice = None
                  )
                val (session, journey, draftReturn) =
                  sessionWithDisposalDetailsAnswers(
                    currentAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )

                val newDisposalPrice = 2d
                val updatedAnswers   = currentAnswers.copy(
                  disposalPrice = Some(AmountInPence.fromPounds(newDisposalPrice))
                )
                val newDraftReturn   =
                  updateDraftReturn(draftReturn, updatedAnswers, isFurtherOrAmendReturn = false)
                val newJourney       = journey.copy(draftReturn = newDraftReturn)

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(newJourney)(
                    Right(())
                  )
                  mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
                }

                checkIsRedirect(
                  performAction(
                    Seq("disposalPrice" -> newDisposalPrice.toString)
                  ),
                  controllers.returns.disposaldetails.routes.DisposalDetailsController
                    .checkYourAnswers()
                )
            }
          }

        "the user has answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
            forAll {
              (
                userType: UserType,
                disposalMethod: DisposalMethod,
                c: CompleteDisposalDetailsAnswers,
                individualUserType: IndividualUserType
              ) =>
                val currentAnswers                  =
                  c.copy(disposalPrice = AmountInPence.fromPounds(1d))
                val (session, journey, draftReturn) =
                  sessionWithDisposalDetailsAnswers(
                    currentAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType),
                    isAmend = true
                  )

                val newDisposalPrice = 2d
                val newDraftReturn   = updateDraftReturn(
                  draftReturn,
                  currentAnswers.copy(disposalPrice = AmountInPence.fromPounds(newDisposalPrice)),
                  isFurtherOrAmendReturn = true
                )
                val newJourney       =
                  journey.copy(draftReturn = newDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends

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

                checkIsRedirect(
                  performAction(
                    Seq("disposalPrice" -> newDisposalPrice.toString)
                  ),
                  controllers.returns.disposaldetails.routes.DisposalDetailsController
                    .checkYourAnswers()
                )
            }
          }
      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val currentAnswers =
                sample[CompleteDisposalDetailsAnswers]
                  .copy(disposalPrice = AmountInPence.fromPounds(1d))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    currentAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(Seq("disposalPrice" -> "1")),
                controllers.returns.disposaldetails.routes.DisposalDetailsController
                  .checkYourAnswers()
              )
          }
        }
      }

      "accept submitted values with commas" in {
        forAll(acceptedUserTypeGen, disposalMethodGen, individualUserTypeGen) {
          (
            userType: UserType,
            disposalMethod: DisposalMethod,
            individualUserType: IndividualUserType
          ) =>
            val currentAnswers =
              sample[CompleteDisposalDetailsAnswers]
                .copy(disposalPrice = AmountInPence.fromPounds(1000d))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )._1
              )
            }

            checkIsRedirect(
              performAction(Seq("disposalPrice" -> "1,000")),
              controllers.returns.disposaldetails.routes.DisposalDetailsController
                .checkYourAnswers()
            )
        }
      }

      "accept submitted values with pound signs" in {
        forAll(acceptedUserTypeGen, disposalMethodGen, individualUserTypeGen) {
          (
            userType: UserType,
            disposalMethod: DisposalMethod,
            individualUserType: IndividualUserType
          ) =>
            val currentAnswers =
              sample[CompleteDisposalDetailsAnswers]
                .copy(disposalPrice = AmountInPence.fromPounds(1d))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )._1
              )
            }

            checkIsRedirect(
              performAction(Seq("disposalPrice" -> "£1")),
              controllers.returns.disposaldetails.routes.DisposalDetailsController
                .checkYourAnswers()
            )
        }
      }

    }

    "handling requests to display the what was disposal price page for indirect disposal assettype" must {

      def performAction(): Future[Result] =
        controller.whatWasDisposalPrice()(FakeRequest())

      val requiredPreviousAnswers = IncompleteDisposalDetailsAnswers.empty.copy(
        shareOfProperty = Some(ShareOfProperty.Full)
      )

      def disposalPriceTitleScenarios(
        individualUserType: IndividualUserType,
        userType: UserType
      ): List[(DisposalMethod, ShareOfProperty, String)] = {
        val userMsgKey = userMessageKey(individualUserType, userType)
        val key        = "disposalPrice"

        List(
          (DisposalMethod.Sold, ShareOfProperty.Full, s"$key$userMsgKey.indirect.SoldOther.title"),
          (DisposalMethod.Gifted, ShareOfProperty.Full, s"$key$userMsgKey.indirect.Gifted.title"),
          (DisposalMethod.Other, ShareOfProperty.Full, s"$key$userMsgKey.indirect.SoldOther.title")
        )
      }

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whatWasDisposalPrice(),
        mockUUIDGenerator
      )

      behave like noDisposalMethodBehaviour(() => performAction())

      behave like noPropertyShareBehaviour(() => performAction())

      "display the page" when {

        "the user has not answered the question before" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserType) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              disposalPriceTitleScenarios(individualUserType, userType)
                .foreach { case (disposalMethod, share, expectedTitleKey) =>
                  withClue(
                    s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                      s"($disposalMethod $share, $expectedTitleKey): "
                  ) {
                    inSequence {
                      mockAuthWithNoRetrievals()
                      mockGetSession(
                        sessionWithIndirectDisposalDetailsAnswers(
                          requiredPreviousAnswers.copy(shareOfProperty = Some(share)),
                          disposalMethod,
                          userType,
                          Some(individualUserType)
                        )._1
                      )
                    }

                    checkPageIsDisplayed(
                      performAction(),
                      messageFromMessageKey(expectedTitleKey)
                    )
                  }
                }
          }
        }

        "the user has answered the question before but has not completed the disposal details section" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              _: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              disposalPriceTitleScenarios(individualUserType, userType)
                .foreach { case (disposalMethod, share, expectedTitleKey) =>
                  withClue(
                    s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                      s"($disposalMethod $share, $expectedTitleKey): "
                  ) {
                    inSequence {
                      mockAuthWithNoRetrievals()
                      mockGetSession(
                        sessionWithIndirectDisposalDetailsAnswers(
                          requiredPreviousAnswers.copy(
                            shareOfProperty = Some(share),
                            disposalPrice = Some(AmountInPence.fromPounds(12.34))
                          ),
                          disposalMethod,
                          userType,
                          Some(individualUserType)
                        )._1
                      )
                    }

                    checkPageIsDisplayed(
                      performAction(),
                      messageFromMessageKey(expectedTitleKey),
                      doc =>
                        doc
                          .select("#disposalPrice")
                          .attr("value") shouldBe "12.34"
                    )
                  }
                }
          }
        }

        "the user has answered the question before but has completed the disposal details section" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              _: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              disposalPriceTitleScenarios(individualUserType, userType)
                .foreach { case (disposalMethod, share, expectedTitleKey) =>
                  withClue(
                    s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                      s"($disposalMethod $share, $expectedTitleKey): "
                  ) {
                    inSequence {
                      mockAuthWithNoRetrievals()
                      mockGetSession(
                        sessionWithIndirectDisposalDetailsAnswers(
                          sample[CompleteDisposalDetailsAnswers].copy(
                            shareOfProperty = share,
                            disposalPrice = AmountInPence.fromPounds(12.34)
                          ),
                          disposalMethod,
                          userType,
                          Some(individualUserType)
                        )._1
                      )
                    }

                    checkPageIsDisplayed(
                      performAction(),
                      messageFromMessageKey(expectedTitleKey),
                      doc =>
                        doc
                          .select("#disposalPrice")
                          .attr("value") shouldBe "12.34"
                    )
                  }
                }
          }
        }

      }

    }

    "handling submitted answers to the what was disposal price page for indirect disposal assettype" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.whatWasDisposalPriceSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: DisposalDetailsAnswers,
        isFurtherOrAmendReturn: Boolean
      ): DraftSingleDisposalReturn =
        d.copy(
          disposalDetailsAnswers = Some(newAnswers),
          initialGainOrLoss = None,
          reliefDetailsAnswers =
            d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief(d.triageAnswers.isPeriodOfAdmin)),
          gainOrLossAfterReliefs = None,
          exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else d.exemptionAndLossesAnswers,
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whatWasDisposalPriceSubmit(),
        mockUUIDGenerator
      )

      behave like noDisposalMethodBehaviour(() => performAction(Seq.empty))

      behave like noPropertyShareBehaviour(() => performAction(Seq.empty))

      "show a form error" when {

        val key = "disposalPrice"

        def disposalMethodErrorKey(disposalMethod: DisposalMethod): String =
          disposalMethod match {
            case DisposalMethod.Gifted => ".Gifted"
            case _                     => ".SoldOther"
          }

        def test(
          data: (String, String)*
        )(
          expectedErrorMessageKey: String
        )(
          disposalMethod: DisposalMethod,
          userType: UserType,
          individualUserType: IndividualUserType
        )(
          userKey: String
        ): Unit = {

          val disposalMethodKey = disposalMethodErrorKey(disposalMethod)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithIndirectDisposalDetailsAnswers(
                sample[CompleteDisposalDetailsAnswers].copy(
                  shareOfProperty = ShareOfProperty.Full
                ),
                disposalMethod,
                userType,
                Some(individualUserType)
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey(s"$key$userKey.indirect$disposalMethodKey.title"),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "the data is invalid" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val disposalMethodKey = disposalMethodErrorKey(disposalMethod)
              val userKey           = userMessageKey(individualUserType, userType)

              amountOfMoneyErrorScenarios(
                key = key,
                errorContext = Some(s"$key$userKey.indirect$disposalMethodKey")
              ).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  test(scenario.formData*)(scenario.expectedErrorMessageKey)(
                    disposalMethod,
                    userType,
                    individualUserType
                  )(
                    userKey
                  )
                }
              }
          }
        }

      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val currentAnswers                  =
                sample[CompleteDisposalDetailsAnswers]
                  .copy(disposalPrice = AmountInPence.fromPounds(1d))
              val (session, journey, draftReturn) =
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )
              val newDisposalPrice                = AmountInPence.fromPounds(2d)
              val newDraftReturn                  = updateDraftReturn(
                draftReturn,
                currentAnswers.copy(disposalPrice = newDisposalPrice),
                isFurtherOrAmendReturn = false
              )
              val newJourney                      = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(
                  Left(Error(""))
                )
              }

              checkIsTechnicalErrorPage(
                performAction(
                  Seq("disposalPrice" -> newDisposalPrice.inPounds().toString)
                )
              )
          }
        }

        "there is an error updating the session data" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val currentAnswers                  =
                sample[CompleteDisposalDetailsAnswers]
                  .copy(disposalPrice = AmountInPence.fromPounds(1d))
              val (session, journey, draftReturn) =
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )
              val newDisposalPrice                = AmountInPence.fromPounds(2d)
              val newDraftReturn                  = updateDraftReturn(
                draftReturn,
                currentAnswers.copy(disposalPrice = newDisposalPrice),
                isFurtherOrAmendReturn = false
              )
              val newJourney                      = journey.copy(draftReturn = newDraftReturn)

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
                  Seq("disposalPrice" -> newDisposalPrice.inPounds().toString)
                )
              )
          }
        }

      }

      "redirect to the cya page" when {

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated" in {
            forAll(
              acceptedUserTypeGen,
              disposalMethodGen,
              individualUserTypeGen
            ) {
              (
                userType: UserType,
                disposalMethod: DisposalMethod,
                individualUserType: IndividualUserType
              ) =>
                val incompleteDisposalDetailsAnswers =
                  IncompleteDisposalDetailsAnswers(
                    Some(ShareOfProperty.Full),
                    None,
                    None
                  )
                val (session, journey, draftReturn)  =
                  sessionWithDisposalDetailsAnswers(
                    incompleteDisposalDetailsAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )

                val newDisposalPrice = 2d
                val updatedAnswers   = incompleteDisposalDetailsAnswers.copy(
                  disposalPrice = Some(AmountInPence.fromPounds(newDisposalPrice))
                )
                val newDraftReturn   =
                  updateDraftReturn(draftReturn, updatedAnswers, isFurtherOrAmendReturn = false)
                val newJourney       = journey.copy(draftReturn = newDraftReturn)

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

                checkIsRedirect(
                  performAction(
                    Seq("disposalPrice" -> newDisposalPrice.toString)
                  ),
                  controllers.returns.disposaldetails.routes.DisposalDetailsController
                    .checkYourAnswers()
                )
            }
          }

        "the user has not answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
            forAll(
              acceptedUserTypeGen,
              disposalMethodGen,
              individualUserTypeGen
            ) {
              (
                userType: UserType,
                disposalMethod: DisposalMethod,
                individualUserType: IndividualUserType
              ) =>
                val currentAnswers                  = sample[IncompleteDisposalDetailsAnswers]
                  .copy(
                    shareOfProperty = Some(sample[ShareOfProperty]),
                    disposalPrice = None
                  )
                val (session, journey, draftReturn) =
                  sessionWithDisposalDetailsAnswers(
                    currentAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType),
                    isAmend = true
                  )

                val newDisposalPrice = 2d
                val updatedAnswers   = currentAnswers.copy(
                  disposalPrice = Some(AmountInPence.fromPounds(newDisposalPrice))
                )
                val newDraftReturn   =
                  updateDraftReturn(draftReturn, updatedAnswers, isFurtherOrAmendReturn = true)
                val newJourney       =
                  journey.copy(draftReturn = newDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends

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

                checkIsRedirect(
                  performAction(
                    Seq("disposalPrice" -> newDisposalPrice.toString)
                  ),
                  controllers.returns.disposaldetails.routes.DisposalDetailsController
                    .checkYourAnswers()
                )
            }
          }

        "the user has answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
            forAll {
              (
                userType: UserType,
                disposalMethod: DisposalMethod,
                c: CompleteDisposalDetailsAnswers,
                individualUserType: IndividualUserType
              ) =>
                val currentAnswers                  =
                  c.copy(disposalPrice = AmountInPence.fromPounds(1d))
                val (session, journey, draftReturn) =
                  sessionWithDisposalDetailsAnswers(
                    currentAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )

                val newDisposalPrice = 2d
                val newDraftReturn   = updateDraftReturn(
                  draftReturn,
                  currentAnswers.copy(disposalPrice = AmountInPence.fromPounds(newDisposalPrice)),
                  isFurtherOrAmendReturn = false
                )
                val newJourney       = journey.copy(draftReturn = newDraftReturn)

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

                checkIsRedirect(
                  performAction(
                    Seq("disposalPrice" -> newDisposalPrice.toString)
                  ),
                  controllers.returns.disposaldetails.routes.DisposalDetailsController
                    .checkYourAnswers()
                )
            }
          }
      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val currentAnswers =
                sample[CompleteDisposalDetailsAnswers]
                  .copy(disposalPrice = AmountInPence.fromPounds(1d))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    currentAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(Seq("disposalPrice" -> "1")),
                controllers.returns.disposaldetails.routes.DisposalDetailsController
                  .checkYourAnswers()
              )
          }
        }
      }

      "accept submitted values with commas" in {
        forAll(acceptedUserTypeGen, disposalMethodGen, individualUserTypeGen) {
          (
            userType: UserType,
            disposalMethod: DisposalMethod,
            individualUserType: IndividualUserType
          ) =>
            val currentAnswers =
              sample[CompleteDisposalDetailsAnswers]
                .copy(disposalPrice = AmountInPence.fromPounds(1000d))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )._1
              )
            }

            checkIsRedirect(
              performAction(Seq("disposalPrice" -> "1,000")),
              controllers.returns.disposaldetails.routes.DisposalDetailsController
                .checkYourAnswers()
            )
        }
      }

      "accept submitted values with pound signs" in {
        forAll(acceptedUserTypeGen, disposalMethodGen, individualUserTypeGen) {
          (
            userType: UserType,
            disposalMethod: DisposalMethod,
            individualUserType: IndividualUserType
          ) =>
            val currentAnswers =
              sample[CompleteDisposalDetailsAnswers]
                .copy(disposalPrice = AmountInPence.fromPounds(1d))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )._1
              )
            }

            checkIsRedirect(
              performAction(Seq("disposalPrice" -> "£1")),
              controllers.returns.disposaldetails.routes.DisposalDetailsController
                .checkYourAnswers()
            )
        }
      }

    }

    "handling requests to display the what were disposal fees page" must {

      def performAction(): Future[Result] =
        controller.whatWereDisposalFees()(FakeRequest())

      val requiredPreviousAnswers = IncompleteDisposalDetailsAnswers.empty
        .copy(
          shareOfProperty = Some(ShareOfProperty.Full),
          disposalPrice = Some(AmountInPence.fromPounds(2d))
        )

      def disposalFeesTitleScenarios(
        individualUserType: IndividualUserType,
        userType: UserType
      ): Seq[(DisposalMethod, ShareOfProperty, String)] = {
        def row(disposalMethod: DisposalMethod, shareOfProperty: ShareOfProperty) = {
          val expectedMessage = (individualUserType, userType) match {
            case (Capacitor | PersonalRepresentative, _)    => "disposalFees.2.title"
            case (PersonalRepresentativeInPeriodOfAdmin, _) => "disposalFees.3.title"
            case (_, UserType.Individual)                   => "disposalFees.1.title"
            case (_, UserType.Organisation)                 => "disposalFees.5.title"
            case (_, UserType.Agent)                        => "disposalFees.4.title"
            case _                                          => "disposalFees.1.title"
          }
          (disposalMethod, shareOfProperty, expectedMessage)
        }
        val userKey                                                               = userMessageKey(individualUserType, userType)
        List(
          (DisposalMethod.Sold, ShareOfProperty.Full, s"disposalFees$userKey.title"),
          (DisposalMethod.Gifted, ShareOfProperty.Full, s"disposalFees$userKey.title"),
          (DisposalMethod.Other, ShareOfProperty.Full, s"disposalFees$userKey.title"),
          row(DisposalMethod.Sold, ShareOfProperty.Half),
          row(DisposalMethod.Sold, ShareOfProperty.Other(1)),
          row(DisposalMethod.Gifted, ShareOfProperty.Half),
          row(DisposalMethod.Gifted, ShareOfProperty.Other(1)),
          row(DisposalMethod.Other, ShareOfProperty.Half),
          row(DisposalMethod.Other, ShareOfProperty.Other(1))
        )
      }

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whatWereDisposalFees(),
        mockUUIDGenerator
      )

      behave like noDisposalMethodBehaviour(() => performAction())

      behave like noPropertyShareBehaviour(() => performAction())

      "redirect to the disposal price page" when {

        "the user has not answered that question yet" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    requiredPreviousAnswers.copy(disposalPrice = None),
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.DisposalDetailsController.whatWasDisposalPrice()
              )
          }
        }

      }

      "display the page" when {

        "the user has not answered the question before" in {
          forAll(acceptedUserTypeGen, individualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              disposalFeesTitleScenarios(individualUserType, userType).foreach {
                case (disposalMethod, share, expectedTitleKey) =>
                  withClue(
                    s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                      s"($disposalMethod $share, $expectedTitleKey): "
                  ) {
                    inSequence {
                      mockAuthWithNoRetrievals()
                      mockGetSession(
                        sessionWithDisposalDetailsAnswers(
                          requiredPreviousAnswers.copy(
                            shareOfProperty = Some(share)
                          ),
                          disposalMethod,
                          userType,
                          Some(individualUserType)
                        )._1
                      )
                    }

                    checkPageIsDisplayed(
                      performAction(),
                      messageFromMessageKey(expectedTitleKey)
                    )
                  }
              }
          }
        }

        "the user has answered the question before but has not completed the disposal detail section" in {
          forAll(acceptedUserTypeGen, individualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              disposalFeesTitleScenarios(individualUserType, userType).foreach {
                case (disposalMethod, share, expectedTitleKey) =>
                  withClue(
                    s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                      s"($disposalMethod $share, $expectedTitleKey): "
                  ) {
                    inSequence {
                      mockAuthWithNoRetrievals()
                      mockGetSession(
                        sessionWithDisposalDetailsAnswers(
                          requiredPreviousAnswers.copy(
                            shareOfProperty = Some(share),
                            disposalFees = Some(AmountInPence.fromPounds(12.34))
                          ),
                          disposalMethod,
                          userType,
                          Some(individualUserType)
                        )._1
                      )
                    }

                    checkPageIsDisplayed(
                      performAction(),
                      messageFromMessageKey(expectedTitleKey),
                      doc =>
                        doc
                          .select("#disposalFees")
                          .attr("value") shouldBe "12.34"
                    )
                  }
              }
          }
        }

        "the user has answered the question before but has completed the disposal detail section" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              _: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              disposalFeesTitleScenarios(individualUserType, userType).foreach {
                case (disposalMethod, share, expectedTitleKey) =>
                  withClue(
                    s"For (disposalMethod, shareOfProperty, expectedTitleKey) = " +
                      s"($disposalMethod $share, $expectedTitleKey): "
                  ) {
                    inSequence {
                      mockAuthWithNoRetrievals()
                      mockGetSession(
                        sessionWithDisposalDetailsAnswers(
                          sample[CompleteDisposalDetailsAnswers].copy(
                            shareOfProperty = share,
                            disposalFees = AmountInPence.fromPounds(12.34)
                          ),
                          disposalMethod,
                          userType,
                          Some(individualUserType)
                        )._1
                      )
                    }

                    checkPageIsDisplayed(
                      performAction(),
                      messageFromMessageKey(expectedTitleKey),
                      doc =>
                        doc
                          .select("#disposalFees")
                          .attr("value") shouldBe "12.34"
                    )
                  }
              }
          }
        }

      }

    }

    "handling submitted answers to the what was disposal fees page" must {

      def performAction(data: Seq[(String, String)]): Future[Result] =
        controller.whatWereDisposalFeesSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      def updateDraftReturn(
        d: DraftSingleDisposalReturn,
        newAnswers: DisposalDetailsAnswers,
        isFurtherOrAmendReturn: Boolean
      ): DraftSingleDisposalReturn =
        d.copy(
          disposalDetailsAnswers = Some(newAnswers),
          initialGainOrLoss = None,
          reliefDetailsAnswers =
            d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief(d.triageAnswers.isPeriodOfAdmin)),
          gainOrLossAfterReliefs = None,
          exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else d.exemptionAndLossesAnswers,
          yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
        )

      val requiredPreviousAnswers = IncompleteDisposalDetailsAnswers.empty
        .copy(
          shareOfProperty = Some(ShareOfProperty.Full),
          disposalPrice = Some(AmountInPence.fromPounds(2d))
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.whatWereDisposalFeesSubmit(),
        mockUUIDGenerator
      )

      behave like noDisposalMethodBehaviour(() => performAction(Seq.empty))

      behave like noPropertyShareBehaviour(() => performAction(Seq.empty))

      "redirect to the disposal price page" when {

        "the user has not answered that question yet" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    requiredPreviousAnswers.copy(disposalPrice = None),
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(Seq.empty),
                routes.DisposalDetailsController.whatWasDisposalPrice()
              )
          }
        }

      }

      "show a form error" when {

        def test(
          data: (String, String)*
        )(
          expectedErrorMessageKey: String
        )(
          disposalMethod: DisposalMethod,
          userType: UserType,
          individualUserType: IndividualUserType
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithDisposalDetailsAnswers(
                sample[CompleteDisposalDetailsAnswers].copy(
                  shareOfProperty = ShareOfProperty.Full
                ),
                disposalMethod,
                userType,
                Some(individualUserType)
              )._1
            )
          }
          val userKey = userMessageKey(individualUserType, userType)

          checkPageIsDisplayed(
            performAction(data),
            messageFromMessageKey(s"disposalFees$userKey.title"),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )
        }

        "the amount is submitted is invalid" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val userKey = userMessageKey(individualUserType, userType)

              amountOfMoneyErrorScenarios(
                key = "disposalFees",
                errorContext = Some(s"disposalFees$userKey")
              ).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  test(scenario.formData*)(scenario.expectedErrorMessageKey)(
                    disposalMethod,
                    userType,
                    individualUserType
                  )
                }
              }
          }
        }

      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val currentAnswers                  =
                sample[CompleteDisposalDetailsAnswers]
                  .copy(disposalFees = AmountInPence.fromPounds(1d))
              val (session, journey, draftReturn) =
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )

              val newDisposalFees = AmountInPence.fromPounds(2d)
              val newDraftReturn  = updateDraftReturn(
                draftReturn,
                currentAnswers.copy(
                  disposalFees = newDisposalFees
                ),
                isFurtherOrAmendReturn = false
              )
              val newJourney      = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(
                  Left(Error(""))
                )
              }

              checkIsTechnicalErrorPage(
                performAction(
                  Seq("disposalFees" -> newDisposalFees.inPounds().toString)
                )
              )
          }
        }

        "there is an error updating the session data" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val currentAnswers                  =
                sample[CompleteDisposalDetailsAnswers]
                  .copy(disposalFees = AmountInPence.fromPounds(1d))
              val (session, journey, draftReturn) =
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )

              val newDisposalFees = AmountInPence.fromPounds(2d)
              val newDraftReturn  = updateDraftReturn(
                draftReturn,
                currentAnswers.copy(
                  disposalFees = newDisposalFees
                ),
                isFurtherOrAmendReturn = false
              )
              val newJourney      = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(
                  Right(())
                )
                mockStoreSession(
                  session.copy(journeyStatus = Some(newJourney))
                )(
                  Left(Error(""))
                )
              }

              checkIsTechnicalErrorPage(
                performAction(
                  Seq("disposalFees" -> newDisposalFees.inPounds().toString)
                )
              )
          }
        }

      }

      "redirect to the cya page" when {

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated for disposalfees = 0" in {
            forAll(
              acceptedUserTypeGen,
              disposalMethodGen,
              acceptedIndividualUserType
            ) {
              (
                userType: UserType,
                disposalMethod: DisposalMethod,
                individualUserType: IndividualUserType
              ) =>
                val incompleteDisposalDetailsAnswers =
                  IncompleteDisposalDetailsAnswers(
                    Some(ShareOfProperty.Full),
                    Some(AmountInPence.fromPounds(1d)),
                    None
                  )

                val (session, journey, draftReturn) =
                  sessionWithDisposalDetailsAnswers(
                    incompleteDisposalDetailsAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )

                val disposalFees   = 0d
                val updatedAnswers = incompleteDisposalDetailsAnswers.copy(
                  disposalFees = Some(AmountInPence.fromPounds(disposalFees))
                )
                val newDraftReturn = updateDraftReturn(draftReturn, updatedAnswers, isFurtherOrAmendReturn = false)
                val newJourney     = journey.copy(draftReturn = newDraftReturn)

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(newJourney)(
                    Right(())
                  )
                  mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                    Right(())
                  )
                }

                checkIsRedirect(
                  performAction(Seq("disposalFees" -> disposalFees.toString)),
                  controllers.returns.disposaldetails.routes.DisposalDetailsController
                    .checkYourAnswers()
                )
            }
          }

        "the user hasn't ever answered the disposal details question " +
          "and the draft return and session data has been successfully updated" in {
            forAll(
              acceptedUserTypeGen,
              disposalMethodGen,
              acceptedIndividualUserType
            ) {
              (
                userType: UserType,
                disposalMethod: DisposalMethod,
                individualUserType: IndividualUserType
              ) =>
                val incompleteDisposalDetailsAnswers =
                  IncompleteDisposalDetailsAnswers(
                    Some(ShareOfProperty.Full),
                    Some(AmountInPence.fromPounds(1d)),
                    None
                  )

                val (session, journey, draftReturn) =
                  sessionWithDisposalDetailsAnswers(
                    incompleteDisposalDetailsAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType),
                    isAmend = true
                  )

                val newDisposalFees = 2d
                val updatedAnswers  = incompleteDisposalDetailsAnswers.copy(
                  disposalFees = Some(AmountInPence.fromPounds(newDisposalFees))
                )
                val newDraftReturn  =
                  updateDraftReturn(draftReturn, updatedAnswers, isFurtherOrAmendReturn = true)
                val newJourney      =
                  journey.copy(draftReturn = newDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(newJourney)(
                    Right(())
                  )
                  mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(
                    Right(())
                  )
                }

                checkIsRedirect(
                  performAction(Seq("disposalFees" -> newDisposalFees.toString)),
                  controllers.returns.disposaldetails.routes.DisposalDetailsController
                    .checkYourAnswers()
                )
            }
          }

        "the user has answered all of the disposal details questions " +
          "and the draft return and session data has been successfully updated" in {
            forAll(
              acceptedUserTypeGen,
              disposalMethodGen,
              acceptedIndividualUserType
            ) {
              (
                userType: UserType,
                disposalMethod: DisposalMethod,
                individualUserType: IndividualUserType
              ) =>
                val currentAnswers                  =
                  sample[CompleteDisposalDetailsAnswers]
                    .copy(disposalFees = AmountInPence.fromPounds(1d))
                val (session, journey, draftReturn) =
                  sessionWithDisposalDetailsAnswers(
                    currentAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )

                val newDisposalFees = 2d
                val newDraftReturn  = updateDraftReturn(
                  draftReturn,
                  currentAnswers.copy(
                    disposalFees = AmountInPence.fromPounds(newDisposalFees)
                  ),
                  isFurtherOrAmendReturn = false
                )
                val newJourney      = journey.copy(draftReturn = newDraftReturn)

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockStoreDraftReturn(newJourney)(
                    Right(())
                  )
                  mockStoreSession(
                    session.copy(journeyStatus = Some(newJourney))
                  )(
                    Right(())
                  )
                }

                checkIsRedirect(
                  performAction(Seq("disposalFees" -> newDisposalFees.toString)),
                  controllers.returns.disposaldetails.routes.DisposalDetailsController
                    .checkYourAnswers()
                )
            }
          }

      }

      "not update the draft return or the session data" when {

        "the answer given has not changed from a previous one" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val currentAnswers =
                sample[CompleteDisposalDetailsAnswers]
                  .copy(disposalFees = AmountInPence.fromPounds(1d))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    currentAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(Seq("disposalFees" -> "1")),
                controllers.returns.disposaldetails.routes.DisposalDetailsController
                  .checkYourAnswers()
              )
          }
        }

      }

      "accept submitted values with commas" in {
        forAll(acceptedUserTypeGen, disposalMethodGen, individualUserTypeGen) {
          (
            userType: UserType,
            disposalMethod: DisposalMethod,
            individualUserType: IndividualUserType
          ) =>
            val currentAnswers =
              sample[CompleteDisposalDetailsAnswers]
                .copy(disposalFees = AmountInPence.fromPounds(1000d))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )._1
              )
            }

            checkIsRedirect(
              performAction(Seq("disposalFees" -> "1,000")),
              controllers.returns.disposaldetails.routes.DisposalDetailsController
                .checkYourAnswers()
            )
        }
      }

      "accept submitted values with pound signs" in {
        forAll(acceptedUserTypeGen, disposalMethodGen, individualUserTypeGen) {
          (
            userType: UserType,
            disposalMethod: DisposalMethod,
            individualUserType: IndividualUserType
          ) =>
            val currentAnswers =
              sample[CompleteDisposalDetailsAnswers]
                .copy(disposalFees = AmountInPence.fromPounds(1d))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithDisposalDetailsAnswers(
                  currentAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )._1
              )
            }

            checkIsRedirect(
              performAction(Seq("disposalFees" -> "£1")),
              controllers.returns.disposaldetails.routes.DisposalDetailsController
                .checkYourAnswers()
            )
        }
      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      val completeAnswers = CompleteDisposalDetailsAnswers(
        sample[ShareOfProperty],
        sample[AmountInPence],
        sample[AmountInPence]
      )

      val allQuestionsAnswered = IncompleteDisposalDetailsAnswers(
        Some(completeAnswers.shareOfProperty),
        Some(completeAnswers.disposalPrice),
        Some(completeAnswers.disposalFees)
      )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswers(),
        mockUUIDGenerator
      )

      "skip the how much did you own page and redirect to the disposal price page" when {

        "user is non-uk resident and selects indirect disposal" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithIndirectDisposalDetailsAnswers(
                    None,
                    disposalMethod,
                    userType,
                    Some(individualUserType),
                    isAmend = false
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.DisposalDetailsController.whatWasDisposalPrice()
              )
          }
        }

      }

      "redirect to the how much did you own page" when {

        "there are no disposal details answers in session" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    None,
                    disposalMethod,
                    userType,
                    Some(individualUserType),
                    isAmend = false
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.DisposalDetailsController.howMuchDidYouOwn()
              )
          }
        }

        "there are disposal details in session but no answer for the property share question" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    allQuestionsAnswered.copy(shareOfProperty = None),
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.DisposalDetailsController.howMuchDidYouOwn()
              )
          }
        }

      }

      "redirect to the disposal price page" when {

        "the user has not answered that question" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    allQuestionsAnswered.copy(disposalPrice = None),
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.DisposalDetailsController.whatWasDisposalPrice()
              )
          }
        }

      }

      "redirect to the disposal fees page" when {

        "the user has not answered that question" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    allQuestionsAnswered.copy(disposalFees = None),
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.DisposalDetailsController.whatWereDisposalFees()
              )
          }
        }

      }

      "show an error page" when {

        "there is an error updating the draft return" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val (session, journey, draftReturn) =
                sessionWithDisposalDetailsAnswers(
                  allQuestionsAnswered,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )

              val newDraftReturn = draftReturn.copy(
                disposalDetailsAnswers = Some(completeAnswers)
              )

              val newJourney = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
          }
        }

        "there is an error updating the session" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              individualUserType: IndividualUserType
            ) =>
              val (session, journey, draftReturn) =
                sessionWithDisposalDetailsAnswers(
                  allQuestionsAnswered,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )

              val newDraftReturn = draftReturn.copy(
                disposalDetailsAnswers = Some(completeAnswers)
              )

              val newJourney = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(Right(()))
                mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
          }
        }

      }

      "show the check your answers page" when {

        def testIsCheckYourAnswers(
          result: Future[Result],
          completeDisposalDetailsAnswers: CompleteDisposalDetailsAnswers,
          userType: UserType,
          individualUserType: IndividualUserType
        ): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey("returns.disposal-details.cya.title"),
            { doc =>
              validateDisposalDetailsCheckYourAnswersPage(
                completeDisposalDetailsAnswers,
                doc
              )
              val userKey =
                userMessageKey(
                  individualUserType,
                  userType
                ) // TODO - change when the CYA changes go in
              doc
                .select("#propertyShare-question")
                .text() shouldBe messageFromMessageKey(
                s"shareOfProperty$userKey.title"
              )
            }
          )

        "the user has already answered all of the questions" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            completeDisposalDetailsAnswersGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              completeAnswers: CompleteDisposalDetailsAnswers,
              individualUserType: IndividualUserType
            ) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithDisposalDetailsAnswers(
                    completeAnswers,
                    disposalMethod,
                    userType,
                    Some(individualUserType)
                  )._1
                )
              }

              testIsCheckYourAnswers(
                performAction(),
                completeAnswers,
                userType,
                individualUserType
              )
          }
        }

        "the user has just answered all of the questions" in {
          forAll(
            acceptedUserTypeGen,
            disposalMethodGen,
            completeDisposalDetailsAnswersGen,
            individualUserTypeGen
          ) {
            (
              userType: UserType,
              disposalMethod: DisposalMethod,
              completeAnswers: CompleteDisposalDetailsAnswers,
              individualUserType: IndividualUserType
            ) =>
              val incompleteAnswers               = IncompleteDisposalDetailsAnswers(
                Some(completeAnswers.shareOfProperty),
                Some(completeAnswers.disposalPrice),
                Some(completeAnswers.disposalFees)
              )
              val (session, journey, draftReturn) =
                sessionWithDisposalDetailsAnswers(
                  incompleteAnswers,
                  disposalMethod,
                  userType,
                  Some(individualUserType)
                )

              val newDraftReturn = draftReturn.copy(
                disposalDetailsAnswers = Some(completeAnswers)
              )
              val newJourney     = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(newJourney)(Right(()))
                mockStoreSession(session.copy(journeyStatus = Some(newJourney)))(Right(()))
              }

              testIsCheckYourAnswers(
                performAction(),
                completeAnswers,
                userType,
                individualUserType
              )
          }
        }

      }

    }

    "handling submits on the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswersSubmit(),
        mockUUIDGenerator
      )

      "redirect to the task list page" in {
        forAll(acceptedUserTypeGen, disposalMethodGen, individualUserTypeGen) {
          (
            userType: UserType,
            disposalMethod: DisposalMethod,
            individualUserType: IndividualUserType
          ) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithDisposalDetailsAnswers(
                  sample[CompleteDisposalDetailsAnswers],
                  disposalMethod,
                  userType,
                  Some(individualUserType)
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

  def noDisposalMethodBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to start endpoint" when {

      "there is no disposal method" in {
        val draftReturn = sample[DraftSingleDisposalReturn].copy(
          triageAnswers = sample[IncompleteSingleDisposalTriageAnswers]
            .copy(disposalMethod = None)
        )

        val fillingOutReturn =
          sample[FillingOutReturn].copy(draftReturn = draftReturn)
        val sessionData      =
          SessionData.empty.copy(journeyStatus = Some(fillingOutReturn))

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(sessionData)
        }

        checkIsRedirect(
          performAction(),
          controllers.routes.StartController.start()
        )
      }
    }

  def noPropertyShareBehaviour(performAction: () => Future[Result]): Unit =
    "redirect to the what was your share page" when {

      "there is no property share" in {
        for {
          individualUserType <- List(Self, PersonalRepresentative, Capacitor)
          disposalMethod     <- List(DisposalMethod.Sold, DisposalMethod.Gifted, DisposalMethod.Other)
          userType           <- List(UserType.Agent, UserType.Individual, UserType.Organisation)
        } yield {
          val draftReturn = sample[DraftSingleDisposalReturn].copy(
            triageAnswers = sample[CompleteSingleDisposalTriageAnswers],
            disposalDetailsAnswers = Some(
              IncompleteDisposalDetailsAnswers(
                None,
                Some(sample[AmountInPence]),
                Some(sample[AmountInPence])
              )
            )
          )

          val (journey, _) =
            fillingOutReturn(disposalMethod, userType, Some(individualUserType), isAmend = false)
          val sessionData  = SessionData.empty.copy(journeyStatus = Some(journey.copy(draftReturn = draftReturn)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          checkIsRedirect(performAction(), routes.DisposalDetailsController.howMuchDidYouOwn())
        }
      }
    }

}

object DisposalDetailsControllerSpec extends Matchers {

  def userMessageKey(
    individualUserType: IndividualUserType,
    userType: UserType
  ): String =
    (individualUserType, userType) match {
      case (Capacitor, _)                             => ".capacitor"
      case (PersonalRepresentative, _)                => ".personalRep"
      case (PersonalRepresentativeInPeriodOfAdmin, _) => ".personalRepInPeriodOfAdmin"
      case (_, UserType.Individual)                   => ""
      case (_, UserType.Organisation)                 => ".trust"
      case (_, UserType.Agent)                        => ".agent"
      case other                                      => sys.error(s"User type '$other' not handled")
    }

  def validateDisposalDetailsCheckYourAnswersPage(
    disposalDetailsAnswers: CompleteDisposalDetailsAnswers,
    doc: Document,
    isIndirectDisposal: Boolean = false
  ): Unit = {
    if (isIndirectDisposal) {
      doc
        .select("#propertyShare-answer")
        .text()
        .stripSuffix("%") shouldBe ""
    } else {
      doc
        .select("#propertyShare-answer")
        .text()
        .stripSuffix("%") shouldBe disposalDetailsAnswers.shareOfProperty.percentageValue.toString()
    }

    doc
      .select("#disposalPrice-answer")
      .text() shouldBe formatAmountOfMoneyWithPoundSign(
      disposalDetailsAnswers.disposalPrice.inPounds()
    )

    doc
      .select("#disposalFees-answer")
      .text() shouldBe formatAmountOfMoneyWithPoundSign(
      disposalDetailsAnswers.disposalFees.inPounds()
    )
  }

  def expectedTitles(
    disposalMethod: DisposalMethod,
    userType: UserType,
    individualUserType: IndividualUserType
  ): (String, String) = {
    val userKey = userMessageKey(individualUserType, userType)

    val disposalMethodKey = disposalMethod match {
      case DisposalMethod.Gifted => ".Gifted"
      case _                     => ".SoldOther"
    }

    (
      s"disposalPrice$userKey$disposalMethodKey.title",
      s"disposalFees$userKey.title"
    )
  }
}
