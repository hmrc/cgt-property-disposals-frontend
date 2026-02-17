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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails

import org.jsoup.nodes.Document
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.RebasingCutoffDates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.RebasingCutoffDates.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.DateErrorScenarios.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.AcquisitionDetailsControllerSpec.validateAcquisitionDetailsCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.Agent
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AcquisitionDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DisposalDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TaxYearGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.UserTypeGen.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionMethod.Inherited
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{IndirectDisposal, NonResidential, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ShareOfProperty.Half
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, models}

import java.time.LocalDate
import scala.concurrent.Future

class AcquisitionDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  private lazy val controller = instanceOf[AcquisitionDetailsController]

  private val mockRebasingUtil = new RebasingEligibilityUtil()

  private val mockUUIDGenerator = mock[UUIDGenerator]

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[RebasingEligibilityUtil].toInstance(mockRebasingUtil),
      bind[UUIDGenerator].toInstance(mockUUIDGenerator)
    )

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

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

  def assetTypeMessageKey(assetType: AssetType): String =
    if (assetType === IndirectDisposal) ".indirect" else ""

  private def expectedSubmitText(isAmend: Boolean) =
    messageFromMessageKey(if (isAmend) "button.continue" else "button.saveAndContinue")
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

  private def redirectToStartBehaviour(performAction: () => Future[Result]): Unit = {
    def hasValidPeriodOfAdminState(
      triageAnswers: SingleDisposalTriageAnswers,
      representeeAnswers: Option[RepresenteeAnswers]
    ) =
      triageAnswers.fold(_.individualUserType, _.individualUserType).forall {
        case PersonalRepresentativeInPeriodOfAdmin =>
          representeeAnswers.exists(_.fold(_ => false, _.dateOfDeath.isDefined))

        case _ =>
          true
      }

    redirectToStartWhenInvalidJourney(
      () => performAction(),
      {
        case FillingOutReturn(_, _, _, d: DraftSingleDisposalReturn, _, _)
            if hasValidPeriodOfAdminState(d.triageAnswers, d.representeeAnswers) =>
          true
        case FillingOutReturn(_, _, _, d: DraftSingleIndirectDisposalReturn, _, _)
            if hasValidPeriodOfAdminState(d.triageAnswers, d.representeeAnswers) =>
          true

        case _: StartingToAmendReturn =>
          true

        case _ => false
      }
    )

    "redirect to the start endpoint" when {

      "there are no answers for the section and the user type is period of admin and" when {

        "there are no representee answers" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                None,
                Some(AssetType.Residential),
                wasUkResident = Some(true),
                UserType.Individual,
                IndividualUserType.PersonalRepresentativeInPeriodOfAdmin,
                Some(sample[DisposalDate]),
                None,
                isAmend = false
              )._1
            )
          }

          checkIsRedirect(performAction(), controllers.routes.StartController.start())
        }

        "the representee answers are incomplete" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                None,
                Some(AssetType.Residential),
                wasUkResident = Some(true),
                UserType.Individual,
                IndividualUserType.PersonalRepresentativeInPeriodOfAdmin,
                Some(sample[DisposalDate]),
                Some(sample[IncompleteRepresenteeAnswers]),
                isAmend = false
              )._1
            )
          }

          checkIsRedirect(performAction(), controllers.routes.StartController.start())
        }

        "the representee answers are complete but there is no date of death" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                None,
                Some(AssetType.Residential),
                wasUkResident = Some(true),
                UserType.Individual,
                IndividualUserType.PersonalRepresentativeInPeriodOfAdmin,
                Some(sample[DisposalDate]),
                Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = None)),
                isAmend = false
              )._1
            )
          }

          checkIsRedirect(performAction(), controllers.routes.StartController.start())
        }

      }

    }

  }

  def sessionWithState(
    answers: AcquisitionDetailsAnswers,
    assetType: AssetType,
    wasUkResident: Boolean,
    userType: UserType,
    individualUserType: IndividualUserType,
    disposalDate: DisposalDate = sample[DisposalDate],
    representeeAnswers: Option[RepresenteeAnswers] = Some(
      sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath]))
    ),
    isAmend: Boolean = false
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) =
    sessionWithState(
      Some(answers),
      Some(assetType),
      Some(wasUkResident),
      userType,
      individualUserType,
      Some(disposalDate),
      representeeAnswers,
      isAmend
    )

  def sessionWithState(
    answers: Option[AcquisitionDetailsAnswers],
    assetType: Option[AssetType],
    wasUkResident: Option[Boolean],
    userType: UserType,
    individualUserType: IndividualUserType,
    disposalDate: Option[DisposalDate],
    representeeAnswers: Option[RepresenteeAnswers],
    isAmend: Boolean
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {

    val draftReturn = sample[DraftSingleDisposalReturn].copy(
      triageAnswers = sample[IncompleteSingleDisposalTriageAnswers].copy(
        assetType = assetType,
        wasAUKResident = wasUkResident,
        disposalDate = disposalDate,
        individualUserType = Some(individualUserType)
      ),
      acquisitionDetailsAnswers = answers,
      representeeAnswers = representeeAnswers,
      disposalDetailsAnswers = None
    )

    val journey = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      agentReferenceNumber = setAgentReferenceNumber(userType),
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

  def sessionWithStateForAgent(
    answers: AcquisitionDetailsAnswers,
    assetType: Option[AssetType],
    wasUkResident: Option[Boolean]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn, UserType) = {

    val draftReturn = sample[DraftSingleDisposalReturn].copy(
      triageAnswers = sample[IncompleteSingleDisposalTriageAnswers].copy(
        Some(IndividualUserType.Self),
        assetType = assetType,
        wasAUKResident = wasUkResident
      ),
      acquisitionDetailsAnswers = Some(answers),
      disposalDetailsAnswers = Some(
        sample[DisposalDetailsAnswers.IncompleteDisposalDetailsAnswers]
          .copy(shareOfProperty = Some(Half), disposalPrice = None, disposalFees = None)
      )
    )

    val journey = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      agentReferenceNumber = setAgentReferenceNumber(Agent),
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(Agent)
      )
    )

    val sessionData = SessionData.empty.copy(
      userType = Some(Agent),
      journeyStatus = Some(journey)
    )
    (sessionData, journey, draftReturn, UserType.Agent)
  }

  def sessionWithSingleIndirectDisposalState(
    answers: Option[AcquisitionDetailsAnswers],
    wasUkResident: Option[Boolean],
    userType: UserType,
    individualUserType: IndividualUserType,
    disposalDate: Option[DisposalDate],
    representeeAnswers: Option[RepresenteeAnswers]
  ): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) = {

    val draftReturn = sample[DraftSingleIndirectDisposalReturn].copy(
      triageAnswers = sample[IncompleteSingleDisposalTriageAnswers].copy(
        assetType = Some(IndirectDisposal),
        wasAUKResident = wasUkResident,
        disposalDate = disposalDate,
        individualUserType = Some(individualUserType)
      ),
      disposalDetailsAnswers = Some(sample[CompleteDisposalDetailsAnswers]),
      acquisitionDetailsAnswers = answers,
      representeeAnswers = representeeAnswers
    )

    val journey = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      agentReferenceNumber = setAgentReferenceNumber(userType),
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      ),
      amendReturnData = None
    )

    val sessionData = SessionData.empty.copy(
      userType = Some(userType),
      journeyStatus = Some(journey)
    )

    (sessionData, journey, draftReturn)
  }

  private def commonUpdateDraftReturn(
    d: DraftSingleDisposalReturn,
    newAnswers: AcquisitionDetailsAnswers,
    fillingOutReturn: FillingOutReturn
  ) =
    d.copy(
      acquisitionDetailsAnswers = Some(newAnswers),
      initialGainOrLoss = None,
      reliefDetailsAnswers = d.reliefDetailsAnswers.map(_.unsetPrrAndLettingRelief(d.triageAnswers.isPeriodOfAdmin)),
      exemptionAndLossesAnswers =
        if (fillingOutReturn.isFurtherOrAmendReturn.contains(true)) None else d.exemptionAndLossesAnswers,
      yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails()),
      gainOrLossAfterReliefs = None
    )

  private def commonUpdateDraftReturn(
    d: DraftSingleIndirectDisposalReturn,
    newAnswers: AcquisitionDetailsAnswers,
    fillingOutReturn: FillingOutReturn
  ) =
    d.copy(
      acquisitionDetailsAnswers = Some(newAnswers),
      exemptionAndLossesAnswers =
        if (fillingOutReturn.isFurtherOrAmendReturn.contains(true)) None else d.exemptionAndLossesAnswers,
      yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails()),
      gainOrLossAfterReliefs = None
    )

  val acceptedUserTypeGen: Gen[UserType] = userTypeGen.filter {
    case UserType.Agent | UserType.Organisation | UserType.Individual => true
    case _                                                            => false
  }

  val acceptedIndividualUserTypeGen: Gen[IndividualUserType] =
    individualUserTypeGen.filter {
      case Self | Capacitor | PersonalRepresentative => true
      case _                                         => false
    }

  val acceptedAssetTypeGenerator: Gen[AssetType] = assetTypeGen.filter {
    case IndirectDisposal | Residential | NonResidential => true
    case _                                               => false
  }

  "AcquisitionDetailsController" when {

    "handling requests to display the acquisition method page" must {

      def performAction(): Future[Result] =
        controller.acquisitionMethod()(FakeRequest())

      val key = "acquisitionMethod"

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.acquisitionMethod(),
        mockUUIDGenerator
      )

      "display the page" when {

        "the user has not completed the acquisition details section of the return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              List(Some(IncompleteAcquisitionDetailsAnswers.empty), None)
                .foreach { answers =>
                  withClue(s"For answers $answers: ") {
                    inSequence {
                      mockAuthWithNoRetrievals()
                      mockGetSession(
                        sessionWithState(
                          answers,
                          Some(assetType),
                          None,
                          userType,
                          individualUserType,
                          Some(sample[DisposalDate]),
                          Some(
                            sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath]))
                          ),
                          isAmend = false
                        )._1
                      )
                    }

                    val userMsgKey   =
                      userMessageKey(individualUserType, userType)
                    val assetTypeKey = assetTypeMessageKey(assetType)

                    checkPageIsDisplayed(
                      performAction(),
                      messageFromMessageKey(s"$key$userMsgKey$assetTypeKey.title"),
                      { doc =>
                        doc
                          .select("#back, .govuk-back-link")
                          .attr("href") shouldBe controllers.returns.routes.TaskListController
                          .taskList()
                          .url
                        doc
                          .select("#content > article > form, #main-content form")
                          .attr(
                            "action"
                          )             shouldBe routes.AcquisitionDetailsController
                          .acquisitionMethodSubmit()
                          .url
                      }
                    )
                  }
                }
          }
        }

        "the user has already completed the acquisition details section of the return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              List(Some(IncompleteAcquisitionDetailsAnswers.empty), None)
                .foreach { answers =>
                  val assetType = sample[AssetType]
                  val isAmend   = sample[Boolean]
                  withClue(s"For answers $answers: ") {
                    inSequence {
                      mockAuthWithNoRetrievals()
                      mockGetSession(
                        sessionWithState(
                          sample[CompleteAcquisitionDetailsAnswers],
                          assetType,
                          sample[Boolean],
                          userType,
                          individualUserType,
                          sample[DisposalDate],
                          isAmend = isAmend
                        )._1
                      )
                    }

                    val userKey      = userMessageKey(individualUserType, userType)
                    val assetTypeKey = assetTypeMessageKey(assetType)
                    checkPageIsDisplayed(
                      performAction(),
                      messageFromMessageKey(s"$key$userKey$assetTypeKey.title"),
                      { doc =>
                        doc
                          .select("#back, .govuk-back-link")
                          .attr(
                            "href"
                          )                                shouldBe routes.AcquisitionDetailsController
                          .checkYourAnswers()
                          .url
                        doc
                          .select("#content > article > form, #main-content form")
                          .attr(
                            "action"
                          )                                shouldBe routes.AcquisitionDetailsController
                          .acquisitionMethodSubmit()
                          .url
                        doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                      }
                    )
                  }
                }
          }
        }

      }

    }

    "handling submitted answers to the acquisition method page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionMethodSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.acquisitionMethodSubmit(),
        mockUUIDGenerator
      )

      val key      = "acquisitionMethod"
      val otherKey = "otherAcquisitionMethod"

      "show a form error" when {

        def test(data: (String, String)*)(
          expectedErrorMessageKey: String
        )(
          userType: UserType,
          individualUserType: IndividualUserType,
          assetType: AssetType,
          userKey: String
        ): Unit =
          testFormError(data*)(userType, individualUserType, assetType)(
            expectedErrorMessageKey
          )(
            s"$key$userKey.title"
          )(performAction)

        "nothing is submitted" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val userKey      = userMessageKey(individualUserType, userType)
              val assetTypeKey = assetTypeMessageKey(assetType)
              test()(s"$key$userKey$assetTypeKey.error.required")(
                userType,
                individualUserType,
                assetType,
                userKey + assetTypeKey
              )
          }
        }

        "an unknown value is submitted" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val assetTypeKey = assetTypeMessageKey(assetType)
              val userKey      = userMessageKey(individualUserType, userType)
              test("acquisitionMethod" -> "4")(s"$key$userKey$assetTypeKey.error.required")(
                userType,
                individualUserType,
                assetType,
                userKey + assetTypeKey
              )
          }
        }

        "other is selected with a value" that {

          "the user enters acquisition method that doesn't exist" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
              (userType: UserType, individualUserType: IndividualUserType, assetType) =>
                val assetTypeKey = assetTypeMessageKey(assetType)
                val userKey      = userMessageKey(individualUserType, userType)
                test(key -> "3")(s"$otherKey$userKey$assetTypeKey.error.required")(
                  userType,
                  individualUserType,
                  assetType,
                  userKey + assetTypeKey
                )
            }
          }

          "the user enters acquisition method that is empty" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
              (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
                val assetTypeKey = assetTypeMessageKey(assetType)
                val userKey      = userMessageKey(individualUserType, userType)
                test(key -> "3", otherKey -> "")(
                  s"$otherKey$userKey$assetTypeKey.error.required"
                )(
                  userType,
                  individualUserType,
                  assetType,
                  userKey + assetTypeKey
                )
            }
          }

          "the user enters acquisition method that contains invalid characters" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
              (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
                val assetTypeKey = assetTypeMessageKey(assetType)
                val userKey      = userMessageKey(individualUserType, userType)
                test(key -> "3", otherKey -> "1,234")(
                  s"$otherKey$userKey$assetTypeKey.error.invalid"
                )(userType, individualUserType, assetType, userKey + assetTypeKey)
            }
          }

          "the  user enters acquisition method that is too long" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
              (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
                val assetTypeKey = assetTypeMessageKey(assetType)
                val userKey      = userMessageKey(individualUserType, userType)
                test(key -> "3", otherKey -> ("a" * 36))(
                  s"$otherKey$userKey$assetTypeKey.error.tooLong"
                )(userType, individualUserType, assetType, userKey + assetTypeKey)
            }
          }

        }

      }

      "show an error page" when {

        val (method, methodValue) = AcquisitionMethod.Bought -> 0

        def getSessionDataJourneyAndDraftReturn(
          userType: UserType,
          individualUserType: IndividualUserType,
          assetType: AssetType,
          isAmend: Boolean
        ): (SessionData, FillingOutReturn, DraftReturn) = {
          val (session, journey, draftReturn) = sessionWithState(
            None,
            Some(assetType),
            None,
            userType,
            individualUserType,
            None,
            Some(
              sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath]))
            ),
            isAmend
          )
          val updatedDraftReturn              = commonUpdateDraftReturn(
            draftReturn,
            IncompleteAcquisitionDetailsAnswers.empty.copy(
              acquisitionMethod = Some(method)
            ),
            journey
          )
          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(
                  userType,
                  individualUserType,
                  assetType,
                  isAmend = true
                )
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  journey.copy(draftReturn = updatedDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends
                )(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(key -> methodValue.toString)
              )
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(
                  userType,
                  individualUserType,
                  assetType,
                  isAmend = false
                )

              val updatedJourney = journey.copy(draftReturn = updatedDraftReturn)
              val updatedSession = session.copy(journeyStatus = Some(updatedJourney))
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(updatedSession)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(key -> methodValue.toString)
              )
          }
        }

      }

      "redirect to the check your answers page" when {

        "the acquisition details journey is incomplete and" when {

          def test(
            data: (String, String)*
          )(method: AcquisitionMethod)(
            userType: UserType,
            individualUserType: IndividualUserType,
            assetType: AssetType
          ): Unit = {
            val (session, journey, draftReturn) = sessionWithState(
              None,
              Some(assetType),
              None,
              userType,
              individualUserType,
              None,
              Some(sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath]))),
              isAmend = false
            )
            val updatedDraftReturn              = commonUpdateDraftReturn(
              draftReturn,
              IncompleteAcquisitionDetailsAnswers.empty
                .copy(acquisitionMethod = Some(method)),
              journey
            )
            val updatedJourney                  =
              journey.copy(draftReturn = updatedDraftReturn)
            val updatedSession                  = session.copy(journeyStatus = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(updatedJourney)(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction(data*),
              routes.AcquisitionDetailsController.checkYourAnswers()
            )
          }

          "the user selects bought it" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
              (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
                test(key -> "0")(AcquisitionMethod.Bought)(
                  userType,
                  individualUserType,
                  assetType
                )
            }
          }

          "the user selects inherited it" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(key -> "1")(AcquisitionMethod.Inherited)(
                  userType,
                  individualUserType,
                  Residential
                )
            }
          }

          "the user selects was gifted it" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test("acquisitionMethod" -> "2")(AcquisitionMethod.Gifted)(
                  userType,
                  individualUserType,
                  Residential
                )
            }
          }

          "the user selects other" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(key -> "3", otherKey -> "things")(
                  AcquisitionMethod.Other("things")
                )(userType, individualUserType, Residential)
            }
          }

          "the user hasn't started this section before and they are on an indirect disposal journey and is not period of admin" in {
            val (session, journey, draftReturn) = sessionWithSingleIndirectDisposalState(
              None,
              None,
              UserType.Individual,
              IndividualUserType.Self,
              None,
              None
            )
            val updatedDraftReturn              = commonUpdateDraftReturn(
              draftReturn,
              IncompleteAcquisitionDetailsAnswers.empty
                .copy(acquisitionMethod = Some(AcquisitionMethod.Gifted), improvementCosts = Some(AmountInPence.zero)),
              journey
            )
            val updatedJourney                  =
              journey.copy(draftReturn = updatedDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends
            val updatedSession                  = session.copy(journeyStatus = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(updatedJourney)(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction("acquisitionMethod" -> "2"),
              routes.AcquisitionDetailsController.checkYourAnswers()
            )
          }

        }

        "the acquisition details journey is complete and" when {

          def test(data: (String, String)*)(
            oldMethod: AcquisitionMethod,
            method: AcquisitionMethod
          )(
            userType: UserType,
            individualUserType: IndividualUserType
          ): Unit = {
            val answers                         = sample[CompleteAcquisitionDetailsAnswers]
              .copy(acquisitionMethod = oldMethod)
            val (session, journey, draftReturn) = sessionWithState(
              answers,
              sample[AssetType],
              sample[Boolean],
              userType,
              individualUserType
            )
            val updatedAnswers                  = IncompleteAcquisitionDetailsAnswers(
              Some(method),
              Some(answers.acquisitionDate),
              None,
              None,
              Some(answers.improvementCosts),
              None,
              None
            )

            val updatedDraftReturn =
              commonUpdateDraftReturn(draftReturn, updatedAnswers, journey)
            val updatedJourney     = journey.copy(draftReturn = updatedDraftReturn)
            val updatedSession     = session.copy(journeyStatus = Some(updatedJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(updatedJourney)(Right(()))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkIsRedirect(
              performAction(data*),
              routes.AcquisitionDetailsController.checkYourAnswers()
            )
          }

          "the user selects bought it" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(key -> "0")(
                  AcquisitionMethod.Inherited,
                  AcquisitionMethod.Bought
                )(userType, individualUserType)
            }
          }

          "the user selects inherited it" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(key -> "1")(
                  AcquisitionMethod.Bought,
                  AcquisitionMethod.Inherited
                )(userType, individualUserType)
            }
          }

          "the user selects was gifted it" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(key -> "2")(
                  AcquisitionMethod.Bought,
                  AcquisitionMethod.Gifted
                )(userType, individualUserType)
            }
          }

          "the user selects other" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(key -> "3", otherKey -> "things")(
                  AcquisitionMethod.Bought,
                  AcquisitionMethod.Other("things")
                )(userType, individualUserType)
            }
          }

        }

      }

      "not update the draft return or session" when {

        "the user has selected the same as before" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (incompleteAnswers, completeAnswers) =
                sample[IncompleteAcquisitionDetailsAnswers] -> sample[CompleteAcquisitionDetailsAnswers]
              List(
                Seq(key -> "0")                             -> incompleteAnswers.copy(acquisitionMethod = Some(AcquisitionMethod.Bought)),
                Seq(key -> "1")                             -> incompleteAnswers.copy(acquisitionMethod = Some(AcquisitionMethod.Inherited)),
                Seq(key -> "2")                             -> incompleteAnswers.copy(acquisitionMethod = Some(AcquisitionMethod.Gifted)),
                Seq(key -> "3", otherKey -> "other things") -> incompleteAnswers
                  .copy(acquisitionMethod = Some(AcquisitionMethod.Other("other things"))),
                Seq(key -> "0")                             -> completeAnswers.copy(acquisitionMethod = AcquisitionMethod.Bought),
                Seq(key -> "1")                             -> completeAnswers.copy(acquisitionMethod = AcquisitionMethod.Inherited),
                Seq(key -> "2")                             -> completeAnswers.copy(acquisitionMethod = AcquisitionMethod.Gifted),
                Seq(key -> "3", otherKey -> "other things") -> completeAnswers
                  .copy(acquisitionMethod = AcquisitionMethod.Other("other things"))
              ).foreach { case (data, answers) =>
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(
                    sessionWithState(
                      answers,
                      sample[AssetType],
                      sample[Boolean],
                      userType,
                      individualUserType
                    )._1
                  )
                }

                checkIsRedirect(
                  performAction(data*),
                  routes.AcquisitionDetailsController.checkYourAnswers()
                )
              }
          }
        }

      }

    }

    "handling requests to display the acquisition date page" must {

      def performAction(): Future[Result] =
        controller.acquisitionDate()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.acquisitionDate(),
        mockUUIDGenerator
      )

      val key = "acquisitionDate"

      "redirect to the task list page" when {

        "there is no disposal date in the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    Some(sample[CompleteAcquisitionDetailsAnswers]),
                    Some(sample[AssetType]),
                    Some(sample[Boolean]),
                    userType,
                    individualUserType,
                    None,
                    Some(
                      sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath]))
                    ),
                    isAmend = false
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

      "redirect to the acquisition method page" when {

        "that question has not yet been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers]
                      .copy(acquisitionMethod = None),
                    sample[AssetType],
                    sample[Boolean],
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.acquisitionMethod()
              )
          }
        }

      }

      "display the page" when {

        "the user has not yet completed the acquisition details section" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val isAmend = sample[Boolean]
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionMethod = Some(AcquisitionMethod.Bought)
                    ),
                    assetType,
                    sample[Boolean],
                    userType,
                    individualUserType,
                    isAmend = isAmend
                  )._1
                )
              }

              val assetTypeKey = assetTypeMessageKey(assetType)
              val userKey      = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey$assetTypeKey.title"),
                { doc =>
                  doc
                    .select("#back, .govuk-back-link")
                    .attr("href")                    shouldBe routes.AcquisitionDetailsController
                    .acquisitionMethod()
                    .url
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action")                  shouldBe routes.AcquisitionDetailsController
                    .acquisitionDateSubmit()
                    .url
                  doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                }
              )
          }
        }

        "the user has already completed the acquisition details section" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val isAmend = sample[Boolean]
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers],
                    assetType,
                    sample[Boolean],
                    userType,
                    individualUserType,
                    isAmend = isAmend
                  )._1
                )
              }

              val userKey      = userMessageKey(individualUserType, userType)
              val assetTypeKey = assetTypeMessageKey(assetType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey$assetTypeKey.title"),
                { doc =>
                  doc
                    .select("#back, .govuk-back-link")
                    .attr("href")                    shouldBe routes.AcquisitionDetailsController
                    .checkYourAnswers()
                    .url
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action")                  shouldBe routes.AcquisitionDetailsController
                    .acquisitionDateSubmit()
                    .url
                  doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                }
              )
          }
        }

      }

    }

    "handling submitted answers to the acquisition date page" must {

      val key = "acquisitionDate"

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionDateSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      def formData(date: LocalDate): List[(String, String)] =
        List(
          "acquisitionDate-day"   -> date.getDayOfMonth.toString,
          "acquisitionDate-month" -> date.getMonthValue.toString,
          "acquisitionDate-year"  -> date.getYear.toString
        )

      val disposalDate = DisposalDate(LocalDate.of(2020, 1, 1), sample[TaxYear])

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.acquisitionMethodSubmit(),
        mockUUIDGenerator
      )

      "redirect to the task list page" when {

        "there is no disposal date in the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    Some(sample[CompleteAcquisitionDetailsAnswers]),
                    Some(sample[AssetType]),
                    Some(sample[Boolean]),
                    userType,
                    individualUserType,
                    None,
                    Some(
                      sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath]))
                    ),
                    isAmend = false
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

      "show a form error" when {

        def test(
          data: (String, String)*
        )(
          userType: UserType,
          individualUserType: IndividualUserType,
          assetType: AssetType,
          userKey: String
        )(expectedErrorKey: String): Unit =
          testFormError(data*)(userType, individualUserType, assetType)(
            expectedErrorKey
          )(s"$key$userKey.title")(
            performAction,
            Some(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers],
                assetType,
                sample[Boolean],
                userType,
                individualUserType,
                disposalDate
              )._1
            )
          )

        "the user enters date that is invalid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val userKey      = userMessageKey(individualUserType, userType)
              val assetTypeKey = assetTypeMessageKey(assetType)
              dateErrorScenarios(key, userKey + assetTypeKey).foreach {
                case d @ DateErrorScenario(
                      dayString,
                      monthString,
                      yearString,
                      expectedErrorKey
                    ) =>
                  withClue(s"For $d: ") {
                    val formData =
                      List(
                        "acquisitionDate-day"   -> dayString,
                        "acquisitionDate-month" -> monthString,
                        "acquisitionDate-year"  -> yearString
                      ).collect { case (id, Some(input)) => id -> input }

                    test(formData*)(userType, individualUserType, assetType, userKey + assetTypeKey)(
                      expectedErrorKey
                    )
                  }
              }
          }
        }

        "the user enters date that is after the disposal date" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val assetTypeKey = assetTypeMessageKey(assetType)
              val userKey      = userMessageKey(individualUserType, userType)
              val tomorrow     = disposalDate.value.plusDays(1L)
              test(formData(tomorrow)*)(
                userType,
                individualUserType,
                assetType,
                userKey + assetTypeKey
              )(s"$key$userKey$assetTypeKey.error.tooFarInFuture")
          }
        }

        "the user enters date that is before 01-01-1900" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val assetTypeKey = assetTypeMessageKey(assetType)
              val userKey      = userMessageKey(individualUserType, userType)
              val before1900   = LocalDate.of(1800, 1, 1)

              test(formData(before1900)*)(
                userType,
                individualUserType,
                assetType,
                userKey + assetTypeKey
              )(s"$key$userKey$assetTypeKey.error.before1900")
          }
        }

      }

      "show an error page" when {

        val acquisitionDate = AcquisitionDate(disposalDate.value)

        def getSessionDataJourneyAndDraftReturn(
          userType: UserType,
          individualUserType: IndividualUserType
        ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {

          val answers = sample[CompleteAcquisitionDetailsAnswers].copy(
            acquisitionDate = AcquisitionDate(acquisitionDate.value.plusDays(1L))
          )

          val wasUkResident                   = sample[Boolean]
          val assetType                       = sample[AssetType]
          val (session, journey, draftReturn) =
            sessionWithState(
              answers,
              assetType,
              wasUkResident,
              userType,
              individualUserType,
              disposalDate
            )
          val improvementCosts                = if (assetType === IndirectDisposal) Some(answers.improvementCosts) else None
          val newAnswers                      =
            IncompleteAcquisitionDetailsAnswers(
              Some(answers.acquisitionMethod),
              Some(acquisitionDate),
              None,
              None,
              improvementCosts,
              None,
              None
            )

          val updatedDraftReturn =
            commonUpdateDraftReturn(draftReturn, newAnswers, journey)

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(
                  userType,
                  individualUserType
                )
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(journey.copy(draftReturn = updatedDraftReturn))(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(formData(acquisitionDate.value)*)
              )
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(
                  userType,
                  individualUserType
                )
              val updatedJourney                         = journey.copy(draftReturn = updatedDraftReturn)
              val updatedSession                         = session.copy(journeyStatus = Some(updatedJourney))
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(updatedSession)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(formData(acquisitionDate.value)*)
              )
          }
        }

      }

      "update the session correctly and redirect to the cya page" when {
        def test(
          assetType: AssetType,
          wasUkResident: Boolean,
          submittedAcquisitionDate: LocalDate,
          oldAnswers: AcquisitionDetailsAnswers,
          newAnswers: AcquisitionDetailsAnswers,
          userType: UserType,
          individualUserType: IndividualUserType,
          isAmend: Boolean
        ): Unit = {
          val (session, journey, draftReturn) = sessionWithState(
            oldAnswers,
            assetType,
            wasUkResident,
            userType,
            individualUserType,
            disposalDate,
            isAmend = isAmend
          )
          val updatedNewAnswers               =
            if (assetType === IndirectDisposal) {
              newAnswers.fold(
                _.copy(improvementCosts = oldAnswers.fold(_.improvementCosts, e => Some(e.improvementCosts))),
                _.copy(improvementCosts =
                  oldAnswers.fold(_.improvementCosts.getOrElse(AmountInPence.zero), _.improvementCosts)
                )
              )
            } else {
              newAnswers
            }

          val updatedDraftReturn =
            commonUpdateDraftReturn(draftReturn, updatedNewAnswers, journey)
          val updatedJourney     =
            journey.copy(draftReturn = updatedDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends
          val updatedSession     = session.copy(journeyStatus = Some(updatedJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(updatedSession)(Right(()))

          }

          checkIsRedirect(
            performAction(formData(submittedAcquisitionDate)*),
            routes.AcquisitionDetailsController.checkYourAnswers()
          )
        }

        "the user has not answered this question before" in {
          val date       = disposalDate.value
          val oldAnswers = IncompleteAcquisitionDetailsAnswers.empty.copy(
            acquisitionMethod = Some(AcquisitionMethod.Bought)
          )
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              test(
                sample[AssetType],
                sample[Boolean],
                date,
                oldAnswers,
                oldAnswers.copy(acquisitionDate = Some(AcquisitionDate(date))),
                userType,
                individualUserType,
                isAmend = true
              )
          }
        }

        "the user has completed this section before" in {
          val date       = disposalDate.value
          val oldAnswers = sample[CompleteAcquisitionDetailsAnswers].copy(
            acquisitionDate = AcquisitionDate(date.minusDays(1L))
          )
          val assetType  = sample[AssetType]
          val newAnswers = IncompleteAcquisitionDetailsAnswers(
            Some(oldAnswers.acquisitionMethod),
            Some(AcquisitionDate(date)),
            None,
            None,
            None,
            None,
            None
          )
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              test(
                assetType,
                sample[Boolean],
                date,
                oldAnswers,
                newAnswers,
                userType,
                individualUserType,
                isAmend = true
              )
          }
        }

      }

    }

    "handling requests to display the period of admin market value page" must {

      def performAction(): Future[Result] =
        controller.periodOfAdminMarketValue()(FakeRequest())

      val dateOfDeath = DateOfDeath(LocalDate.ofEpochDay(0L))

      val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(dateOfDeath))

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.periodOfAdminMarketValue(),
        mockUUIDGenerator
      )

      "redirect to the check your answers page" when {

        "the user is not period of admin" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = Some(
                    AcquisitionDate(
                      nonUkResidentsResidentialProperty.minusDays(2)
                    )
                  ),
                  acquisitionPrice = None
                ),
                AssetType.Residential,
                wasUkResident = false,
                UserType.Individual,
                PersonalRepresentative
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.checkYourAnswers()
          )
        }

      }

      "display the page" when {

        def checkPage(
          assetType: AssetType,
          isAgent: Boolean
        ): Unit = {
          val isAmend = sample[Boolean]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = Some(AcquisitionDate(dateOfDeath.value)),
                  acquisitionMethod = Some(AcquisitionMethod.Other("period of admin"))
                ),
                assetType,
                wasUkResident = true,
                if (isAgent) UserType.Agent else UserType.Individual,
                PersonalRepresentativeInPeriodOfAdmin,
                representeeAnswers = Some(representeeAnswers),
                isAmend = isAmend
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              s"periodOfAdminMarketValue${assetTypeMessageKey(assetType)}.title",
              TimeUtils.govDisplayFormat(dateOfDeath.value)
            ),
            { doc =>
              doc
                .select("#back, .govuk-back-link")
                .attr("href")   shouldBe controllers.returns.routes.TaskListController
                .taskList()
                .url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action") shouldBe routes.AcquisitionDetailsController
                .periodOfAdminMarketValueSubmit()
                .url

              val expectedHelpTextKey =
                if (isAgent) {
                  s"periodOfAdminMarketValue.agent${assetTypeMessageKey(assetType)}.helpText"
                } else {
                  s"periodOfAdminMarketValue${assetTypeMessageKey(assetType)}.helpText"
                }

              doc
                .select("#periodOfAdminMarketValue-hint")
                .text()                          shouldBe messageFromMessageKey(expectedHelpTextKey)
              doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)

            }
          )
        }

        "the user is a period of admin user and" when {

          "it is not an indirect disposal and it is not an agent" in {
            checkPage(AssetType.Residential, isAgent = false)
          }

          "it is not an indirect disposal and it is an agent" in {
            checkPage(AssetType.Residential, isAgent = true)

          }

          "it is an indirect disposal and it is not an agent" in {
            checkPage(AssetType.IndirectDisposal, isAgent = false)
          }

          "it is an indirect disposal and it is an agent" in {
            checkPage(AssetType.IndirectDisposal, isAgent = true)

          }

        }

      }

    }

    "handling submitted period of admin market value answers" must {

      val key = "periodOfAdminMarketValue"

      def performAction(data: (String, String)*): Future[Result] =
        controller.periodOfAdminMarketValueSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      val dateOfDeath = DateOfDeath(LocalDate.ofEpochDay(0L))

      val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(dateOfDeath))

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.periodOfAdminMarketValueSubmit(),
        mockUUIDGenerator
      )

      "redirect to the check your answers page" when {

        "the user is not period of admin" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = Some(
                    AcquisitionDate(
                      nonUkResidentsResidentialProperty.minusDays(2)
                    )
                  ),
                  acquisitionPrice = None
                ),
                AssetType.Residential,
                wasUkResident = false,
                UserType.Individual,
                PersonalRepresentative
              )._1
            )
          }

          checkIsRedirect(
            performAction(),
            routes.AcquisitionDetailsController.checkYourAnswers()
          )
        }

      }

      "show a form error" when {

        def test(data: (String, String)*)(
          expectedErrorMessageKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = AcquisitionDate(dateOfDeath.value)
                ),
                AssetType.Residential,
                wasUkResident = true,
                UserType.Individual,
                PersonalRepresentativeInPeriodOfAdmin,
                representeeAnswers = Some(representeeAnswers)
              )._1
            )
          }

          val formattedDateOfDeath = TimeUtils.govDisplayFormat(dateOfDeath.value)
          checkPageIsDisplayed(
            performAction(data*),
            messageFromMessageKey(
              s"$key.title",
              formattedDateOfDeath
            ),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey,
                formattedDateOfDeath
              ),
            BAD_REQUEST
          )
        }

        "the amount of money is zero" in {
          test(key -> "0")(s"$key.error.tooSmall")
        }

        "nothing is submitted" in {
          test()(s"$key.error.required")
        }

        "the number submitted is larger than the maximum value" in {
          test(key -> (MoneyUtils.maxAmountOfPounds + 1).toString)(s"$key.error.tooLarge")
        }

        "invalid characters are submitted" in {
          test(key -> "abc")(s"$key.error.invalid")
        }

      }

      "show an error page" when {
        val price = 1.23d

        val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
          acquisitionDate = Some(AcquisitionDate(allResidents.minusDays(2))),
          acquisitionPrice = Some(sample[AmountInPence])
        )

        val (session, journey, draftReturn) = sessionWithState(
          answers,
          AssetType.Residential,
          wasUkResident = true,
          UserType.Agent,
          PersonalRepresentativeInPeriodOfAdmin,
          representeeAnswers = Some(representeeAnswers)
        )
        val updatedDraftReturn              = commonUpdateDraftReturn(
          draftReturn,
          answers.copy(
            acquisitionPrice = Some(AmountInPence(123L))
          ),
          journey
        )

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(journey.copy(draftReturn = updatedDraftReturn))(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(key -> price.toString)
          )
        }

        "there is an error updating the session" in {
          val updatedJourney = journey.copy(draftReturn = updatedDraftReturn)
          val updatedSession = session.copy(journeyStatus = Some(updatedJourney))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(key -> price.toString)
          )
        }

      }

      "redirect to the check you answers page" when {

        val scenarios = List(
          List(key -> "£1,234") -> AmountInPence(123400L),
          List(key -> "1")      -> AmountInPence(100L)
        )

        "the price submitted is valid and the journey was incomplete" in {
          scenarios.foreach { case (formData, expectedAmountInPence) =>
            withClue(
              s"For form data $formData and expected amount in pence $expectedAmountInPence: "
            ) {
              val (session, journey, draftReturn) = sessionWithState(
                None,
                Some(AssetType.Residential),
                Some(false),
                UserType.Individual,
                PersonalRepresentativeInPeriodOfAdmin,
                Some(sample[DisposalDate]),
                Some(representeeAnswers),
                isAmend = true
              )
              val updatedDraftReturn              = commonUpdateDraftReturn(
                draftReturn,
                IncompleteAcquisitionDetailsAnswers.empty.copy(
                  acquisitionMethod = Some(AcquisitionMethod.Other("period of admin")),
                  acquisitionDate = Some(AcquisitionDate(dateOfDeath.value)),
                  acquisitionPrice = Some(expectedAmountInPence)
                ),
                journey
              )

              val updatedJourney =
                journey.copy(draftReturn = updatedDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends
              val updatedSession = session.copy(journeyStatus = Some(updatedJourney))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(updatedSession)(Right(()))
              }

              checkIsRedirect(
                performAction(formData*),
                routes.AcquisitionDetailsController.checkYourAnswers()
              )
            }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          scenarios.foreach { case (formData, expectedAmountInPence) =>
            withClue(
              s"For form data $formData and expected amount in pence $expectedAmountInPence: "
            ) {
              val answers                         =
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = AcquisitionDate(dateOfDeath.value),
                  rebasedAcquisitionPrice = Some(AmountInPence(expectedAmountInPence.value + 1L))
                )
              val (session, journey, draftReturn) = sessionWithState(
                answers,
                AssetType.Residential,
                wasUkResident = true,
                UserType.Agent,
                PersonalRepresentativeInPeriodOfAdmin,
                representeeAnswers = Some(representeeAnswers)
              )
              val updatedDraftReturn              = commonUpdateDraftReturn(
                draftReturn,
                answers.copy(
                  acquisitionPrice = expectedAmountInPence
                ),
                journey
              )
              val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)
              val updatedSession                  = session.copy(journeyStatus = Some(updatedJourney))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(updatedSession)(Right(()))
              }

              checkIsRedirect(
                performAction(formData*),
                routes.AcquisitionDetailsController.checkYourAnswers()
              )
            }
          }
        }

      }

    }

    "handling requests to display the acquisition price page" must {

      def performAction(): Future[Result] =
        controller.acquisitionPrice()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.acquisitionPrice(),
        mockUUIDGenerator
      )

      behave like missingAcquisitionDateBehaviour(() => performAction())

      behave like missingAcquisitionMethodBehaviour(() => performAction())

      "display the page" when {

        def incompleteSection(
          userType: UserType,
          individualUserType: IndividualUserType,
          assetType: AssetType,
          userKey: String
        ): Unit =
          forAll { (acquisitionMethod: AcquisitionMethod) =>
            val isAmend         = sample[Boolean]
            val acquisitionDate = sample[AcquisitionDate]
            val answers         = sample[IncompleteAcquisitionDetailsAnswers].copy(
              acquisitionMethod = Some(acquisitionMethod),
              acquisitionDate = Some(acquisitionDate)
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithState(
                  answers,
                  assetType,
                  sample[Boolean],
                  userType,
                  individualUserType,
                  isAmend = isAmend
                )._1
              )
            }

            val pricePageTitle = acquisitionMethod match {
              case AcquisitionMethod.Bought =>
                messageFromMessageKey(s"acquisitionPriceBought$userKey.title")
              case _                        =>
                messageFromMessageKey(
                  s"acquisitionPriceNotBought$userKey.title",
                  TimeUtils.govDisplayFormat(acquisitionDate.value)
                )
            }

            checkPageIsDisplayed(
              performAction(),
              pricePageTitle,
              { doc =>
                doc
                  .select("#back, .govuk-back-link")
                  .attr("href")                    shouldBe routes.AcquisitionDetailsController
                  .acquisitionDate()
                  .url
                doc
                  .select("#content > article > form, #main-content form")
                  .attr("action")                  shouldBe routes.AcquisitionDetailsController
                  .acquisitionPriceSubmit()
                  .url
                doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
              }
            )
          }

        "the user has not yet completed the acquisition details section" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val userKey      = userMessageKey(individualUserType, userType)
              val assetTypeKey = assetTypeMessageKey(assetType)
              incompleteSection(userType, individualUserType, assetType, userKey + assetTypeKey)
          }
        }

        def sectionCompleted(
          userType: UserType,
          individualUserType: IndividualUserType,
          assetType: AssetType,
          userKey: String
        ): Unit = {
          val isAmend = sample[Boolean]
          val answers = sample[CompleteAcquisitionDetailsAnswers]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                answers,
                assetType,
                sample[Boolean],
                userType,
                individualUserType,
                isAmend = isAmend
              )._1
            )
          }

          val pricePageTitle = answers.acquisitionMethod match {
            case AcquisitionMethod.Bought =>
              messageFromMessageKey(s"acquisitionPriceBought$userKey.title")
            case _                        =>
              messageFromMessageKey(
                s"acquisitionPriceNotBought$userKey.title",
                TimeUtils.govDisplayFormat(answers.acquisitionDate.value)
              )
          }

          checkPageIsDisplayed(
            performAction(),
            pricePageTitle,
            { doc =>
              doc
                .select("#back, .govuk-back-link")
                .attr("href")                    shouldBe routes.AcquisitionDetailsController
                .checkYourAnswers()
                .url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                  shouldBe routes.AcquisitionDetailsController
                .acquisitionPriceSubmit()
                .url
              doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
            }
          )
        }

        "the user has already completed the acquisition details section" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val assetTypeKey = assetTypeMessageKey(assetType)
              val userKey      = userMessageKey(individualUserType, userType)
              sectionCompleted(userType, individualUserType, assetType, userKey + assetTypeKey)
          }
        }

      }

    }

    "handling submitted acquisition prices" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionPriceSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.acquisitionPriceSubmit(),
        mockUUIDGenerator
      )

      behave like missingAcquisitionDateBehaviour(() => performAction())

      behave like missingAcquisitionMethodBehaviour(() => performAction())

      "show a form error" when {

        def invalidPrice(
          userType: UserType,
          individualUserType: IndividualUserType,
          assetType: AssetType,
          userKey: String
        ): Unit =
          forAll { (answers: CompleteAcquisitionDetailsAnswers) =>
            val scenarioSession = sessionWithState(
              answers,
              assetType,
              sample[Boolean],
              userType,
              individualUserType
            )._1
            val assetTypeKey    = assetTypeMessageKey(assetType)
            val contextKey      = answers.acquisitionMethod match {
              case AcquisitionMethod.Bought => s"acquisitionPriceBought$userKey$assetTypeKey"
              case _                        => s"acquisitionPriceNotBought$userKey$assetTypeKey"
            }

            amountOfMoneyErrorScenarios(
              s"acquisitionPrice",
              errorContext = Some(contextKey)
            ).foreach { scenario =>
              withClue(s"For $scenario: ") {
                testFormError(scenario.formData*)(
                  userType,
                  individualUserType,
                  assetType
                )(scenario.expectedErrorMessageKey)(
                  s"$contextKey.title",
                  TimeUtils.govDisplayFormat(answers.acquisitionDate.value)
                )(performAction, Some(scenarioSession))
              }
            }
          }

        "the user provides an invalid data" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val userKey = userMessageKey(individualUserType, userType)
              invalidPrice(userType, individualUserType, assetType, userKey)
          }
        }

        def amountZero(
          userType: UserType,
          individualUserType: IndividualUserType,
          assetType: AssetType,
          userKey: String
        ): Unit =
          forAll { (answers: CompleteAcquisitionDetailsAnswers) =>
            val scenarioSession = sessionWithState(
              answers,
              assetType,
              sample[Boolean],
              userType,
              individualUserType
            )._1

            val contextKey = answers.acquisitionMethod match {
              case AcquisitionMethod.Bought => s"acquisitionPriceBought$userKey"
              case _                        => s"acquisitionPriceNotBought$userKey"
            }
            testFormError("acquisitionPrice" -> "0")(
              userType,
              individualUserType,
              assetType
            )(
              s"$contextKey.error.tooSmall"
            )(
              s"$contextKey.title",
              TimeUtils.govDisplayFormat(answers.acquisitionDate.value)
            )(performAction, Some(scenarioSession))
          }

        "the user enters zero for amount" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val assetTypeKey = assetTypeMessageKey(assetType)
              val userKey      = userMessageKey(individualUserType, userType)
              amountZero(userType, individualUserType, assetType, userKey + assetTypeKey)
          }
        }

      }

      "show an error page" when {

        val price = 1.23d

        def getSessionDataJourneyAndDraftSession(
          userType: UserType,
          individualUserType: IndividualUserType
        ): (SessionData, FillingOutReturn, DraftReturn) = {
          val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
            acquisitionMethod = Some(AcquisitionMethod.Bought),
            acquisitionDate = Some(sample[AcquisitionDate])
          )

          val (session, journey, draftReturn) = sessionWithState(
            answers,
            sample[AssetType],
            sample[Boolean],
            userType,
            individualUserType
          )

          val updatedDraftReturn = commonUpdateDraftReturn(
            draftReturn,
            answers.copy(acquisitionPrice = Some(AmountInPence(123L))),
            journey
          )

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftSession(
                  userType,
                  individualUserType
                )
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(journey.copy(draftReturn = updatedDraftReturn))(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction("acquisitionPrice" -> price.toString)
              )
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftSession(
                  userType,
                  individualUserType
                )
              val updatedJourney                         = journey.copy(draftReturn = updatedDraftReturn)
              val updatedSession                         = session.copy(journeyStatus = Some(updatedJourney))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(updatedSession)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction("acquisitionPrice" -> price.toString)
              )
          }
        }

      }

      "redirect to the check you answers page" when {

        "the price submitted is valid and the journey was incomplete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val answers                         = IncompleteAcquisitionDetailsAnswers.empty.copy(
                acquisitionMethod = Some(sample[AcquisitionMethod]),
                acquisitionDate = Some(sample[AcquisitionDate])
              )
              val (session, journey, draftReturn) =
                sessionWithState(
                  answers,
                  sample[AssetType],
                  sample[Boolean],
                  userType,
                  individualUserType,
                  isAmend = true
                )

              val updatedDraftReturn = commonUpdateDraftReturn(
                draftReturn,
                answers.copy(acquisitionPrice = Some(AmountInPence(123400L))),
                journey
              )

              val updatedJourney =
                journey.copy(draftReturn = updatedDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends
              val updatedSession = session.copy(journeyStatus = Some(updatedJourney))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(updatedSession)(Right(()))
              }

              checkIsRedirect(
                performAction("acquisitionPrice" -> "£1,234"),
                routes.AcquisitionDetailsController.checkYourAnswers()
              )
          }
        }

        "the price submitted is valid and the journey was complete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val answers                         = sample[CompleteAcquisitionDetailsAnswers]
              val (session, journey, draftReturn) = sessionWithState(
                answers,
                sample[AssetType],
                sample[Boolean],
                userType,
                individualUserType
              )
              val updatedDraftReturn              = commonUpdateDraftReturn(
                draftReturn,
                answers.copy(acquisitionPrice = AmountInPence(123456L)),
                journey
              )

              val updatedJourney = journey.copy(draftReturn = updatedDraftReturn)
              val updatedSession = session.copy(journeyStatus = Some(updatedJourney))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(updatedSession)(Right(()))
              }

              checkIsRedirect(
                performAction("acquisitionPrice" -> "£1,234.56"),
                routes.AcquisitionDetailsController.checkYourAnswers()
              )
          }

        }

      }

    }

    "handling requests to display the rebased acquisition price page" must {

      def performAction(): Future[Result] =
        controller.rebasedAcquisitionPrice()(FakeRequest())

      val acquisitionDate = AcquisitionDate(LocalDate.ofEpochDay(0L))

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.rebasedAcquisitionPrice(),
        mockUUIDGenerator
      )

      behave like missingAssetTypeAndResidentialStatusBehaviour(() => performAction())

      behave like missingAcquisitionDateBehaviour(() => performAction())

      "redirect to the acquisition price page" when {

        "that question hasn't been answered for eligible rebase from non uk resident" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = Some(
                        AcquisitionDate(
                          nonUkResidentsResidentialProperty.minusDays(2)
                        )
                      ),
                      acquisitionPrice = None
                    ),
                    AssetType.Residential,
                    wasUkResident = false,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.acquisitionPrice()
              )
          }
        }

      }

      "that question hasn't been answered for eligible rebase from uk resident" in {
        forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
          (userType: UserType, individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithState(
                  sample[IncompleteAcquisitionDetailsAnswers].copy(
                    acquisitionDate = Some(
                      AcquisitionDate(
                        RebasingCutoffDates.allResidents.minusDays(2)
                      )
                    ),
                    acquisitionMethod = None
                  ),
                  sample[AssetType],
                  wasUkResident = true,
                  userType,
                  individualUserType
                )._1
              )
            }

            checkIsRedirect(
              performAction(),
              routes.AcquisitionDetailsController.acquisitionDate()
            )
        }
      }

      "redirect to the check your answers page" when {

        "the user does not meet the rebasing criteria" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(allResidents.plusDays(1))
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType
                  )._1
                )
              }
              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.checkYourAnswers()
              )
          }
        }

      }

      "display the page" when {

        "the acquisition details section has not yet been completed for non uk resident" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = Some(
                        AcquisitionDate(
                          RebasingCutoffDates.nonUkResidentsResidentialProperty
                        )
                      ),
                      acquisitionPrice = None
                    ),
                    AssetType.Residential,
                    wasUkResident = false,
                    userType,
                    individualUserType
                  )._1
                )
              }
              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.acquisitionPrice()
              )
          }
        }

        def incompleteSectionUkResident(
          userType: UserType,
          individualUserType: IndividualUserType,
          assetType: AssetType,
          userKey: String,
          displayHelpText: Boolean
        ): Unit = {
          val isAmend = sample[Boolean]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = Some(
                    AcquisitionDate(
                      RebasingCutoffDates.allResidents.minusDays(1L)
                    )
                  ),
                  acquisitionPrice = Some(sample[AmountInPence]),
                  acquisitionMethod = Some(AcquisitionMethod.Bought)
                ),
                assetType,
                wasUkResident = true,
                userType,
                individualUserType,
                isAmend = isAmend
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              s"rebaseAcquisitionPrice${assetTypeMessageKey(assetType)}.title",
              TimeUtils.govDisplayFormat(allResidents)
            ),
            { doc =>
              doc
                .select("#back, .govuk-back-link")
                .attr("href")                    shouldBe routes.AcquisitionDetailsController
                .acquisitionDate()
                .url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                  shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPriceSubmit()
                .url
              if (displayHelpText) {
                doc
                  .select("#rebaseAcquisitionPrice-hint")
                  .text() shouldBe messageFromMessageKey(
                  s"rebaseAcquisitionPrice$userKey.helpText"
                )
              }
              doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
            }
          )
        }

        "the user has not yet completed the rebase acquisition details section for uk resident" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val userKey      = userMessageKey(individualUserType, userType)
              val assetTypeKey = assetTypeMessageKey(assetType)
              incompleteSectionUkResident(
                userType,
                individualUserType,
                assetType,
                userKey + assetTypeKey,
                assetType !== IndirectDisposal
              )
          }
        }

        def incompleteSectionNonUKResident(
          userType: UserType,
          individualUserType: IndividualUserType,
          userKey: String
        ): Unit = {
          val isAmend = sample[Boolean]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = Some(acquisitionDate),
                  acquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                wasUkResident = false,
                userType,
                individualUserType,
                isAmend = isAmend
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              TimeUtils.govDisplayFormat(
                nonUkResidentsResidentialProperty
              )
            ),
            { doc =>
              doc
                .select("#back, .govuk-back-link")
                .attr("href")                    shouldBe routes.AcquisitionDetailsController
                .acquisitionPrice()
                .url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                  shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPriceSubmit()
                .url
              doc
                .select("#rebaseAcquisitionPrice-hint")
                .text()                          shouldBe messageFromMessageKey(
                s"rebaseAcquisitionPrice$userKey.helpText"
              )
              doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
            }
          )
        }

        "the user has not yet completed the rebase acquisition details section for non-uk resident" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              incompleteSectionNonUKResident(
                userType,
                individualUserType,
                userKey
              )
          }
        }

        def sectionCompleted(
          userType: UserType,
          individualUserType: IndividualUserType
        ): Unit = {
          val isAmend = sample[Boolean]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers]
                  .copy(acquisitionDate = acquisitionDate),
                AssetType.Residential,
                wasUkResident = true,
                userType,
                individualUserType,
                isAmend = isAmend
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              TimeUtils.govDisplayFormat(allResidents)
            ),
            { doc =>
              doc
                .select("#back, .govuk-back-link")
                .attr("href")                    shouldBe routes.AcquisitionDetailsController
                .checkYourAnswers()
                .url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                  shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPriceSubmit()
                .url
              doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
            }
          )
        }

        "the user has already completed the acquisition details section" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              sectionCompleted(userType, individualUserType)
          }
        }

        def amountNonZero(
          userType: UserType,
          individualUserType: IndividualUserType,
          assetType: AssetType
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = acquisitionDate,
                  rebasedAcquisitionPrice = Some(AmountInPence(1L))
                ),
                assetType,
                wasUkResident = true,
                userType,
                individualUserType
              )._1
            )
          }
          val expectedTitleKey =
            if (assetType === IndirectDisposal) {
              "rebaseAcquisitionPrice.indirect.title"
            } else {
              "rebaseAcquisitionPrice.title"
            }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              expectedTitleKey,
              TimeUtils.govDisplayFormat(allResidents)
            ),
            doc =>
              doc
                .select("#rebaseAcquisitionPrice")
                .attr("value") shouldBe "0.01"
          )
        }

        "the amount in the session is non-zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              amountNonZero(userType, individualUserType, assetType)
          }
        }

      }

    }

    "handling submitted rebased acquisition price answers" must {

      val key = "rebaseAcquisitionPrice"

      def performAction(data: (String, String)*): Future[Result] =
        controller.rebasedAcquisitionPriceSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      val acquisitionDate = AcquisitionDate(allResidents.minusDays(1))

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.rebasedAcquisitionPriceSubmit(),
        mockUUIDGenerator
      )

      behave like missingAssetTypeAndResidentialStatusBehaviour(() => performAction())

      behave like missingAcquisitionDateBehaviour(() => performAction())

      "redirect to the acquisition price page" when {

        "that question hasn't been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = Some(
                        AcquisitionDate(
                          RebasingCutoffDates.nonUkResidentsNonResidentialProperty
                            .minusDays(1)
                        )
                      ),
                      acquisitionPrice = None
                    ),
                    AssetType.NonResidential,
                    wasUkResident = false,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.acquisitionPrice()
              )
          }
        }

      }

      "show a form error" when {

        def test(data: (String, String)*)(
          userType: UserType,
          individualUserType: IndividualUserType
        )(
          expectedErrorMessageKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers]
                  .copy(acquisitionDate = acquisitionDate),
                AssetType.Residential,
                wasUkResident = true,
                userType,
                individualUserType
              )._1
            )
          }

          val formattedRebaseDate = TimeUtils.govDisplayFormat(allResidents)
          checkPageIsDisplayed(
            performAction(data*),
            messageFromMessageKey(
              s"$key.title",
              formattedRebaseDate
            ),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey,
                formattedRebaseDate
              ),
            BAD_REQUEST
          )
        }

        "the amount of money is zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              test("rebaseAcquisitionPrice" -> "0")(
                userType,
                individualUserType
              )(
                s"$key.error.tooSmall"
              )
          }
        }

      }

      "show an error page" when {
        val price = 1.23d

        def getSessionDataJourneyAndDraftReturn(
          userType: UserType,
          individualUserType: IndividualUserType
        ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
          val answers                         = IncompleteAcquisitionDetailsAnswers.empty.copy(
            acquisitionDate = Some(AcquisitionDate(allResidents.minusDays(2))),
            acquisitionPrice = Some(sample[AmountInPence])
          )
          val (session, journey, draftReturn) = sessionWithState(
            answers,
            AssetType.Residential,
            wasUkResident = true,
            userType,
            individualUserType
          )
          val updatedDraftReturn              = commonUpdateDraftReturn(
            draftReturn,
            answers.copy(
              rebasedAcquisitionPrice = Some(AmountInPence(123L)),
              acquisitionPrice = Some(AmountInPence(123L)),
              shouldUseRebase = Some(true)
            ),
            journey
          )

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(
                  userType,
                  individualUserType
                )
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(journey.copy(draftReturn = updatedDraftReturn))(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(key -> price.toString)
              )
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(
                  userType,
                  individualUserType
                )
              val updatedJourney                         = journey.copy(draftReturn = updatedDraftReturn)
              val updatedSession                         = session.copy(journeyStatus = Some(updatedJourney))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(updatedSession)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(key -> price.toString)
              )
          }
        }

      }

      "redirect to the check you answers page" when {

        val scenarios = List(
          List("rebaseAcquisitionPrice" -> "£1,234") -> AmountInPence(123400L),
          List("rebaseAcquisitionPrice" -> "1")      -> AmountInPence(100L)
        )

        "the price submitted is valid and the journey was incomplete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              scenarios.foreach { case (formData, expectedAmountInPence) =>
                withClue(
                  s"For form data $formData and expected amount in pence $expectedAmountInPence: "
                ) {
                  val answers                         =
                    IncompleteAcquisitionDetailsAnswers.empty.copy(
                      acquisitionDate = Some(acquisitionDate),
                      acquisitionPrice = Some(sample[AmountInPence])
                    )
                  val (session, journey, draftReturn) = sessionWithState(
                    answers,
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType
                  )
                  val updatedDraftReturn              = commonUpdateDraftReturn(
                    draftReturn,
                    answers.copy(
                      rebasedAcquisitionPrice = Some(expectedAmountInPence),
                      acquisitionPrice = Some(expectedAmountInPence),
                      shouldUseRebase = Some(true)
                    ),
                    journey
                  )

                  val updatedJourney = journey.copy(draftReturn = updatedDraftReturn)
                  val updatedSession = session.copy(journeyStatus = Some(updatedJourney))

                  inSequence {
                    mockAuthWithNoRetrievals()
                    mockGetSession(session)
                    mockStoreDraftReturn(updatedJourney)(Right(()))
                    mockStoreSession(updatedSession)(Right(()))
                  }

                  checkIsRedirect(
                    performAction(formData*),
                    routes.AcquisitionDetailsController.checkYourAnswers()
                  )
                }
              }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (_: UserType, individualUserType: IndividualUserType) =>
              scenarios.foreach { case (formData, expectedAmountInPence) =>
                withClue(
                  s"For form data $formData and expected amount in pence $expectedAmountInPence: "
                ) {
                  val answers                         =
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = acquisitionDate,
                      rebasedAcquisitionPrice = Some(AmountInPence(expectedAmountInPence.value + 1L))
                    )
                  val (session, journey, draftReturn) = sessionWithState(
                    answers,
                    AssetType.Residential,
                    wasUkResident = true,
                    UserType.Individual,
                    individualUserType,
                    isAmend = true
                  )
                  val updatedDraftReturn              = commonUpdateDraftReturn(
                    draftReturn,
                    answers.copy(
                      rebasedAcquisitionPrice = Some(expectedAmountInPence),
                      acquisitionPrice = expectedAmountInPence,
                      shouldUseRebase = true
                    ),
                    journey
                  )
                  val updatedJourney                  =
                    journey.copy(draftReturn = updatedDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends
                  val updatedSession                  = session.copy(journeyStatus = Some(updatedJourney))

                  inSequence {
                    mockAuthWithNoRetrievals()
                    mockGetSession(session)
                    mockStoreDraftReturn(updatedJourney)(Right(()))
                    mockStoreSession(updatedSession)(Right(()))
                  }

                  checkIsRedirect(
                    performAction(formData*),
                    routes.AcquisitionDetailsController.checkYourAnswers()
                  )
                }
              }
          }
        }

      }

    }

    "handling requests to display the improvement costs page" must {

      val key = "improvementCosts"

      def performAction(): Future[Result] =
        controller.improvementCosts()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.improvementCosts(),
        mockUUIDGenerator
      )

      behave like missingAssetTypeAndResidentialStatusBehaviour(() => performAction())

      behave like missingAcquisitionDateBehaviour(() => performAction())

      "redirect to the acquisition price page" when {

        "the user does not meet the rebasing criteria and the user has not supplied an acquisition price yet" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = Some(AcquisitionDate(allResidents.plusDays(1L))),
                      acquisitionPrice = None
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.acquisitionPrice()
              )
          }
        }

      }

      "redirect to the rebased acquisition price page" when {

        "the user does meet the rebasing criteria and the user has not supplied a rebased acquisition price yet" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionMethod = Some(AcquisitionMethod.Bought),
                      acquisitionDate = Some(AcquisitionDate(allResidents.minusDays(1L))),
                      acquisitionPrice = Some(AmountInPence(100L)),
                      rebasedAcquisitionPrice = None
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
              )
          }
        }

      }

      "display the page" when {

        def incompleteSection(
          userType: UserType,
          individualUserType: IndividualUserType,
          userKey: String
        ): Unit = {
          val isAmend = sample[Boolean]
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate = Some(AcquisitionDate(allResidents.minusDays(1L))),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                wasUkResident = true,
                userType,
                individualUserType,
                isAmend = isAmend
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key$userKey.title"),
            { doc =>
              doc
                .select("#back, .govuk-back-link")
                .attr("href")                    shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPrice()
                .url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                  shouldBe routes.AcquisitionDetailsController
                .improvementCostsSubmit()
                .url
              doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
            }
          )
        }

        "the user meets the rebasing criteria and their acquisition details journey is incomplete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              incompleteSection(userType, individualUserType, userKey)
          }
        }

        "the user does not meet the rebasing criteria and their acquisition details journey is incomplete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            val isAmend = sample[Boolean]
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = Some(AcquisitionDate(allResidents.plusDays(1L))),
                      acquisitionPrice = Some(sample[AmountInPence]),
                      rebasedAcquisitionPrice = None
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType,
                    isAmend = isAmend
                  )._1
                )
              }
              val userKey = userMessageKey(individualUserType, userType)
              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"),
                { doc =>
                  doc
                    .select("#back, .govuk-back-link")
                    .attr("href")                    shouldBe routes.AcquisitionDetailsController
                    .acquisitionPrice()
                    .url
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action")                  shouldBe routes.AcquisitionDetailsController
                    .improvementCostsSubmit()
                    .url
                  doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                }
              )
          }
        }

        "the user meets the rebasing criteria and their acquisition details journey is complete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val isAmend = sample[Boolean]
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(allResidents.minusDays(1L)),
                      rebasedAcquisitionPrice = Some(sample[AmountInPence])
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType,
                    isAmend = isAmend
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"),
                { doc =>
                  doc
                    .select("#back, .govuk-back-link")
                    .attr("href")                    shouldBe routes.AcquisitionDetailsController
                    .checkYourAnswers()
                    .url
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action")                  shouldBe routes.AcquisitionDetailsController
                    .improvementCostsSubmit()
                    .url
                  doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                }
              )
          }
        }

        "the user does not meet the rebasing criteria and their acquisition details journey is complete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val isAmend = sample[Boolean]
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(allResidents.plusDays(1L)),
                      rebasedAcquisitionPrice = None
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType,
                    isAmend = isAmend
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"),
                { doc =>
                  doc
                    .select("#back, .govuk-back-link")
                    .attr("href")                    shouldBe routes.AcquisitionDetailsController
                    .checkYourAnswers()
                    .url
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action")                  shouldBe routes.AcquisitionDetailsController
                    .improvementCostsSubmit()
                    .url
                  doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                }
              )
          }
        }

        "the amount in the session is zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(allResidents),
                      rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                      improvementCosts = AmountInPence.zero
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"$key$userKey.title"
                ),
                doc =>
                  doc
                    .select("#improvementCosts-1")
                    .hasAttr("checked")
              )
          }
        }

        "the amount in the session is non-zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val isAmend = sample[Boolean]
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(allResidents),
                      rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                      improvementCosts = AmountInPence(2L)
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType,
                    isAmend = isAmend
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"$key$userKey.title"
                ),
                { doc =>
                  doc
                    .select("#improvementCosts-0")
                    .attr("checked")                 shouldBe ""
                  doc
                    .select("#improvementCostsValue")
                    .attr("value")                   shouldBe "0.02"
                  doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                }
              )
          }
        }

      }

    }

    "handling submitted improvement costs answers" must {

      val key      = "improvementCosts"
      val valueKey = "improvementCostsValue"

      def performAction(data: (String, String)*): Future[Result] =
        controller.improvementCostsSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      val acquisitionDate = AcquisitionDate(LocalDate.ofEpochDay(0L))

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.improvementCostsSubmit(),
        mockUUIDGenerator
      )

      behave like missingAssetTypeAndResidentialStatusBehaviour(() => performAction())

      behave like missingAcquisitionDateBehaviour(() => performAction())

      "redirect to the acquisition price page" when {

        "the user does not meet the rebasing criteria and the user has not supplied an acquisition price yet" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = Some(AcquisitionDate(allResidents.plusDays(1L))),
                      acquisitionPrice = None
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.acquisitionPrice()
              )
          }
        }

      }

      "redirect to the rebased acquisition price page" when {

        "the user does meet the rebasing criteria and the user has not supplied an acquisition price yet" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = Some(AcquisitionDate(allResidents.minusDays(1L))),
                      rebasedAcquisitionPrice = None
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
              )
          }
        }

      }

      "show a form error" when {

        def test(
          data: (String, String)*
        )(
          userType: UserType,
          individualUserType: IndividualUserType,
          assetType: AssetType,
          userKey: String
        )(expectedErrorKey: String): Unit =
          testFormError(data*)(userType, individualUserType, assetType)(
            expectedErrorKey
          )(s"$key$userKey.title")(performAction)

        "no option has been selected" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val assetTypeKey = assetTypeMessageKey(assetType)
              val userKey      = userMessageKey(individualUserType, userType)
              test()(userType, individualUserType, assetType, userKey)(
                s"$key$userKey$assetTypeKey.error.required"
              )
          }
        }

        "the option selected is not valid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test(key -> "2")(userType, individualUserType, assetType, userKey)(
                s"$key$userKey.error.invalid"
              )
          }
        }

        "the amount of money is invalid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val userKey = userMessageKey(individualUserType, userType)
              amountOfMoneyErrorScenarios(valueKey).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  val data = (key -> "0") :: scenario.formData
                  test(data*)(userType, individualUserType, assetType, userKey)(
                    scenario.expectedErrorMessageKey
                  )
                }
              }
          }
        }

        "the amount of money is zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test(key -> "0", valueKey -> "0")(
                userType,
                individualUserType,
                assetType,
                userKey
              )(
                s"$valueKey.error.tooSmall"
              )
          }
        }

      }

      "show an error page" when {
        val price = 1.23d

        def getSessionDataJourneyAndDraftReturn(
          userType: UserType,
          individualUserType: IndividualUserType
        ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {
          val answers                         = IncompleteAcquisitionDetailsAnswers.empty.copy(
            acquisitionDate = Some(acquisitionDate),
            acquisitionPrice = Some(sample[AmountInPence]),
            rebasedAcquisitionPrice = Some(sample[AmountInPence])
          )
          val (session, journey, draftReturn) = sessionWithState(
            answers,
            AssetType.Residential,
            wasUkResident = true,
            userType,
            individualUserType
          )
          val updatedDraftReturn              = commonUpdateDraftReturn(
            draftReturn,
            answers.copy(improvementCosts = Some(AmountInPence(123L))),
            journey
          )

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(
                  userType,
                  individualUserType
                )
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(journey.copy(draftReturn = updatedDraftReturn))(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(key -> "0", valueKey -> price.toString)
              )
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(
                  userType,
                  individualUserType
                )
              val updatedJourney                         = journey.copy(draftReturn = updatedDraftReturn)
              val updatedSession                         = session.copy(journeyStatus = Some(updatedJourney))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(updatedSession)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(key -> "0", valueKey -> price.toString)
              )
          }
        }

      }

      "redirect to the check you answers page" when {
        val scenarios = List(
          List(key -> "0", valueKey -> "£1,234") -> AmountInPence(123400L),
          List(key -> "1")                       -> AmountInPence.zero
        )

        "the price submitted is valid and the journey was incomplete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              scenarios.foreach { case (formData, expectedAmountInPence) =>
                withClue(
                  s"For form data $formData and expected amount in pence $expectedAmountInPence: "
                ) {
                  val answers                         =
                    IncompleteAcquisitionDetailsAnswers.empty.copy(
                      acquisitionDate = Some(acquisitionDate),
                      acquisitionPrice = Some(sample[AmountInPence]),
                      rebasedAcquisitionPrice = Some(sample[AmountInPence])
                    )
                  val (session, journey, draftReturn) = sessionWithState(
                    answers,
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType
                  )
                  val updatedDraftReturn              = commonUpdateDraftReturn(
                    draftReturn,
                    answers.copy(improvementCosts = Some(expectedAmountInPence)),
                    journey
                  )
                  val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)
                  val updatedSession                  = session.copy(journeyStatus = Some(updatedJourney))

                  inSequence {
                    mockAuthWithNoRetrievals()
                    mockGetSession(session)
                    mockStoreDraftReturn(updatedJourney)(Right(()))
                    mockStoreSession(updatedSession)(Right(()))
                  }

                  checkIsRedirect(
                    performAction(formData*),
                    routes.AcquisitionDetailsController.checkYourAnswers()
                  )
                }
              }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              scenarios.foreach { case (formData, expectedAmountInPence) =>
                withClue(
                  s"For form data $formData and expected amount in pence $expectedAmountInPence: "
                ) {

                  val answers                         = sample[CompleteAcquisitionDetailsAnswers]
                    .copy(
                      acquisitionDate = acquisitionDate,
                      rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                      improvementCosts = AmountInPence(expectedAmountInPence.value + 1L)
                    )
                  val (session, journey, draftReturn) = sessionWithState(
                    answers,
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType,
                    isAmend = true
                  )
                  val updatedDraftReturn              = commonUpdateDraftReturn(
                    draftReturn,
                    answers.copy(improvementCosts = expectedAmountInPence),
                    journey
                  )
                  val updatedJourney                  =
                    journey.copy(draftReturn = updatedDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends
                  val updatedSession                  = session.copy(journeyStatus = Some(updatedJourney))

                  inSequence {
                    mockAuthWithNoRetrievals()
                    mockGetSession(session)
                    mockStoreDraftReturn(updatedJourney)(Right(()))
                    mockStoreSession(updatedSession)(Right(()))
                  }

                  checkIsRedirect(
                    performAction(formData*),
                    routes.AcquisitionDetailsController.checkYourAnswers()
                  )
                }
              }
          }
        }

      }

    }

    "handling requests to display the acquisition fees page" must {

      val key = "acquisitionFees"

      def performAction(): Future[Result] =
        controller.acquisitionFees()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.acquisitionFees(),
        mockUUIDGenerator
      )

      "redirect to the improvement costs page" when {

        "that question hasn't been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      improvementCosts = None,
                      acquisitionDate = Some(sample[AcquisitionDate])
                    ),
                    sample[AssetType],
                    sample[Boolean],
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.improvementCosts()
              )
          }
        }

      }

      "display the page" when {

        "the acquisition details section has not yet been completed without rebasing" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val isAmend = sample[Boolean]
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = Some(
                        AcquisitionDate(
                          nonUkResidentsNonResidentialProperty.plusDays(2L)
                        )
                      ),
                      shouldUseRebase = Some(false),
                      improvementCosts = Some(sample[AmountInPence])
                    ),
                    AssetType.NonResidential,
                    wasUkResident = false,
                    userType,
                    individualUserType,
                    isAmend = isAmend
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"),
                { doc =>
                  doc
                    .select("#back, .govuk-back-link")
                    .attr("href")                    shouldBe routes.AcquisitionDetailsController
                    .improvementCosts()
                    .url
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action")                  shouldBe routes.AcquisitionDetailsController
                    .acquisitionFeesSubmit()
                    .url
                  doc
                    .select("#acquisitionFees-hint")
                    .text()                          shouldBe messageFromMessageKey(s"$key$userKey.helpText")
                  doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                }
              )
          }
        }

        "the acquisition details section has not yet been completed with rebasing" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            val isAmend = sample[Boolean]
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = Some(AcquisitionDate(allResidents.minusDays(2L))),
                      shouldUseRebase = Some(true),
                      improvementCosts = Some(sample[AmountInPence])
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType,
                    isAmend = isAmend
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.rebased.title"),
                { doc =>
                  doc
                    .select("#back, .govuk-back-link")
                    .attr("href")                    shouldBe routes.AcquisitionDetailsController
                    .improvementCosts()
                    .url
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action")                  shouldBe routes.AcquisitionDetailsController
                    .acquisitionFeesSubmit()
                    .url
                  doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                }
              )
          }
        }

        "the acquisition details section has been completed without rebasing" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val isAmend = sample[Boolean]
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(
                        nonUkResidentsResidentialProperty.plusDays(1L)
                      ),
                      shouldUseRebase = false
                    ),
                    AssetType.Residential,
                    sample[Boolean],
                    userType,
                    individualUserType,
                    isAmend = isAmend
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"),
                { doc =>
                  doc
                    .select("#back, .govuk-back-link")
                    .attr("href")                    shouldBe routes.AcquisitionDetailsController
                    .checkYourAnswers()
                    .url
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action")                  shouldBe routes.AcquisitionDetailsController
                    .acquisitionFeesSubmit()
                    .url
                  doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                }
              )
          }
        }

        "the acquisition details section has been completed with rebasing" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val isAmend = sample[Boolean]
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(allResidents.minusDays(1L)),
                      shouldUseRebase = true
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType,
                    isAmend = isAmend
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.rebased.title"),
                { doc =>
                  doc
                    .select("#back, .govuk-back-link")
                    .attr("href")                    shouldBe routes.AcquisitionDetailsController
                    .checkYourAnswers()
                    .url
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action")                  shouldBe routes.AcquisitionDetailsController
                    .acquisitionFeesSubmit()
                    .url
                  doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                }
              )
          }
        }

        "the amount in the session is zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(
                        nonUkResidentsResidentialProperty.plusDays(2L)
                      ),
                      acquisitionFees = AmountInPence.zero,
                      shouldUseRebase = false
                    ),
                    assetType,
                    sample[Boolean],
                    userType,
                    individualUserType
                  )._1
                )
              }

              val userKey      = userMessageKey(individualUserType, userType)
              val assetTypeKey = assetTypeMessageKey(assetType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey$assetTypeKey.title"),
                doc =>
                  doc
                    .select("#acquisitionFees-1")
                    .hasAttr("checked")
              )
          }
        }

        "the amount in the session is non-zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val isAmend = sample[Boolean]
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(
                        nonUkResidentsResidentialProperty.plusDays(2L)
                      ),
                      acquisitionFees = AmountInPence(3L),
                      shouldUseRebase = false
                    ),
                    assetType,
                    sample[Boolean],
                    userType,
                    individualUserType,
                    isAmend = isAmend
                  )._1
                )
              }

              val userKey      = userMessageKey(individualUserType, userType)
              val assetTypeKey = assetTypeMessageKey(assetType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  s"$key$userKey$assetTypeKey.title"
                ),
                { doc =>
                  doc
                    .select("#acquisitionFees-0")
                    .attr("checked")                 shouldBe ""
                  doc
                    .select("#acquisitionFeesValue")
                    .attr("value")                   shouldBe "0.03"
                  doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                }
              )
          }
        }

        "the user is a personal rep in a period of admin" in {
          List(UserType.Individual, UserType.Agent).foreach { userType =>
            withClue(s"For user type $userType: ") {
              val isAmend = sample[Boolean]
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = Some(
                        AcquisitionDate(
                          nonUkResidentsNonResidentialProperty.plusDays(2L)
                        )
                      ),
                      shouldUseRebase = Some(false),
                      improvementCosts = Some(sample[AmountInPence])
                    ),
                    AssetType.NonResidential,
                    wasUkResident = false,
                    userType,
                    PersonalRepresentativeInPeriodOfAdmin,
                    isAmend = isAmend
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key.personalRep.title"),
                { doc =>
                  doc
                    .select("#back, .govuk-back-link")
                    .attr("href")                    shouldBe routes.AcquisitionDetailsController
                    .improvementCosts()
                    .url
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action")                  shouldBe routes.AcquisitionDetailsController
                    .acquisitionFeesSubmit()
                    .url
                  doc
                    .select("#acquisitionFees-hint")
                    .text()                          shouldBe messageFromMessageKey("acquisitionFees.personalRepInPeriodOfAdmin.helpText")
                  doc.select("#submitButton").text() shouldBe expectedSubmitText(isAmend)
                }
              )
            }

          }

        }

      }

    }

    "handling submitted acquisition fees" must {

      val key      = "acquisitionFees"
      val valueKey = "acquisitionFeesValue"

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionFeesSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.acquisitionFeesSubmit(),
        mockUUIDGenerator
      )

      "redirect to the improvement costs page" when {

        "the question has not been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      improvementCosts = None,
                      acquisitionDate = Some(sample[AcquisitionDate])
                    ),
                    sample[AssetType],
                    sample[Boolean],
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.improvementCosts()
              )
          }
        }

      }

      "show a form error" when {

        def test(
          data: (String, String)*
        )(
          userType: UserType,
          individualUserType: IndividualUserType,
          assetType: AssetType,
          userKey: String,
          wasUkResident: Boolean
        )(expectedErrorKey: String): Unit =
          testFormError(data*)(userType, individualUserType, assetType, wasUkResident)(
            expectedErrorKey,
            models.TimeUtils.govDisplayFormat(mockRebasingUtil.getRebasingCutOffDate(assetType, wasUkResident))
          )(s"$key$userKey.title")(performAction)

        "no option has been selected" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val userKey      = userMessageKey(individualUserType, userType)
              val userErrorKey = if (userKey.startsWith(".")) "" else ".individual"
              val assetTypeKey = assetTypeMessageKey(assetType)
              test()(userType, individualUserType, assetType, userKey + assetTypeKey, wasUkResident = false)(
                s"$key$userErrorKey$assetTypeKey.error.required"
              )
          }
        }

        "the option selected is not valid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val assetTypeKey = assetTypeMessageKey(assetType)
              val userKey      = userMessageKey(individualUserType, userType)
              val userErrorKey = if (userKey.startsWith(".")) "" else ".individual"
              test(key -> "2")(userType, individualUserType, assetType, userKey + assetTypeKey, wasUkResident = false)(
                s"$key$userErrorKey$assetTypeKey.error.invalid"
              )
          }
        }

        "the amount of money is invalid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val userKey      = userMessageKey(individualUserType, userType)
              val assetTypeKey = assetTypeMessageKey(assetType)
              amountOfMoneyErrorScenarios(valueKey).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  val data = (key -> "0") :: scenario.formData
                  test(data*)(
                    userType,
                    individualUserType,
                    assetType,
                    userKey + assetTypeKey,
                    wasUkResident = false
                  )(
                    scenario.expectedErrorMessageKey
                  )
                }
              }
          }
        }

        "the amount of money is zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen, acceptedAssetTypeGenerator) {
            (userType: UserType, individualUserType: IndividualUserType, assetType: AssetType) =>
              val assetTypeKey = assetTypeMessageKey(assetType)
              val userKey      = userMessageKey(individualUserType, userType)
              test(key -> "0", valueKey -> "0")(
                userType,
                individualUserType,
                assetType,
                userKey + assetTypeKey,
                wasUkResident = false
              )(
                s"$valueKey.error.tooSmall"
              )
          }
        }

      }

      "show an error page" when {
        val price = 1.23d

        def getSessionDataJourneyAndDraftReturn(
          userType: UserType,
          individualUserType: IndividualUserType
        ): (SessionData, FillingOutReturn, DraftReturn) = {
          val answers                         = IncompleteAcquisitionDetailsAnswers.empty.copy(
            improvementCosts = Some(sample[AmountInPence]),
            acquisitionDate = Some(sample[AcquisitionDate])
          )
          val (session, journey, draftReturn) = sessionWithState(
            answers,
            sample[AssetType],
            sample[Boolean],
            userType,
            individualUserType
          )
          val updatedDraftReturn              = commonUpdateDraftReturn(
            draftReturn,
            answers.copy(acquisitionFees = Some(AmountInPence(123L))),
            journey
          )

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(
                  userType,
                  individualUserType
                )
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(journey.copy(draftReturn = updatedDraftReturn))(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(key -> "0", valueKey -> price.toString)
              )
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(
                  userType,
                  individualUserType
                )
              val updatedJourney                         = journey.copy(draftReturn = updatedDraftReturn)
              val updatedSession                         = session.copy(journeyStatus = Some(updatedJourney))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(updatedSession)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction(key -> "0", valueKey -> price.toString)
              )
          }
        }

      }

      "redirect to the check you answers page" when {

        val scenarios = List(
          List(key -> "0", valueKey -> "£1,234") -> AmountInPence(123400L),
          List(key -> "1")                       -> AmountInPence.zero
        )

        "the price submitted is valid and the journey was incomplete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              scenarios.foreach { case (formData, expectedAmountInPence) =>
                withClue(
                  s"For form data $formData and expected amount in pence $expectedAmountInPence: "
                ) {
                  val answers                         =
                    IncompleteAcquisitionDetailsAnswers.empty.copy(
                      improvementCosts = Some(sample[AmountInPence]),
                      acquisitionDate = Some(sample[AcquisitionDate])
                    )
                  val (session, journey, draftReturn) = sessionWithState(
                    answers,
                    sample[AssetType],
                    sample[Boolean],
                    userType,
                    individualUserType,
                    isAmend = true
                  )
                  val updatedDraftReturn              = commonUpdateDraftReturn(
                    draftReturn,
                    answers.copy(acquisitionFees = Some(expectedAmountInPence)),
                    journey
                  )

                  val updatedJourney =
                    journey.copy(draftReturn = updatedDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends
                  val updatedSession = session.copy(journeyStatus = Some(updatedJourney))

                  inSequence {
                    mockAuthWithNoRetrievals()
                    mockGetSession(session)
                    mockStoreDraftReturn(updatedJourney)(Right(()))
                    mockStoreSession(updatedSession)(Right(()))
                  }

                  checkIsRedirect(
                    performAction(formData*),
                    routes.AcquisitionDetailsController.checkYourAnswers()
                  )
                }
              }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              scenarios.foreach { case (formData, expectedAmountInPence) =>
                withClue(
                  s"For form data $formData and expected amount in pence $expectedAmountInPence: "
                ) {
                  val answers                         =
                    sample[CompleteAcquisitionDetailsAnswers]
                      .copy(acquisitionFees = AmountInPence(expectedAmountInPence.value + 1L))
                  val (session, journey, draftReturn) = sessionWithState(
                    answers,
                    sample[AssetType],
                    sample[Boolean],
                    userType,
                    individualUserType
                  )
                  val updatedDraftReturn              = commonUpdateDraftReturn(
                    draftReturn,
                    answers.copy(acquisitionFees = expectedAmountInPence),
                    journey
                  )
                  val updatedJourney                  = journey.copy(draftReturn = updatedDraftReturn)
                  val updatedSession                  = session.copy(journeyStatus = Some(updatedJourney))

                  inSequence {
                    mockAuthWithNoRetrievals()
                    mockGetSession(session)
                    mockStoreDraftReturn(updatedJourney)(Right(()))
                    mockStoreSession(updatedSession)(Right(()))
                  }

                  checkIsRedirect(
                    performAction(formData*),
                    routes.AcquisitionDetailsController.checkYourAnswers()
                  )
                }
              }
          }

        }

      }

    }

    "handling requests to display the should use rebase page" must {

      def performAction(): Future[Result] =
        controller.shouldUseRebase()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.shouldUseRebase(),
        mockUUIDGenerator
      )

      "display the page" when {

        "the user is non uk, residential and acquisition date before rebasing date" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      improvementCosts = Some(sample[AmountInPence])
                    ),
                    AssetType.Residential,
                    wasUkResident = false,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  "shouldUseRebase.title",
                  TimeUtils.govDisplayFormat(
                    nonUkResidentsResidentialProperty
                  )
                ),
                doc =>
                  doc.select("#single-warning").text() shouldBe (userType match {
                    case UserType.Organisation =>
                      "! " + messageFromMessageKey("generic.warning") + " " + messageFromMessageKey(
                        "shouldUseRebase.nrTrust.warning"
                      )
                    case _                     => ""
                  })
              )
          }

        }

        "the user is non uk and residential asset type" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      improvementCosts = Some(sample[AmountInPence]),
                      shouldUseRebase = Some(true)
                    ),
                    AssetType.NonResidential,
                    wasUkResident = false,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  "shouldUseRebase.title",
                  TimeUtils.govDisplayFormat(
                    nonUkResidentsNonResidentialProperty
                  )
                ),
                doc => doc.select("#shouldUseRebase-true").hasAttr("checked")
              )
          }
        }
      }

      "redirect to check your answers" when {

        "the user is uk" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      improvementCosts = Some(sample[AmountInPence])
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.checkYourAnswers()
              )
          }
        }
      }
    }

    "handling requests to submit to should use rebased" must {

      val disposalDate = DisposalDate(LocalDate.of(1200, 1, 1), sample[TaxYear])

      def performAction(data: (String, String)*): Future[Result] =
        controller.shouldUseRebaseSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withMethod("POST")
        )

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.shouldUseRebaseSubmit(),
        mockUUIDGenerator
      )

      "show a form error for non residential non uk" when {

        val date: String = TimeUtils.govDisplayFormat(
          nonUkResidentsNonResidentialProperty
        )

        def test(
          data: (String, String)*
        )(userType: UserType, individualUserType: IndividualUserType)(
          expectedErrorKey: String
        ): Unit =
          testFormError(data*)(userType, individualUserType, NonResidential)(
            expectedErrorKey
          )("shouldUseRebase.title", date)(
            performAction,
            Some(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers]
                  .copy(acquisitionDate =
                    AcquisitionDate(
                      nonUkResidentsNonResidentialProperty.minusDays(1)
                    )
                  ),
                NonResidential,
                wasUkResident = false,
                userType,
                individualUserType,
                disposalDate
              )._1
            )
          )

        "no option has been selected" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              test()(userType, individualUserType)(
                "shouldUseRebase.error.required"
              )
          }
        }

      }

      "show a form error for residential non uk" when {
        val date: String = TimeUtils.govDisplayFormat(
          nonUkResidentsResidentialProperty
        )

        def test(
          data: (String, String)*
        )(userType: UserType, individualUserType: IndividualUserType)(
          expectedErrorKey: String
        ): Unit =
          testFormError(data*)(userType, individualUserType, Residential)(
            expectedErrorKey
          )("shouldUseRebase.title", date)(
            performAction,
            Some(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers]
                  .copy(acquisitionDate = AcquisitionDate(nonUkResidentsResidentialProperty)),
                Residential,
                wasUkResident = false,
                userType,
                individualUserType,
                disposalDate
              )._1
            )
          )

        "no option has been selected" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              test()(userType, individualUserType)(
                "shouldUseRebase.error.required"
              )
          }
        }
      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      val completeAnswers = CompleteAcquisitionDetailsAnswers(
        sample[AcquisitionMethod],
        sample[AcquisitionDate],
        sample[AmountInPence],
        Some(sample[AmountInPence]),
        sample[AmountInPence],
        sample[AmountInPence],
        sample[Boolean]
      )

      val allQuestionsAnswered = IncompleteAcquisitionDetailsAnswers(
        Some(completeAnswers.acquisitionMethod),
        Some(completeAnswers.acquisitionDate),
        Some(completeAnswers.acquisitionPrice),
        completeAnswers.rebasedAcquisitionPrice,
        Some(completeAnswers.improvementCosts),
        Some(completeAnswers.acquisitionFees),
        Some(completeAnswers.shouldUseRebase)
      )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswers(),
        mockUUIDGenerator
      )

      behave like missingAssetTypeAndResidentialStatusBehaviour(() => performAction())

      def testRedirectOnMissingData(
        session: SessionData,
        expectedRedirect: Call
      ): Unit = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkIsRedirect(performAction(), expectedRedirect)
      }

      "redirect to the acquisition method page" when {

        "the question has not been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              testRedirectOnMissingData(
                sessionWithState(
                  allQuestionsAnswered.copy(acquisitionMethod = None),
                  sample[AssetType],
                  sample[Boolean],
                  userType,
                  individualUserType
                )._1,
                routes.AcquisitionDetailsController.acquisitionMethod()
              )
          }
        }

      }

      "redirect to the acquisition date page" when {

        "the question has not been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              testRedirectOnMissingData(
                sessionWithState(
                  allQuestionsAnswered.copy(acquisitionDate = None),
                  sample[AssetType],
                  sample[Boolean],
                  userType,
                  individualUserType
                )._1,
                routes.AcquisitionDetailsController.acquisitionDate()
              )
          }
        }

      }

      "redirect to the period of admin market value page" when {

        "there is no acquisition price and the user is a period of admin" in {
          testRedirectOnMissingData(
            sessionWithState(
              allQuestionsAnswered
                .copy(
                  acquisitionPrice = None,
                  acquisitionDate = Some(AcquisitionDate(LocalDate.now())),
                  acquisitionMethod = Some(AcquisitionMethod.Other("period of admin"))
                ),
              sample[AssetType],
              sample[Boolean],
              UserType.Individual,
              PersonalRepresentativeInPeriodOfAdmin
            )._1,
            routes.AcquisitionDetailsController.periodOfAdminMarketValue()
          )
        }

      }

      "redirect to the acquisition price page" when {

        "the question has not been answered, and eligible for acquisition price" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              testRedirectOnMissingData(
                sessionWithState(
                  allQuestionsAnswered
                    .copy(
                      acquisitionPrice = None,
                      acquisitionDate = Some(AcquisitionDate(LocalDate.now()))
                    ),
                  sample[AssetType],
                  sample[Boolean],
                  userType,
                  individualUserType
                )._1,
                routes.AcquisitionDetailsController.acquisitionPrice()
              )
          }
        }

      }

      "redirect to the rebased acquisition price page" when {

        "the question has not been answered and the user meets the rebasing criteria" when {

          "the user was a uk resident and is disposing a residential property" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testRedirectOnMissingData(
                  sessionWithState(
                    allQuestionsAnswered.copy(
                      acquisitionDate = Some(AcquisitionDate(allResidents.minusDays(1L))),
                      rebasedAcquisitionPrice = None
                    ),
                    AssetType.Residential,
                    wasUkResident = true,
                    userType,
                    individualUserType
                  )._1,
                  routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
                )
            }
          }

          "the user was a non-uk resident and is disposing a residential property" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testRedirectOnMissingData(
                  sessionWithState(
                    allQuestionsAnswered.copy(
                      acquisitionDate = Some(
                        AcquisitionDate(
                          RebasingCutoffDates.nonUkResidentsResidentialProperty
                        )
                      ),
                      rebasedAcquisitionPrice = None
                    ),
                    AssetType.Residential,
                    wasUkResident = false,
                    userType,
                    individualUserType
                  )._1,
                  routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
                )
            }
          }

          "the user was a non-uk resident and is disposing a non-residential property" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                testRedirectOnMissingData(
                  sessionWithState(
                    allQuestionsAnswered.copy(
                      acquisitionDate = Some(
                        AcquisitionDate(
                          RebasingCutoffDates.nonUkResidentsNonResidentialProperty
                            .minusDays(1L)
                        )
                      ),
                      rebasedAcquisitionPrice = None
                    ),
                    AssetType.NonResidential,
                    wasUkResident = false,
                    userType,
                    individualUserType
                  )._1,
                  routes.AcquisitionDetailsController.rebasedAcquisitionPrice()
                )
            }
          }

        }

      }

      "redirect to the improvement costs page" when {

        "the question has not been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              testRedirectOnMissingData(
                sessionWithState(
                  allQuestionsAnswered.copy(improvementCosts = None),
                  sample[AssetType],
                  sample[Boolean],
                  userType,
                  individualUserType
                )._1,
                routes.AcquisitionDetailsController.improvementCosts()
              )
          }
        }

      }

      "redirect to the acquisition fees page" when {

        "the question has not been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              testRedirectOnMissingData(
                sessionWithState(
                  allQuestionsAnswered.copy(acquisitionFees = None),
                  sample[AssetType],
                  sample[Boolean],
                  userType,
                  individualUserType
                )._1,
                routes.AcquisitionDetailsController.acquisitionFees()
              )
          }
        }

      }

      "show an error page when the user has just answered all of the questions and" when {

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, draftReturn) = sessionWithState(
                allQuestionsAnswered,
                sample[AssetType],
                sample[Boolean],
                userType,
                individualUserType
              )
              val newDraftReturn                  = draftReturn.copy(acquisitionDetailsAnswers = Some(completeAnswers))
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(journey.copy(draftReturn = newDraftReturn))(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, draftReturn) = sessionWithState(
                allQuestionsAnswered,
                sample[AssetType],
                sample[Boolean],
                userType,
                individualUserType
              )
              val newDraftReturn                  = draftReturn.copy(acquisitionDetailsAnswers = Some(completeAnswers))
              val updatedJourney                  = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(
                  session.copy(journeyStatus = Some(updatedJourney))
                )(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction())
          }
        }

      }

      "show the page" when {

        "the user has just answered all the questions and all updates are successful" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, draftReturn) = sessionWithState(
                allQuestionsAnswered,
                sample[AssetType],
                sample[Boolean],
                userType,
                individualUserType
              )
              val newDraftReturn                  = draftReturn.copy(
                acquisitionDetailsAnswers = Some(completeAnswers)
              )
              val updatedJourney                  = journey.copy(draftReturn = newDraftReturn)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(updatedJourney)(Right(()))
                mockStoreSession(
                  session.copy(journeyStatus = Some(updatedJourney))
                )(Right(()))
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionDetails.cya.title"),
                doc =>
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action") shouldBe routes.AcquisitionDetailsController
                    .checkYourAnswersSubmit()
                    .url
              )
          }
        }

        "the user has already answered all the questions and is non-uk and rebasing" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val nonUkRebasing = CompleteAcquisitionDetailsAnswers(
                sample[AcquisitionMethod],
                AcquisitionDate(
                  nonUkResidentsNonResidentialProperty.minusDays(1)
                ),
                sample[AmountInPence],
                Some(sample[AmountInPence]),
                sample[AmountInPence],
                sample[AmountInPence],
                shouldUseRebase = false
              )
              val assetType     = AssetType.NonResidential
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    nonUkRebasing,
                    assetType,
                    wasUkResident = false,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionDetails.cya.title"),
                { doc =>
                  validateAcquisitionDetailsCheckYourAnswersPage(
                    nonUkRebasing,
                    doc,
                    isUk = false,
                    isRebasing = true,
                    assetType
                  )
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action") shouldBe routes.AcquisitionDetailsController
                    .checkYourAnswersSubmit()
                    .url
                }
              )
          }
        }

        "the user has already answered all the questions and is uk and rebasing" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val nonUkRebasing = CompleteAcquisitionDetailsAnswers(
                sample[AcquisitionMethod],
                AcquisitionDate(allResidents.minusDays(2)),
                sample[AmountInPence],
                Some(sample[AmountInPence]),
                sample[AmountInPence],
                sample[AmountInPence],
                shouldUseRebase = true
              )
              val assetType     = AssetType.NonResidential
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    nonUkRebasing,
                    assetType,
                    wasUkResident = true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionDetails.cya.title"),
                { doc =>
                  validateAcquisitionDetailsCheckYourAnswersPage(
                    nonUkRebasing,
                    doc,
                    isUk = true,
                    isRebasing = true,
                    assetType
                  )
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action") shouldBe routes.AcquisitionDetailsController
                    .checkYourAnswersSubmit()
                    .url
                }
              )
          }
        }

        "the user has already answered all the questions and is uk and not rebasing" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val nonUkRebasing = CompleteAcquisitionDetailsAnswers(
                sample[AcquisitionMethod],
                AcquisitionDate(allResidents.plusDays(1)),
                sample[AmountInPence],
                Some(sample[AmountInPence]),
                sample[AmountInPence],
                sample[AmountInPence],
                shouldUseRebase = true
              )
              val assetType     = AssetType.NonResidential
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    nonUkRebasing,
                    assetType,
                    wasUkResident = true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionDetails.cya.title"),
                { doc =>
                  validateAcquisitionDetailsCheckYourAnswersPage(
                    nonUkRebasing,
                    doc,
                    isUk = true,
                    isRebasing = false,
                    assetType
                  )
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action") shouldBe routes.AcquisitionDetailsController
                    .checkYourAnswersSubmit()
                    .url
                }
              )
          }
        }

        "the user has already answered all the questions and is not uk and not rebasing" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val nonUkRebasing = CompleteAcquisitionDetailsAnswers(
                sample[AcquisitionMethod],
                AcquisitionDate(
                  nonUkResidentsNonResidentialProperty.plusDays(1)
                ),
                sample[AmountInPence],
                Some(sample[AmountInPence]),
                sample[AmountInPence],
                sample[AmountInPence],
                shouldUseRebase = false
              )
              val assetType     = AssetType.NonResidential
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    nonUkRebasing,
                    assetType,
                    wasUkResident = false,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionDetails.cya.title"),
                { doc =>
                  validateAcquisitionDetailsCheckYourAnswersPage(
                    nonUkRebasing,
                    doc,
                    isUk = false,
                    isRebasing = false,
                    assetType
                  )
                  doc
                    .select("#content > article > form, #main-content form")
                    .attr("action") shouldBe routes.AcquisitionDetailsController
                    .checkYourAnswersSubmit()
                    .url
                }
              )
          }
        }

        "display correct questions on check your answers" when {

          "The question for the acquisition on the cya is correct for all acquisition methods" in {
            val date = LocalDate.now()
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                val userKey = userMessageKey(individualUserType, userType)
                List(
                  (
                    AcquisitionMethod.Bought,
                    messages(s"acquisitionPriceBought$userKey.title")
                  ),
                  (
                    AcquisitionMethod.Inherited,
                    messages(
                      s"acquisitionPriceNotBought$userKey.title",
                      TimeUtils.govDisplayFormat(date)
                    )
                  ),
                  (
                    AcquisitionMethod.Gifted,
                    messages(
                      s"acquisitionPriceNotBought$userKey.title",
                      TimeUtils.govDisplayFormat(date)
                    )
                  ),
                  (
                    AcquisitionMethod.Other("test"),
                    messages(
                      s"acquisitionPriceNotBought$userKey.title",
                      TimeUtils.govDisplayFormat(date)
                    )
                  )
                ).foreach { case (method, expectedTitle) =>
                  withClue(s"For $method and $expectedTitle") {
                    val nonUkRebasing = CompleteAcquisitionDetailsAnswers(
                      method,
                      AcquisitionDate(date),
                      sample[AmountInPence],
                      Some(sample[AmountInPence]),
                      sample[AmountInPence],
                      sample[AmountInPence],
                      shouldUseRebase = false
                    )
                    val assetType     = AssetType.NonResidential
                    inSequence {
                      mockAuthWithNoRetrievals()
                      mockGetSession(
                        sessionWithState(
                          nonUkRebasing,
                          assetType,
                          wasUkResident = false,
                          userType,
                          individualUserType
                        )._1
                      )
                    }

                    checkPageIsDisplayed(
                      performAction(),
                      messageFromMessageKey("acquisitionDetails.cya.title"),
                      doc =>
                        doc
                          .select("#acquisitionPrice-question")
                          .text() shouldBe expectedTitle
                    )
                  }
                }
            }
          }

        }

      }

    }

    "handling submits on the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      "redirect to the task list" in {
        forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
          (userType: UserType, individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithState(
                  sample[CompleteAcquisitionDetailsAnswers],
                  sample[AssetType],
                  sample[Boolean],
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

    "handling requests to display the improvement costs page for agent" must {

      val key = "improvementCosts"

      def performAction(): Future[Result] =
        controller.improvementCosts()(FakeRequest())

      "display the page" when {

        "the agent's client meets the rebasing criteria and their acquisition details journey is incomplete" in {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithStateForAgent(
              sample[IncompleteAcquisitionDetailsAnswers].copy(
                acquisitionDate = Some(AcquisitionDate(allResidents.minusDays(1L))),
                rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                shouldUseRebase = Some(false)
              ),
              Some(AssetType.Residential),
              wasUkResident = Some(false)
            )._1
          )

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.5.title"),
            doc =>
              doc
                .select("#content > article > form, #main-content form")
                .attr("action") shouldBe routes.AcquisitionDetailsController
                .improvementCostsSubmit()
                .url
          )
        }

      }

    }

    "handling requests to display the acquisition price page for agent" must {

      val key = "acquisitionPrice"

      def performAction(): Future[Result] =
        controller.acquisitionPrice()(FakeRequest())

      "display the page" when {

        "the agent's client meets the rebasing criteria and their acquisition details journey is incomplete" in {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithStateForAgent(
              sample[IncompleteAcquisitionDetailsAnswers].copy(
                acquisitionMethod = Some(Inherited),
                acquisitionDate = Some(AcquisitionDate(allResidents)),
                rebasedAcquisitionPrice = None,
                shouldUseRebase = Some(false)
              ),
              Some(AssetType.Residential),
              wasUkResident = None
            )._1
          )

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.8.title", TimeUtils.govDisplayFormat(allResidents)),
            doc =>
              doc
                .select("#content > article > form, #main-content form")
                .attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionPrice()
                .url
          )
        }

      }

    }

    "handling requests to display the acquisition cost(fee) page for agent" must {

      val key = "acquisitionFees"

      def performAction(): Future[Result] =
        controller.acquisitionFees()(FakeRequest())

      "display the page" when {

        "the agent's client meets the rebasing criteria and their acquisition details journey is incomplete" in {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithStateForAgent(
              sample[IncompleteAcquisitionDetailsAnswers].copy(
                acquisitionDate = Some(AcquisitionDate(allResidents.minusDays(1L))),
                rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                shouldUseRebase = Some(true)
              ),
              Some(AssetType.Residential),
              wasUkResident = Some(false)
            )._1
          )

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.6.title"),
            doc =>
              doc
                .select("#content > article > form, #main-content form")
                .attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionFees()
                .url
          )
        }

      }

    }

    def missingAssetTypeAndResidentialStatusBehaviour(
      performAction: () => Future[Result]
    ): Unit =
      "redirect to the task list page" when {

        "there is no asset type" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    Some(sample[CompleteAcquisitionDetailsAnswers]),
                    None,
                    Some(sample[Boolean]),
                    userType,
                    individualUserType,
                    Some(sample[DisposalDate]),
                    Some(
                      sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath]))
                    ),
                    isAmend = false
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                controllers.returns.routes.TaskListController.taskList()
              )
          }
        }

        "there is no residential status" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    Some(sample[CompleteAcquisitionDetailsAnswers]),
                    Some(sample[AssetType]),
                    None,
                    userType,
                    individualUserType,
                    Some(sample[DisposalDate]),
                    Some(
                      sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath]))
                    ),
                    isAmend = false
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

    def missingAcquisitionDateBehaviour(performAction: () => Future[Result]): Unit =
      "redirect to the check you answers page" when {

        "there is no acquisition date" in {
          val completeAcquisitionDetailsAnswers =
            sample[CompleteAcquisitionDetailsAnswers]
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    Some(
                      IncompleteAcquisitionDetailsAnswers(
                        Some(
                          completeAcquisitionDetailsAnswers.acquisitionMethod
                        ),
                        None,
                        Some(
                          completeAcquisitionDetailsAnswers.acquisitionPrice
                        ),
                        completeAcquisitionDetailsAnswers.rebasedAcquisitionPrice,
                        Some(
                          completeAcquisitionDetailsAnswers.improvementCosts
                        ),
                        Some(completeAcquisitionDetailsAnswers.acquisitionFees),
                        Some(completeAcquisitionDetailsAnswers.shouldUseRebase)
                      )
                    ),
                    Some(sample[AssetType]),
                    Some(sample[Boolean]),
                    userType,
                    individualUserType,
                    Some(sample[DisposalDate]),
                    Some(
                      sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath]))
                    ),
                    isAmend = false
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.checkYourAnswers()
              )
          }
        }

      }

    def missingAcquisitionMethodBehaviour(performAction: () => Future[Result]): Unit =
      "redirect to the check you answers page" when {

        "there is no acquisition method" in {
          val completeAcquisitionDetailsAnswers =
            sample[CompleteAcquisitionDetailsAnswers]
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    Some(
                      IncompleteAcquisitionDetailsAnswers(
                        None,
                        Some(completeAcquisitionDetailsAnswers.acquisitionDate),
                        Some(
                          completeAcquisitionDetailsAnswers.acquisitionPrice
                        ),
                        completeAcquisitionDetailsAnswers.rebasedAcquisitionPrice,
                        Some(
                          completeAcquisitionDetailsAnswers.improvementCosts
                        ),
                        Some(completeAcquisitionDetailsAnswers.acquisitionFees),
                        Some(completeAcquisitionDetailsAnswers.shouldUseRebase)
                      )
                    ),
                    Some(sample[AssetType]),
                    Some(sample[Boolean]),
                    userType,
                    individualUserType,
                    Some(sample[DisposalDate]),
                    Some(
                      sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(sample[DateOfDeath]))
                    ),
                    isAmend = false
                  )._1
                )
              }

              checkIsRedirect(
                performAction(),
                routes.AcquisitionDetailsController.checkYourAnswers()
              )
          }
        }

      }

  }

  def testFormError(data: (String, String)*)(
    userType: UserType,
    individualUserType: IndividualUserType,
    assetType: AssetType,
    wasUkResident: Boolean = sample[Boolean]
  )(
    expectedErrorMessageKey: String,
    errorArgs: String*
  )(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: Option[SessionData] = None
  ): Unit = {
    val session = currentSession.getOrElse(
      sessionWithState(
        sample[CompleteAcquisitionDetailsAnswers].copy(
          rebasedAcquisitionPrice = Some(sample[AmountInPence]),
          shouldUseRebase = false
        ),
        assetType,
        wasUkResident,
        userType,
        individualUserType
      )._1
    )

    inSequence {
      mockAuthWithNoRetrievals()
      mockGetSession(session)
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

}

object AcquisitionDetailsControllerSpec extends Matchers {

  def validateAcquisitionDetailsCheckYourAnswersPage(
    acquisitionDetailsAnswers: CompleteAcquisitionDetailsAnswers,
    doc: Document,
    isUk: Boolean,
    isRebasing: Boolean,
    assetType: AssetType
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    val expectedAcquisitionMethodDisplayName =
      acquisitionDetailsAnswers.acquisitionMethod match {
        case AcquisitionMethod.Bought       =>
          messages("returns.acquisitionMethod.Bought")
        case AcquisitionMethod.Inherited    =>
          messages("returns.acquisitionMethod.Inherited")
        case AcquisitionMethod.Gifted       =>
          messages("returns.acquisitionMethod.Gifted")
        case AcquisitionMethod.Other(value) => value
      }

    doc
      .select("#acquisitionMethod-answer")
      .text() shouldBe expectedAcquisitionMethodDisplayName

    if (!isRebasing || !isUk) {
      doc
        .select("#acquisitionPrice-answer")
        .text() shouldBe formatAmountOfMoneyWithPoundSign(
        acquisitionDetailsAnswers.acquisitionPrice.inPounds()
      )
    } else {
      doc.select("#acquisitionPrice-answer").text() shouldBe ""
    }

    if (!(assetType === IndirectDisposal) || isUk || (!isUk && !isRebasing)) {
      if (acquisitionDetailsAnswers.improvementCosts === AmountInPence.zero) {
        doc.select("#improvementCosts-answer").text shouldBe "No"
      } else {
        doc.select("#improvementCosts-answer").text shouldBe "Yes"
        doc
          .select("#improvementCosts-value-answer")
          .text                                     shouldBe formatAmountOfMoneyWithPoundSign(
          acquisitionDetailsAnswers.improvementCosts.inPounds()
        )
      }
    }

    if (acquisitionDetailsAnswers.acquisitionFees === AmountInPence.zero) {
      doc.select("#acquisitionFees-answer").text shouldBe "No"
    } else {
      doc.select("#acquisitionFees-answer").text shouldBe "Yes"
      doc
        .select("#acquisitionFees-value-answer")
        .text                                    shouldBe formatAmountOfMoneyWithPoundSign(
        acquisitionDetailsAnswers.acquisitionFees.inPounds()
      )
    }

    acquisitionDetailsAnswers.rebasedAcquisitionPrice.foreach { rebasedAcquisitionPrice =>
      if (!isUk && isRebasing) {
        if (acquisitionDetailsAnswers.shouldUseRebase) {
          doc.select("#shouldRebase-answer").text shouldBe "Yes"
          doc
            .select("#rebasedAcquisitionPrice-value-answer")
            .text                                 shouldBe formatAmountOfMoneyWithPoundSign(
            rebasedAcquisitionPrice.inPounds()
          )
        } else {
          doc.select("#shouldRebase-answer").text shouldBe "No"
        }
      }
    }
  }
}
