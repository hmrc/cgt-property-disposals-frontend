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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails

import java.time.LocalDate

import org.jsoup.nodes.Document
import org.scalacheck.Gen
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status.BAD_REQUEST
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.RebasingCutoffDates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.RebasingCutoffDates._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.DateErrorScenarios._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.acquisitiondetails.AcquisitionDetailsControllerSpec.validateAcquisitionDetailsCheckYourAnswersPage
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.formatAmountOfMoneyWithPoundSign
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.{CompleteAcquisitionDetailsAnswers, IncompleteAcquisitionDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AcquisitionMethod, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class AcquisitionDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with ScalaCheckDrivenPropertyChecks
    with RedirectToStartBehaviour {
  lazy val controller  = instanceOf[AcquisitionDetailsController]
  val mockRebasingUtil = new RebasingEligibilityUtil()
  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[ReturnsService].toInstance(mockReturnsService),
      bind[RebasingEligibilityUtil].toInstance(mockRebasingUtil)
    )

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  implicit lazy val messages: Messages = MessagesImpl(Lang("en"), messagesApi)

  def userMessageKey(individualUserType: IndividualUserType, userType: UserType): String =
    (individualUserType, userType) match {
      case (Capacitor, _)              => ".capacitor"
      case (PersonalRepresentative, _) => ".personalRep"
      case (_, UserType.Individual)    => ""
      case (_, UserType.Organisation)  => ".trust"
      case (_, UserType.Agent)         => ".agent"
      case other                       => sys.error(s"User type '$other' not handled")
    }

  def setAgentReferenceNumber(userType: UserType): Option[AgentReferenceNumber] = userType match {
    case UserType.Agent => Some(sample[AgentReferenceNumber])
    case _              => None
  }

  def setNameForUserType(userType: UserType): Either[TrustName, IndividualName] = userType match {
    case UserType.Organisation => Left(sample[TrustName])
    case _                     => Right(sample[IndividualName])
  }

  def redirectToStartBehaviour(performAction: () => Future[Result]) =
    redirectToStartWhenInvalidJourney(
      performAction, {
        case _: FillingOutReturn => true
        case _                   => false
      }
    )

  def sessionWithState(
    answers: AcquisitionDetailsAnswers,
    assetType: AssetType,
    wasUkResident: Boolean,
    userType: UserType,
    individualUserType: IndividualUserType,
    disposalDate: DisposalDate = sample[DisposalDate]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) =
    sessionWithState(
      Some(answers),
      Some(assetType),
      Some(wasUkResident),
      userType,
      individualUserType,
      Some(disposalDate)
    )

  def sessionWithState(
    answers: Option[AcquisitionDetailsAnswers],
    assetType: Option[AssetType],
    wasUkResident: Option[Boolean],
    userType: UserType,
    individualUserType: IndividualUserType,
    disposalDate: Option[DisposalDate]
  ): (SessionData, FillingOutReturn, DraftSingleDisposalReturn) = {

    val draftReturn = sample[DraftSingleDisposalReturn].copy(
      triageAnswers = sample[IncompleteSingleDisposalTriageAnswers].copy(
        assetType          = assetType,
        wasAUKResident     = wasUkResident,
        disposalDate       = disposalDate,
        individualUserType = Some(individualUserType)
      ),
      acquisitionDetailsAnswers = answers
    )

    val journey = sample[FillingOutReturn].copy(
      draftReturn          = draftReturn,
      agentReferenceNumber = setAgentReferenceNumber(userType),
      subscribedDetails = sample[SubscribedDetails].copy(
        name = setNameForUserType(userType)
      )
    )

    val sessionData = SessionData.empty.copy(
      userType      = Some(userType),
      journeyStatus = Some(journey)
    )

    (sessionData, journey, draftReturn)
  }

  def commonUpdateDraftReturn(
    d: DraftSingleDisposalReturn,
    newAnswers: AcquisitionDetailsAnswers
  ) =
    d.copy(
      acquisitionDetailsAnswers = Some(newAnswers),
      initialGainOrLoss         = None,
      reliefDetailsAnswers = d.reliefDetailsAnswers.map(
        _.unset(_.privateResidentsRelief).unset(_.lettingsRelief)
      ),
      yearToDateLiabilityAnswers = d.yearToDateLiabilityAnswers.flatMap(_.unsetAllButIncomeDetails())
    )

  val acceptedUserTypeGen: Gen[UserType] = userTypeGen.filter {
    case UserType.Agent | UserType.Organisation | UserType.Individual => true
    case _                                                            => false
  }

  val acceptedIndividualUserTypeGen: Gen[IndividualUserType] = individualUserTypeGen.filter {
    case Self | Capacitor | PersonalRepresentative => true
    case _                                         => false
  }

  "AcquisitionDetailsController" when {

    "handling requests to display the acquisition method page" must {

      def performAction(): Future[Result] = controller.acquisitionMethod()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      val key = "acquisitionMethod"

      "display the page" when {

        "the user has not completed the acquisition details section of the return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              List(Some(IncompleteAcquisitionDetailsAnswers.empty), None).foreach { answers =>
                withClue(s"For answers $answers: ") {
                  inSequence {
                    mockAuthWithNoRetrievals()
                    mockGetSession(
                      sessionWithState(answers, None, None, userType, individualUserType, Some(sample[DisposalDate]))._1
                    )
                  }

                  val userMsgKey = userMessageKey(individualUserType, userType)

                  checkPageIsDisplayed(
                    performAction(),
                    messageFromMessageKey(s"$key$userMsgKey.title"), { doc =>
                      doc.select("#back").attr("href") shouldBe controllers.returns.routes.TaskListController
                        .taskList()
                        .url
                      doc
                        .select("#content > article > form")
                        .attr("action") shouldBe routes.AcquisitionDetailsController
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
              List(Some(IncompleteAcquisitionDetailsAnswers.empty), None).foreach { answers =>
                withClue(s"For answers $answers: ") {
                  inSequence {
                    mockAuthWithNoRetrievals()
                    mockGetSession(
                      sessionWithState(
                        sample[CompleteAcquisitionDetailsAnswers],
                        sample[AssetType],
                        sample[Boolean],
                        userType,
                        individualUserType,
                        sample[DisposalDate]
                      )._1
                    )
                  }

                  val userKey = userMessageKey(individualUserType, userType)

                  checkPageIsDisplayed(
                    performAction(),
                    messageFromMessageKey(s"$key$userKey.title"), { doc =>
                      doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController
                        .checkYourAnswers()
                        .url
                      doc
                        .select("#content > article > form")
                        .attr("action") shouldBe routes.AcquisitionDetailsController
                        .acquisitionMethodSubmit()
                        .url
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
        controller.acquisitionMethodSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      val key      = "acquisitionMethod"
      val otherKey = "otherAcquisitionMethod"

      "show a form error" when {

        def test(data: (String, String)*)(
          expectedErrorMessageKey: String
        )(userType: UserType, individualUserType: IndividualUserType, userKey: String): Unit =
          testFormError(data: _*)(userType, individualUserType)(expectedErrorMessageKey)(
            s"$key$userKey.title"
          )(performAction)

        "nothing is submitted" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test()(s"$key$userKey.error.required")(userType, individualUserType, userKey)
          }
        }

        "an unknown value is submitted" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test("acquisitionMethod" -> "4")(s"$key$userKey.error.required")(userType, individualUserType, userKey)
          }
        }

        "other is selected with a value" that {

          "the user enters acquisition method that doesn't exist" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                val userKey = userMessageKey(individualUserType, userType)
                test(key -> "3")(s"$otherKey$userKey.error.required")(userType, individualUserType, userKey)
            }
          }

          "the user enters acquisition method that is empty" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                val userKey = userMessageKey(individualUserType, userType)
                test(key -> "3", otherKey -> "")(s"$otherKey$userKey.error.required")(
                  userType,
                  individualUserType,
                  userKey
                )
            }
          }

          "the user enters acquisition method that contains invalid characters" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                val userKey = userMessageKey(individualUserType, userType)
                test(key -> "3", otherKey -> "1,234")(
                  s"$otherKey$userKey.error.invalid"
                )(userType, individualUserType, userKey)
            }
          }

          "the  user enters acquisition method that is too long" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                val userKey = userMessageKey(individualUserType, userType)
                test(key -> "3", otherKey -> ("a" * 36))(
                  s"$otherKey$userKey.error.tooLong"
                )(userType, individualUserType, userKey)
            }
          }

        }

      }

      "show an error page" when {

        val (method, methodValue) = AcquisitionMethod.Bought -> 0

        def getSessionDataJourneyAndDraftReturn(
          userType: UserType,
          individualUserType: IndividualUserType
        ): (SessionData, FillingOutReturn, DraftReturn) = {
          val (session, journey, draftReturn) = sessionWithState(None, None, None, userType, individualUserType, None)
          val updatedDraftReturn = commonUpdateDraftReturn(
            draftReturn,
            IncompleteAcquisitionDetailsAnswers.empty.copy(
              acquisitionMethod = Some(method)
            )
          )
          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(userType, individualUserType)
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction(key -> methodValue.toString))
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(userType, individualUserType)
              val updatedSession = session.copy(
                journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
              )
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Right(()))
                mockStoreSession(updatedSession)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction(key -> methodValue.toString))
          }
        }

      }

      "redirect to the check your answers page" when {

        "the acquisition details journey is incomplete and" when {

          def test(
            data: (String, String)*
          )(method: AcquisitionMethod)(userType: UserType, individualUserType: IndividualUserType): Unit = {
            val (session, journey, draftReturn) = sessionWithState(None, None, None, userType, individualUserType, None)
            val updatedDraftReturn = commonUpdateDraftReturn(
              draftReturn,
              IncompleteAcquisitionDetailsAnswers.empty.copy(acquisitionMethod = Some(method))
            )
            val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

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

            checkIsRedirect(performAction(data: _*), routes.AcquisitionDetailsController.checkYourAnswers())
          }

          "the user selects bought it" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(key -> "0")(AcquisitionMethod.Bought)(userType, individualUserType)
            }
          }

          "the user selects inherited it" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(key -> "1")(AcquisitionMethod.Inherited)(userType, individualUserType)
            }
          }

          "the user selects was gifted it" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test("acquisitionMethod" -> "2")(AcquisitionMethod.Gifted)(userType, individualUserType)
            }
          }

          "the user selects other" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(key -> "3", otherKey -> "things")(AcquisitionMethod.Other("things"))(userType, individualUserType)
            }
          }

        }

        "the acquisition details journey is complete and" when {

          def test(data: (String, String)*)(
            oldMethod: AcquisitionMethod,
            method: AcquisitionMethod
          )(userType: UserType, individualUserType: IndividualUserType): Unit = {
            val answers = sample[CompleteAcquisitionDetailsAnswers].copy(acquisitionMethod = oldMethod)
            val (session, journey, draftReturn) = sessionWithState(
              answers,
              sample[AssetType],
              sample[Boolean],
              userType,
              individualUserType
            )
            val updatedAnswers = IncompleteAcquisitionDetailsAnswers(
              Some(method),
              Some(answers.acquisitionDate),
              None,
              None,
              Some(answers.improvementCosts),
              None,
              None
            )

            val updatedDraftReturn = commonUpdateDraftReturn(draftReturn, updatedAnswers)
            val updatedSession = session.copy(
              journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
            )
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

            checkIsRedirect(performAction(data: _*), routes.AcquisitionDetailsController.checkYourAnswers())
          }

          "the user selects bought it" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(key -> "0")(AcquisitionMethod.Inherited, AcquisitionMethod.Bought)(userType, individualUserType)
            }
          }

          "the user selects inherited it" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(key -> "1")(AcquisitionMethod.Bought, AcquisitionMethod.Inherited)(userType, individualUserType)
            }
          }

          "the user selects was gifted it" in {
            forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
              (userType: UserType, individualUserType: IndividualUserType) =>
                test(key -> "2")(AcquisitionMethod.Bought, AcquisitionMethod.Gifted)(userType, individualUserType)
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
              val (incompleteAnswers, completeAnswers) = sample[IncompleteAcquisitionDetailsAnswers] -> sample[
                CompleteAcquisitionDetailsAnswers
              ]
              List(
                Seq(key -> "0") -> incompleteAnswers.copy(acquisitionMethod = Some(AcquisitionMethod.Bought)),
                Seq(key -> "1") -> incompleteAnswers.copy(acquisitionMethod = Some(AcquisitionMethod.Inherited)),
                Seq(key -> "2") -> incompleteAnswers.copy(acquisitionMethod = Some(AcquisitionMethod.Gifted)),
                Seq(key -> "3", otherKey -> "other things") -> incompleteAnswers
                  .copy(acquisitionMethod = Some(AcquisitionMethod.Other("other things"))),
                Seq(key -> "0") -> completeAnswers.copy(acquisitionMethod = AcquisitionMethod.Bought),
                Seq(key -> "1") -> completeAnswers.copy(acquisitionMethod = AcquisitionMethod.Inherited),
                Seq(key -> "2") -> completeAnswers.copy(acquisitionMethod = AcquisitionMethod.Gifted),
                Seq(key -> "3", otherKey -> "other things") -> completeAnswers
                  .copy(acquisitionMethod = AcquisitionMethod.Other("other things"))
              ).foreach {
                case (data, answers) =>
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

                  checkIsRedirect(performAction(data: _*), routes.AcquisitionDetailsController.checkYourAnswers())
              }
          }
        }

      }

    }

    "handling requests to display the acquisition date page" must {

      def performAction(): Future[Result] = controller.acquisitionDate()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

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
                    None
                  )._1
                )
              }

              checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
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
                    sample[IncompleteAcquisitionDetailsAnswers].copy(acquisitionMethod = None),
                    sample[AssetType],
                    sample[Boolean],
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkIsRedirect(performAction(), routes.AcquisitionDetailsController.acquisitionMethod())
          }
        }

      }

      "display the page" when {

        "the user has not yet completed the acquisition details section" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionMethod = Some(AcquisitionMethod.Bought)
                    ),
                    sample[AssetType],
                    sample[Boolean],
                    userType,
                    individualUserType
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionMethod().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionDateSubmit()
                    .url
                }
              )
          }
        }

        "the user has already completed the acquisition details section" in {
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

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionDateSubmit()
                    .url
                }
              )
          }
        }

      }

    }

    "handling submitted answers to the acquisition date page" must {

      val key = "acquisitionDate"

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionDateSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      def formData(date: LocalDate): List[(String, String)] =
        List(
          "acquisitionDate-day"   -> date.getDayOfMonth.toString,
          "acquisitionDate-month" -> date.getMonthValue.toString,
          "acquisitionDate-year"  -> date.getYear.toString
        )

      val disposalDate = DisposalDate(LocalDate.of(2020, 1, 1), sample[TaxYear])

      behave like redirectToStartBehaviour(() => performAction())

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
                    None
                  )._1
                )
              }

              checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
          }
        }

      }

      "show a form error" when {

        def test(
          data: (String, String)*
        )(userType: UserType, individualUserType: IndividualUserType, userKey: String)(expectedErrorKey: String): Unit =
          testFormError(data: _*)(userType, individualUserType)(expectedErrorKey)(s"$key$userKey.title")(
            performAction,
            sessionWithState(
              sample[CompleteAcquisitionDetailsAnswers],
              sample[AssetType],
              sample[Boolean],
              userType,
              individualUserType,
              disposalDate
            )._1
          )

        "individual enters date that is invalid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              dateErrorScenarios(key, userKey).foreach {
                case d @ DateErrorScenario(dayString, monthString, yearString, expectedErrorKey) =>
                  withClue(s"For $d: ") {
                    val formData =
                      List(
                        "acquisitionDate-day"   -> dayString,
                        "acquisitionDate-month" -> monthString,
                        "acquisitionDate-year"  -> yearString
                      ).collect { case (id, Some(input)) => id -> input }

                    test(formData: _*)(userType, individualUserType, userKey)(expectedErrorKey)
                  }
              }
          }
        }

        "individual enters date that is after the disposal date" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey  = userMessageKey(individualUserType, userType)
              val tomorrow = disposalDate.value.plusDays(1L)
              test(formData(tomorrow): _*)(userType, individualUserType, userKey)(s"$key$userKey.error.tooFarInFuture")
          }
        }

      }

      "show an error page" when {

        val acquisitionDate = AcquisitionDate(disposalDate.value)

        def getSessionDataJourneyAndDraftReturn(userType: UserType, individualUserType: IndividualUserType) = {

          val answers = sample[CompleteAcquisitionDetailsAnswers].copy(
            acquisitionDate = AcquisitionDate(acquisitionDate.value.plusDays(1L))
          )

          val wasUkResident = sample[Boolean]
          val (session, journey, draftReturn) =
            sessionWithState(answers, sample[AssetType], wasUkResident, userType, individualUserType, disposalDate)

          val newAnswers =
            IncompleteAcquisitionDetailsAnswers(
              Some(answers.acquisitionMethod),
              Some(acquisitionDate),
              None,
              None,
              None,
              None,
              None
            )

          val updatedDraftReturn = commonUpdateDraftReturn(draftReturn, newAnswers)

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(userType, individualUserType)
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction(formData(acquisitionDate.value): _*))
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(userType, individualUserType)
              val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Right(()))
                mockStoreSession(updatedSession)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction(formData(acquisitionDate.value): _*))
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
          individualUserType: IndividualUserType
        ): Unit = {
          val (session, journey, draftReturn) = sessionWithState(
            oldAnswers,
            assetType,
            wasUkResident,
            userType,
            individualUserType,
            disposalDate
          )

          val updatedDraftReturn = commonUpdateDraftReturn(draftReturn, newAnswers)
          val updatedSession = session.copy(
            journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
          )

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
            performAction(formData(submittedAcquisitionDate): _*),
            routes.AcquisitionDetailsController.checkYourAnswers()
          )
        }

        "the user has not answered this question before" in {
          val date = disposalDate.value
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
                individualUserType
              )
          }
        }

        "the user has completed this section before" in {
          val date = disposalDate.value
          val oldAnswers = sample[CompleteAcquisitionDetailsAnswers].copy(
            acquisitionDate = AcquisitionDate(date.minusDays(1L))
          )
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
                sample[AssetType],
                sample[Boolean],
                date,
                oldAnswers,
                newAnswers,
                userType,
                individualUserType
              )
          }
        }

      }

    }

    "handling requests to display the acquisition price page" must {

      def performAction(): Future[Result] = controller.acquisitionPrice()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      behave like missingAcquisitionDateBehaviour(performAction)

      behave like missingAcquisitionMethodBehaviour(performAction)

      "display the page" when {

        def incompleteSection(userType: UserType, individualUserType: IndividualUserType, userKey: String): Unit =
          forAll { acquisitionMethod: AcquisitionMethod =>
            val acquisitionDate = sample[AcquisitionDate]
            val answers = sample[IncompleteAcquisitionDetailsAnswers].copy(
              acquisitionMethod = Some(acquisitionMethod),
              acquisitionDate   = Some(acquisitionDate)
            )

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

            val pricePageTitle = acquisitionMethod match {
              case AcquisitionMethod.Bought =>
                messageFromMessageKey(s"acquisitionPriceBought$userKey.title")
              case _ =>
                messageFromMessageKey(
                  s"acquisitionPriceNotBought$userKey.title",
                  TimeUtils.govDisplayFormat(acquisitionDate.value)
                )
            }

            checkPageIsDisplayed(
              performAction(),
              pricePageTitle, { doc =>
                doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionDate().url
                doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                  .acquisitionPriceSubmit()
                  .url
              }
            )
          }

        "the user has not yet completed the acquisition details section" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              incompleteSection(userType, individualUserType, userKey)
          }
        }

        def sectionCompleted(userType: UserType, individualUserType: IndividualUserType, userKey: String): Unit = {
          val answers = sample[CompleteAcquisitionDetailsAnswers]
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

          val pricePageTitle = answers.acquisitionMethod match {
            case AcquisitionMethod.Bought =>
              messageFromMessageKey(s"acquisitionPriceBought$userKey.title")
            case _ =>
              messageFromMessageKey(
                s"acquisitionPriceNotBought$userKey.title",
                TimeUtils.govDisplayFormat(answers.acquisitionDate.value)
              )
          }

          checkPageIsDisplayed(
            performAction(),
            pricePageTitle, { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .acquisitionPriceSubmit()
                .url
            }
          )
        }

        "the user has already completed the acquisition details section" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              sectionCompleted(userType, individualUserType, userKey)
          }
        }

      }

    }

    "handling submitted acquisition prices" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionPriceSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      behave like missingAcquisitionDateBehaviour(() => performAction())

      behave like missingAcquisitionMethodBehaviour(() => performAction())

      "show a form error" when {

        def invalidPrice(userType: UserType, individualUserType: IndividualUserType, userKey: String): Unit =
          forAll { answers: CompleteAcquisitionDetailsAnswers =>
            val scenarioSession = sessionWithState(
              answers,
              sample[AssetType],
              sample[Boolean],
              userType,
              individualUserType
            )._1

            val contextKey = answers.acquisitionMethod match {
              case AcquisitionMethod.Bought => s"acquisitionPriceBought$userKey"
              case _                        => s"acquisitionPriceNotBought$userKey"
            }

            amountOfMoneyErrorScenarios(s"acquisitionPrice", errorContext = Some(contextKey))
              .foreach { scenario =>
                withClue(s"For $scenario: ") {
                  testFormError(scenario.formData: _*)(userType, individualUserType)(scenario.expectedErrorMessageKey)(
                    s"$contextKey.title",
                    TimeUtils.govDisplayFormat(answers.acquisitionDate.value)
                  )(
                    performAction,
                    scenarioSession
                  )
                }
              }
          }

        "the user provides an invalid data" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              invalidPrice(userType, individualUserType, userKey)
          }
        }

        def amountZero(userType: UserType, individualUserType: IndividualUserType, userKey: String): Unit =
          forAll { answers: CompleteAcquisitionDetailsAnswers =>
            val scenarioSession = sessionWithState(
              answers,
              sample[AssetType],
              sample[Boolean],
              userType,
              individualUserType
            )._1

            val contextKey = answers.acquisitionMethod match {
              case AcquisitionMethod.Bought => s"acquisitionPriceBought$userKey"
              case _                        => s"acquisitionPriceNotBought$userKey"
            }
            testFormError("acquisitionPrice" -> "0")(userType, individualUserType)(
              s"$contextKey.error.tooSmall"
            )(
              s"$contextKey.title",
              TimeUtils.govDisplayFormat(answers.acquisitionDate.value)
            )(
              performAction,
              scenarioSession
            )
          }

        "the user enters zero for amount" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              amountZero(userType, individualUserType, userKey)
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
            acquisitionDate   = Some(sample[AcquisitionDate])
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
            answers.copy(acquisitionPrice = Some(AmountInPence(123L)))
          )

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftSession(userType, individualUserType)
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction("acquisitionPrice" -> price.toString))
          }
        }

        "there is an error updating the session" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftSession(userType, individualUserType)
              val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Right(()))
                mockStoreSession(updatedSession)(Left(Error("")))
              }

              checkIsTechnicalErrorPage(performAction("acquisitionPrice" -> price.toString))
          }
        }

      }

      "redirect to the check you answers page" when {

        "the price submitted is valid and the journey was incomplete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
                acquisitionMethod = Some(sample[AcquisitionMethod]),
                acquisitionDate   = Some(sample[AcquisitionDate])
              )
              val (session, journey, draftReturn) =
                sessionWithState(answers, sample[AssetType], sample[Boolean], userType, individualUserType)

              val updatedDraftReturn = commonUpdateDraftReturn(
                draftReturn,
                answers.copy(acquisitionPrice = Some(AmountInPence(123400L)))
              )

              val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

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
                performAction("acquisitionPrice" -> "£1,234"),
                routes.AcquisitionDetailsController.checkYourAnswers()
              )
          }
        }

        "the price submitted is valid and the journey was complete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val answers = sample[CompleteAcquisitionDetailsAnswers]
              val (session, journey, draftReturn) = sessionWithState(
                answers,
                sample[AssetType],
                sample[Boolean],
                userType,
                individualUserType
              )
              val updatedDraftReturn = commonUpdateDraftReturn(
                draftReturn,
                answers.copy(acquisitionPrice = AmountInPence(123456L))
              )

              val updatedSession = session.copy(
                journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
              )

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
                performAction("acquisitionPrice" -> "£1,234.56"),
                routes.AcquisitionDetailsController.checkYourAnswers()
              )
          }

        }

      }

    }

    "handling requests to display the rebased acquisition price page" must {

      def performAction(): Future[Result] = controller.rebasedAcquisitionPrice()(FakeRequest())

      val acquisitionDate = AcquisitionDate(LocalDate.ofEpochDay(0L))

      behave like redirectToStartBehaviour(performAction)

      behave like missingAssetTypeAndResidentialStatusBehaviour(performAction)

      behave like missingAcquisitionDateBehaviour(performAction)

      "redirect to the acquisition price page" when {

        "that question hasn't been answered for eligible rebase from non uk resident" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate  = Some(AcquisitionDate(nonUkResidentsResidentialProperty.minusDays(2))),
                      acquisitionPrice = None
                    ),
                    AssetType.Residential,
                    false,
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
                    acquisitionDate   = Some(AcquisitionDate(RebasingCutoffDates.ukResidents.minusDays(2))),
                    acquisitionMethod = None
                  ),
                  sample[AssetType],
                  true,
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
                      acquisitionDate = AcquisitionDate(ukResidents.plusDays(1))
                    ),
                    AssetType.Residential,
                    true,
                    userType,
                    individualUserType
                  )._1
                )
              }
              checkIsRedirect(performAction(), routes.AcquisitionDetailsController.checkYourAnswers())
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
                      acquisitionDate =
                        Some(AcquisitionDate(RebasingCutoffDates.nonUkResidentsResidentialProperty.minusDays(1))),
                      acquisitionPrice = None
                    ),
                    AssetType.Residential,
                    false,
                    userType,
                    individualUserType
                  )._1
                )
              }
              checkIsRedirect(performAction(), routes.AcquisitionDetailsController.acquisitionPrice())
          }
        }

        def incompleteSectionUkResident(
          userType: UserType,
          individualUserType: IndividualUserType,
          userKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate   = Some(AcquisitionDate(RebasingCutoffDates.ukResidents.minusDays(2))),
                  acquisitionPrice  = Some(sample[AmountInPence]),
                  acquisitionMethod = Some(AcquisitionMethod.Bought)
                ),
                AssetType.Residential,
                true,
                userType,
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              TimeUtils.govDisplayFormat(ukResidents)
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionDate().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPriceSubmit()
                .url
              doc.select("#rebaseAcquisitionPrice-form-hint").text() shouldBe messageFromMessageKey(
                s"rebaseAcquisitionPrice$userKey.helpText"
              )
            }
          )
        }

        "the user has not yet completed the rebase acquisition details section for uk resident" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              incompleteSectionUkResident(userType, individualUserType, userKey)
          }
        }

        def incompleteSectionNonUKResident(
          userType: UserType,
          individualUserType: IndividualUserType,
          userKey: String
        ): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate  = Some(acquisitionDate),
                  acquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                false,
                userType,
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              TimeUtils.govDisplayFormat(nonUkResidentsResidentialProperty.minusDays(1))
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionPrice().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPriceSubmit()
                .url
              doc.select("#rebaseAcquisitionPrice-form-hint").text() shouldBe messageFromMessageKey(
                s"rebaseAcquisitionPrice$userKey.helpText"
              )
            }
          )
        }

        "the user has not yet completed the rebase acquisition details section for non-uk resident" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              incompleteSectionNonUKResident(userType, individualUserType, userKey)
          }
        }

        def sectionCompleted(userType: UserType, individualUserType: IndividualUserType, userKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(acquisitionDate = acquisitionDate),
                AssetType.Residential,
                true,
                userType,
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              TimeUtils.govDisplayFormat(ukResidents)
            ), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPriceSubmit()
                .url
            }
          )
        }

        "the user has already completed the acquisition details section" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              sectionCompleted(userType, individualUserType, userKey)
          }
        }

        def amountNonZero(userType: UserType, individualUserType: IndividualUserType, userKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = acquisitionDate,
                  rebasedAcquisitionPrice = Some(AmountInPence(1L))
                ),
                AssetType.Residential,
                true,
                userType,
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(
              "rebaseAcquisitionPrice.title",
              TimeUtils.govDisplayFormat(ukResidents)
            ),
            doc => doc.select("#rebaseAcquisitionPrice").attr("value") shouldBe "0.01"
          )
        }

        "the amount in the session is non-zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              amountNonZero(userType, individualUserType, userKey)
          }
        }

      }

    }

    "handling submitted rebased acquisition price answers" must {

      val key = "rebaseAcquisitionPrice"

      def performAction(data: (String, String)*): Future[Result] =
        controller.rebasedAcquisitionPriceSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val acquisitionDate = AcquisitionDate(ukResidents.minusDays(1))

      behave like redirectToStartBehaviour(() => performAction())

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
                      acquisitionDate =
                        Some(AcquisitionDate(RebasingCutoffDates.nonUkResidentsNonResidentialProperty.minusDays(1))),
                      acquisitionPrice = None
                    ),
                    AssetType.NonResidential,
                    false,
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

        def test(data: (String, String)*)(userType: UserType, individualUserType: IndividualUserType, userKey: String)(
          expectedErrorMessageKey: String
        ) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[CompleteAcquisitionDetailsAnswers].copy(acquisitionDate = acquisitionDate),
                AssetType.Residential,
                true,
                userType,
                individualUserType
              )._1
            )
          }

          val formattedRebaseDate = TimeUtils.govDisplayFormat(ukResidents)
          checkPageIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey(
              s"$key.title",
              formattedRebaseDate
            ),
            doc =>
              doc.select("#error-summary-display > ul > li > a").text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey,
                formattedRebaseDate
              ),
            BAD_REQUEST
          )
        }

        "the amount of money is zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test("rebaseAcquisitionPrice" -> "0")(userType, individualUserType, userKey)(
                s"$key.error.tooSmall"
              )
          }
        }

      }

      "show an error page" when {
        val price = 1.23d

        def getSessionDataJourneyAndDraftReturn(userType: UserType, individualUserType: IndividualUserType) = {
          val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
            acquisitionDate  = Some(AcquisitionDate(ukResidents.minusDays(2))),
            acquisitionPrice = Some(sample[AmountInPence])
          )
          val (session, journey, draftReturn) = sessionWithState(
            answers,
            AssetType.Residential,
            true,
            userType,
            individualUserType
          )
          val updatedDraftReturn = commonUpdateDraftReturn(
            draftReturn,
            answers.copy(
              rebasedAcquisitionPrice = Some(AmountInPence(123L)),
              acquisitionPrice        = Some(AmountInPence(123L)),
              shouldUseRebase         = Some(true)
            )
          )

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(userType, individualUserType)
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Left(Error("")))
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
                getSessionDataJourneyAndDraftReturn(userType, individualUserType)
              val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Right(()))
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
              scenarios.foreach {
                case (formData, expectedAmountInPence) =>
                  withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                    val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
                      acquisitionDate  = Some(acquisitionDate),
                      acquisitionPrice = Some(sample[AmountInPence])
                    )
                    val (session, journey, draftReturn) = sessionWithState(
                      answers,
                      AssetType.Residential,
                      true,
                      userType,
                      individualUserType
                    )
                    val updatedDraftReturn = commonUpdateDraftReturn(
                      draftReturn,
                      answers.copy(
                        rebasedAcquisitionPrice = Some(expectedAmountInPence),
                        acquisitionPrice        = Some(expectedAmountInPence),
                        shouldUseRebase         = Some(true)
                      )
                    )

                    val updatedSession =
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
                      performAction(formData: _*),
                      routes.AcquisitionDetailsController.checkYourAnswers()
                    )
                  }
              }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              scenarios.foreach {
                case (formData, expectedAmountInPence) =>
                  withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                    val answers = sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate         = acquisitionDate,
                      rebasedAcquisitionPrice = Some(AmountInPence(expectedAmountInPence.value + 1L))
                    )
                    val (session, journey, draftReturn) = sessionWithState(
                      answers,
                      AssetType.Residential,
                      true,
                      UserType.Individual,
                      individualUserType
                    )
                    val updatedDraftReturn = commonUpdateDraftReturn(
                      draftReturn,
                      answers.copy(
                        rebasedAcquisitionPrice = Some(expectedAmountInPence),
                        acquisitionPrice        = expectedAmountInPence,
                        shouldUseRebase         = true
                      )
                    )
                    val updatedSession = session.copy(
                      journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
                    )

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
                      performAction(formData: _*),
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

      behave like redirectToStartBehaviour(performAction)

      behave like missingAssetTypeAndResidentialStatusBehaviour(performAction)

      behave like missingAcquisitionDateBehaviour(performAction)

      "redirect to the acquisition price page" when {

        "the user does not meet the rebasing criteria and the user has not supplied an acquisition price yet" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate  = Some(AcquisitionDate(ukResidents)),
                      acquisitionPrice = None
                    ),
                    AssetType.Residential,
                    true,
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
                      acquisitionDate         = Some(AcquisitionDate(ukResidents.minusDays(1L))),
                      rebasedAcquisitionPrice = None
                    ),
                    AssetType.Residential,
                    true,
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

        def incompleteSection(userType: UserType, individualUserType: IndividualUserType, userKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithState(
                sample[IncompleteAcquisitionDetailsAnswers].copy(
                  acquisitionDate         = Some(AcquisitionDate(ukResidents.minusDays(1L))),
                  rebasedAcquisitionPrice = Some(sample[AmountInPence])
                ),
                AssetType.Residential,
                true,
                userType,
                individualUserType
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key$userKey.title"), { doc =>
              doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController
                .rebasedAcquisitionPrice()
                .url
              doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                .improvementCostsSubmit()
                .url
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
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate         = Some(AcquisitionDate(ukResidents)),
                      acquisitionPrice        = Some(sample[AmountInPence]),
                      rebasedAcquisitionPrice = Some(sample[AmountInPence])
                    ),
                    AssetType.Residential,
                    true,
                    userType,
                    individualUserType
                  )._1
                )
              }
              val userKey = userMessageKey(individualUserType, userType)
              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.acquisitionPrice().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .improvementCostsSubmit()
                    .url
                }
              )
          }
        }

        "the user meets the rebasing criteria and their acquisition details journey is complete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate         = AcquisitionDate(ukResidents.minusDays(1L)),
                      rebasedAcquisitionPrice = Some(sample[AmountInPence])
                    ),
                    AssetType.Residential,
                    true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .improvementCostsSubmit()
                    .url
                }
              )
          }
        }

        "the user does not meet the rebasing criteria and their acquisition details journey is complete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate         = AcquisitionDate(ukResidents),
                      rebasedAcquisitionPrice = Some(sample[AmountInPence])
                    ),
                    AssetType.Residential,
                    true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .improvementCostsSubmit()
                    .url
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
                      acquisitionDate         = AcquisitionDate(ukResidents),
                      rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                      improvementCosts        = AmountInPence.zero
                    ),
                    AssetType.Residential,
                    true,
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
                doc => doc.select("#improvementCosts-1").attr("checked") shouldBe "checked"
              )
          }
        }

        "the amount in the session is non-zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate         = AcquisitionDate(ukResidents),
                      rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                      improvementCosts        = AmountInPence(2L)
                    ),
                    AssetType.Residential,
                    true,
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
                ), { doc =>
                  doc.select("#improvementCosts-0").attr("checked")  shouldBe "checked"
                  doc.select("#improvementCostsValue").attr("value") shouldBe "0.02"
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
        controller.improvementCostsSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      val acquisitionDate = AcquisitionDate(LocalDate.ofEpochDay(0L))

      behave like redirectToStartBehaviour(() => performAction())

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
                      acquisitionDate  = Some(AcquisitionDate(ukResidents)),
                      acquisitionPrice = None
                    ),
                    AssetType.Residential,
                    true,
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
                      acquisitionDate         = Some(AcquisitionDate(ukResidents.minusDays(1L))),
                      rebasedAcquisitionPrice = None
                    ),
                    AssetType.Residential,
                    true,
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
        )(userType: UserType, individualUserType: IndividualUserType, userKey: String)(expectedErrorKey: String) =
          testFormError(data: _*)(userType, individualUserType)(expectedErrorKey)(s"$key$userKey.title")(performAction)

        "no option has been selected" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test()(userType, individualUserType, userKey)(s"$key.error.required")
          }
        }

        "the option selected is not valid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test(key -> "2")(userType, individualUserType, userKey)(s"$key.error.invalid")
          }
        }

        "the amount of money is invalid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              amountOfMoneyErrorScenarios(valueKey).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  val data = (key -> "0") :: scenario.formData
                  test(data: _*)(userType, individualUserType, userKey)(scenario.expectedErrorMessageKey)
                }
              }
          }
        }

        "the amount of money is zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test(key -> "0", valueKey -> "0")(userType, individualUserType, userKey)(
                s"$valueKey.error.tooSmall"
              )
          }
        }

      }

      "show an error page" when {
        val price = 1.23d

        def getSessionDataJourneyAndDraftReturn(userType: UserType, individualUserType: IndividualUserType) = {
          val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
            acquisitionDate         = Some(acquisitionDate),
            acquisitionPrice        = Some(sample[AmountInPence]),
            rebasedAcquisitionPrice = Some(sample[AmountInPence])
          )
          val (session, journey, draftReturn) = sessionWithState(
            answers,
            AssetType.Residential,
            true,
            userType,
            individualUserType
          )
          val updatedDraftReturn = commonUpdateDraftReturn(
            draftReturn,
            answers.copy(improvementCosts = Some(AmountInPence(123L)))
          )

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(userType, individualUserType)
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Left(Error("")))
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
                getSessionDataJourneyAndDraftReturn(userType, individualUserType)
              val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Right(()))
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
          List(key -> "1") -> AmountInPence.zero
        )

        "the price submitted is valid and the journey was incomplete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              scenarios.foreach {
                case (formData, expectedAmountInPence) =>
                  withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                    val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
                      acquisitionDate         = Some(acquisitionDate),
                      acquisitionPrice        = Some(sample[AmountInPence]),
                      rebasedAcquisitionPrice = Some(sample[AmountInPence])
                    )
                    val (session, journey, draftReturn) = sessionWithState(
                      answers,
                      AssetType.Residential,
                      true,
                      userType,
                      individualUserType
                    )
                    val updatedDraftReturn = commonUpdateDraftReturn(
                      draftReturn,
                      answers.copy(improvementCosts = Some(expectedAmountInPence))
                    )
                    val updatedSession = session.copy(
                      journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
                    )

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
                      performAction(formData: _*),
                      routes.AcquisitionDetailsController.checkYourAnswers()
                    )
                  }
              }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              scenarios.foreach {
                case (formData, expectedAmountInPence) =>
                  withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {

                    val answers = sample[CompleteAcquisitionDetailsAnswers]
                      .copy(
                        acquisitionDate         = acquisitionDate,
                        rebasedAcquisitionPrice = Some(sample[AmountInPence]),
                        improvementCosts        = AmountInPence(expectedAmountInPence.value + 1L)
                      )
                    val (session, journey, draftReturn) = sessionWithState(
                      answers,
                      AssetType.Residential,
                      true,
                      userType,
                      individualUserType
                    )
                    val updatedDraftReturn = commonUpdateDraftReturn(
                      draftReturn,
                      answers.copy(improvementCosts = expectedAmountInPence)
                    )
                    val updatedSession = session.copy(
                      journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
                    )

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
                      performAction(formData: _*),
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

      def performAction(): Future[Result] = controller.acquisitionFees()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "redirect to the improvement costs page" when {

        "that question hasn't been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      improvementCosts = None
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
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate  = Some(AcquisitionDate(nonUkResidentsNonResidentialProperty.plusDays(2L))),
                      shouldUseRebase  = Some(false),
                      improvementCosts = Some(sample[AmountInPence])
                    ),
                    AssetType.NonResidential,
                    false,
                    userType,
                    individualUserType
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.improvementCosts().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionFeesSubmit()
                    .url
                }
              )
          }
        }

        "the acquisition details section has not yet been completed with rebasing" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate  = Some(AcquisitionDate(ukResidents.minusDays(2L))),
                      shouldUseRebase  = Some(true),
                      improvementCosts = Some(sample[AmountInPence])
                    ),
                    AssetType.Residential,
                    true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.rebased.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.improvementCosts().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionFeesSubmit()
                    .url
                }
              )
          }
        }

        "the acquisition details section has been completed without rebasing" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(nonUkResidentsResidentialProperty.plusDays(1L)),
                      shouldUseRebase = false
                    ),
                    AssetType.Residential,
                    sample[Boolean],
                    userType,
                    individualUserType
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionFeesSubmit()
                    .url
                }
              )
          }
        }

        "the acquisition details section has been completed with rebasing" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(ukResidents.minusDays(1L)),
                      shouldUseRebase = true
                    ),
                    AssetType.Residential,
                    true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.rebased.title"), { doc =>
                  doc.select("#back").attr("href") shouldBe routes.AcquisitionDetailsController.checkYourAnswers().url
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
                    .acquisitionFeesSubmit()
                    .url
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
                      acquisitionDate = AcquisitionDate(nonUkResidentsResidentialProperty.plusDays(2L)),
                      acquisitionFees = AmountInPence.zero,
                      shouldUseRebase = false
                    ),
                    sample[AssetType],
                    sample[Boolean],
                    userType,
                    individualUserType
                  )._1
                )
              }

              val userKey = userMessageKey(individualUserType, userType)

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(s"$key$userKey.title"),
                doc => doc.select("#acquisitionFees-1").attr("checked") shouldBe "checked"
              )
          }
        }

        "the amount in the session is non-zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[CompleteAcquisitionDetailsAnswers].copy(
                      acquisitionDate = AcquisitionDate(nonUkResidentsResidentialProperty.plusDays(2L)),
                      acquisitionFees = AmountInPence(3L),
                      shouldUseRebase = false
                    ),
                    sample[AssetType],
                    sample[Boolean],
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
                ), { doc =>
                  doc.select("#acquisitionFees-0").attr("checked")  shouldBe "checked"
                  doc.select("#acquisitionFeesValue").attr("value") shouldBe "0.03"
                }
              )
          }
        }

      }

    }

    "handling submitted acquisition fees" must {

      val key      = "acquisitionFees"
      val valueKey = "acquisitionFeesValue"

      def performAction(data: (String, String)*): Future[Result] =
        controller.acquisitionFeesSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like redirectToStartBehaviour(() => performAction())

      "redirect to the improvement costs page" when {

        "the question has not been answered" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    sample[IncompleteAcquisitionDetailsAnswers].copy(
                      improvementCosts = None
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
        )(userType: UserType, individualUserType: IndividualUserType, userKey: String)(expectedErrorKey: String) =
          testFormError(data: _*)(userType, individualUserType)(expectedErrorKey)(s"$key$userKey.title")(performAction)

        "no option has been selected" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test()(userType, individualUserType, userKey)(s"$key.error.required")
          }
        }

        "the option selected is not valid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test(key -> "2")(userType, individualUserType, userKey)(s"$key.error.invalid")
          }
        }

        "the amount of money is invalid" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              amountOfMoneyErrorScenarios(valueKey).foreach { scenario =>
                withClue(s"For $scenario: ") {
                  val data = (key -> "0") :: scenario.formData
                  test(data: _*)(userType, individualUserType, userKey)(scenario.expectedErrorMessageKey)
                }
              }
          }
        }

        "the amount of money is zero" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val userKey = userMessageKey(individualUserType, userType)
              test(key -> "0", valueKey -> "0")(userType, individualUserType, userKey)(
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
          val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
            improvementCosts = Some(sample[AmountInPence])
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
            answers.copy(acquisitionFees = Some(AmountInPence(123L)))
          )

          (session, journey, updatedDraftReturn)
        }

        "there is an error updating the draft return" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              val (session, journey, updatedDraftReturn) =
                getSessionDataJourneyAndDraftReturn(userType, individualUserType)
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Left(Error("")))
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
                getSessionDataJourneyAndDraftReturn(userType, individualUserType)
              val updatedSession = session.copy(journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn)))

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockStoreDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )(Right(()))
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
          List(key -> "1") -> AmountInPence.zero
        )

        "the price submitted is valid and the journey was incomplete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              scenarios.foreach {
                case (formData, expectedAmountInPence) =>
                  withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                    val answers = IncompleteAcquisitionDetailsAnswers.empty.copy(
                      improvementCosts = Some(sample[AmountInPence])
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
                      answers.copy(acquisitionFees = Some(expectedAmountInPence))
                    )

                    val updatedSession = session.copy(
                      journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
                    )

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
                      performAction(formData: _*),
                      routes.AcquisitionDetailsController.checkYourAnswers()
                    )
                  }
              }
          }
        }

        "the price submitted is valid and the journey was complete" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              scenarios.foreach {
                case (formData, expectedAmountInPence) =>
                  withClue(s"For form data $formData and expected amount in pence $expectedAmountInPence: ") {
                    val answers =
                      sample[CompleteAcquisitionDetailsAnswers]
                        .copy(acquisitionFees = AmountInPence(expectedAmountInPence.value + 1L))
                    val (session, journey, draftReturn) = sessionWithState(
                      answers,
                      sample[AssetType],
                      sample[Boolean],
                      userType,
                      individualUserType
                    )
                    val updatedDraftReturn = commonUpdateDraftReturn(
                      draftReturn,
                      answers.copy(acquisitionFees = expectedAmountInPence)
                    )
                    val updatedSession = session.copy(
                      journeyStatus = Some(journey.copy(draftReturn = updatedDraftReturn))
                    )

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
                      performAction(formData: _*),
                      routes.AcquisitionDetailsController.checkYourAnswers()
                    )
                  }
              }
          }

        }

      }

    }

    "handling requests to display the should use rebase page" must {

      def performAction(): Future[Result] = controller.shouldUseRebase()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

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
                    false,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  "shouldUseRebase.title",
                  TimeUtils.govDisplayFormat(nonUkResidentsResidentialProperty.minusDays(1))
                )
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
                      improvementCosts = Some(sample[AmountInPence])
                    ),
                    AssetType.NonResidential,
                    false,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey(
                  "shouldUseRebase.title",
                  TimeUtils.govDisplayFormat(nonUkResidentsNonResidentialProperty.minusDays(1))
                )
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
                    true,
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
        controller.shouldUseRebaseSubmit()(FakeRequest().withFormUrlEncodedBody(data: _*))

      "show a form error for non residential non uk" when {

        val date: String = TimeUtils.govDisplayFormat(nonUkResidentsNonResidentialProperty.minusDays(1))

        def test(
          data: (String, String)*
        )(userType: UserType, individualUserType: IndividualUserType)(expectedErrorKey: String) =
          testFormError(data: _*)(userType, individualUserType)(expectedErrorKey)("shouldUseRebase.title", date)(
            performAction,
            sessionWithState(
              sample[CompleteAcquisitionDetailsAnswers]
                .copy(acquisitionDate = AcquisitionDate(nonUkResidentsNonResidentialProperty.minusDays(1))),
              AssetType.NonResidential,
              false,
              userType,
              individualUserType,
              disposalDate
            )._1
          )

        "no option has been selected" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              test()(userType, individualUserType)("shouldUseRebase.error.required")
          }
        }

      }

      "show a form error for residential non uk" when {
        val date: String = TimeUtils.govDisplayFormat(nonUkResidentsResidentialProperty.minusDays(1))

        def test(
          data: (String, String)*
        )(userType: UserType, individualUserType: IndividualUserType)(expectedErrorKey: String) =
          testFormError(data: _*)(userType, individualUserType)(expectedErrorKey)("shouldUseRebase.title", date)(
            performAction,
            sessionWithState(
              sample[CompleteAcquisitionDetailsAnswers]
                .copy(acquisitionDate = AcquisitionDate(nonUkResidentsResidentialProperty.minusDays(1))),
              AssetType.Residential,
              false,
              userType,
              individualUserType,
              disposalDate
            )._1
          )

        "no option has been selected" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              test()(userType, individualUserType)("shouldUseRebase.error.required")
          }
        }
      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

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

      behave like redirectToStartBehaviour(performAction)

      behave like missingAssetTypeAndResidentialStatusBehaviour(performAction)

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

      "redirect to the acquisition price page" when {

        "the question has not been answered, and eligible for acquisition price" in {
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              testRedirectOnMissingData(
                sessionWithState(
                  allQuestionsAnswered
                    .copy(acquisitionPrice = None, acquisitionDate = Some(AcquisitionDate(LocalDate.now()))),
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
                      acquisitionDate         = Some(AcquisitionDate(ukResidents.minusDays(1L))),
                      rebasedAcquisitionPrice = None
                    ),
                    AssetType.Residential,
                    true,
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
                      acquisitionDate =
                        Some(AcquisitionDate(RebasingCutoffDates.nonUkResidentsResidentialProperty.minusDays(1L))),
                      rebasedAcquisitionPrice = None
                    ),
                    AssetType.Residential,
                    false,
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
                      acquisitionDate =
                        Some(AcquisitionDate(RebasingCutoffDates.nonUkResidentsNonResidentialProperty.minusDays(1L))),
                      rebasedAcquisitionPrice = None
                    ),
                    AssetType.NonResidential,
                    false,
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
              val newDraftReturn = draftReturn.copy(acquisitionDetailsAnswers = Some(completeAnswers))
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
              val newDraftReturn = draftReturn.copy(acquisitionDetailsAnswers = Some(completeAnswers))
              val updatedJourney = journey.copy(draftReturn                   = newDraftReturn)
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
                mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Left(Error("")))
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
              val newDraftReturn = draftReturn.copy(
                acquisitionDetailsAnswers = Some(completeAnswers)
              )
              val updatedJourney = journey.copy(draftReturn = newDraftReturn)

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
                mockStoreSession(session.copy(journeyStatus = Some(updatedJourney)))(Right(()))
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionDetails.cya.title"),
                doc =>
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
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
                AcquisitionDate(nonUkResidentsNonResidentialProperty.minusDays(1)),
                sample[AmountInPence],
                Some(sample[AmountInPence]),
                sample[AmountInPence],
                sample[AmountInPence],
                false
              )
              val assetType = AssetType.NonResidential
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    nonUkRebasing,
                    assetType,
                    false,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionDetails.cya.title"), { doc =>
                  validateAcquisitionDetailsCheckYourAnswersPage(
                    nonUkRebasing,
                    doc,
                    false,
                    true
                  )
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
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
                AcquisitionDate(ukResidents.minusDays(2)),
                sample[AmountInPence],
                Some(sample[AmountInPence]),
                sample[AmountInPence],
                sample[AmountInPence],
                true
              )
              val assetType = AssetType.NonResidential
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    nonUkRebasing,
                    assetType,
                    true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionDetails.cya.title"), { doc =>
                  validateAcquisitionDetailsCheckYourAnswersPage(
                    nonUkRebasing,
                    doc,
                    true,
                    true
                  )
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
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
                AcquisitionDate(ukResidents.plusDays(1)),
                sample[AmountInPence],
                Some(sample[AmountInPence]),
                sample[AmountInPence],
                sample[AmountInPence],
                true
              )
              val assetType = AssetType.NonResidential
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    nonUkRebasing,
                    assetType,
                    true,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionDetails.cya.title"), { doc =>
                  validateAcquisitionDetailsCheckYourAnswersPage(
                    nonUkRebasing,
                    doc,
                    true,
                    false
                  )
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
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
                AcquisitionDate(nonUkResidentsNonResidentialProperty.plusDays(1)),
                sample[AmountInPence],
                Some(sample[AmountInPence]),
                sample[AmountInPence],
                sample[AmountInPence],
                false
              )
              val assetType = AssetType.NonResidential
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    nonUkRebasing,
                    assetType,
                    false,
                    userType,
                    individualUserType
                  )._1
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("acquisitionDetails.cya.title"), { doc =>
                  validateAcquisitionDetailsCheckYourAnswersPage(
                    nonUkRebasing,
                    doc,
                    false,
                    false
                  )
                  doc.select("#content > article > form").attr("action") shouldBe routes.AcquisitionDetailsController
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
                  (AcquisitionMethod.Bought, messages(s"acquisitionPriceBought$userKey.title")),
                  (
                    AcquisitionMethod.Inherited,
                    messages(s"acquisitionPriceNotBought$userKey.title", TimeUtils.govDisplayFormat(date))
                  ),
                  (
                    AcquisitionMethod.Gifted,
                    messages(s"acquisitionPriceNotBought$userKey.title", TimeUtils.govDisplayFormat(date))
                  ),
                  (
                    AcquisitionMethod.Other("test"),
                    messages(s"acquisitionPriceNotBought$userKey.title", TimeUtils.govDisplayFormat(date))
                  )
                ).foreach {
                  case (method, expectedTitle) =>
                    withClue(s"For $method and $expectedTitle") {
                      val nonUkRebasing = CompleteAcquisitionDetailsAnswers(
                        method,
                        AcquisitionDate(date),
                        sample[AmountInPence],
                        Some(sample[AmountInPence]),
                        sample[AmountInPence],
                        sample[AmountInPence],
                        false
                      )
                      val assetType = AssetType.NonResidential
                      inSequence {
                        mockAuthWithNoRetrievals()
                        mockGetSession(
                          sessionWithState(
                            nonUkRebasing,
                            assetType,
                            false,
                            userType,
                            individualUserType
                          )._1
                        )
                      }

                      checkPageIsDisplayed(
                        performAction(),
                        messageFromMessageKey("acquisitionDetails.cya.title"),
                        doc => doc.select("#acquisitionPrice-question").text() shouldBe expectedTitle
                      )
                    }
                }
            }
          }

        }

      }

    }

    "handling submits on the check your answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswersSubmit()(FakeRequest())

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

            checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
        }
      }
    }

    def missingAssetTypeAndResidentialStatusBehaviour(performAction: () => Future[Result]) =
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
                    Some(sample[DisposalDate])
                  )._1
                )
              }

              checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
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
                    Some(sample[DisposalDate])
                  )._1
                )
              }

              checkIsRedirect(performAction(), controllers.returns.routes.TaskListController.taskList())
          }
        }

      }

    def missingAcquisitionDateBehaviour(performAction: () => Future[Result]) =
      "redirect to the check you answers page" when {

        "there is no acquisition date" in {
          val completeAcquisitionDetailsAnswers = sample[CompleteAcquisitionDetailsAnswers]
          forAll(acceptedUserTypeGen, acceptedIndividualUserTypeGen) {
            (userType: UserType, individualUserType: IndividualUserType) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(
                  sessionWithState(
                    Some(
                      IncompleteAcquisitionDetailsAnswers(
                        Some(completeAcquisitionDetailsAnswers.acquisitionMethod),
                        None,
                        Some(completeAcquisitionDetailsAnswers.acquisitionPrice),
                        completeAcquisitionDetailsAnswers.rebasedAcquisitionPrice,
                        Some(completeAcquisitionDetailsAnswers.improvementCosts),
                        Some(completeAcquisitionDetailsAnswers.acquisitionFees),
                        Some(completeAcquisitionDetailsAnswers.shouldUseRebase)
                      )
                    ),
                    Some(sample[AssetType]),
                    Some(sample[Boolean]),
                    userType,
                    individualUserType,
                    Some(sample[DisposalDate])
                  )._1
                )
              }

              checkIsRedirect(performAction(), routes.AcquisitionDetailsController.checkYourAnswers())
          }
        }

      }

    def missingAcquisitionMethodBehaviour(performAction: () => Future[Result]) =
      "redirect to the check you answers page" when {

        "there is no acquisition method" in {
          val completeAcquisitionDetailsAnswers = sample[CompleteAcquisitionDetailsAnswers]
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
                        Some(completeAcquisitionDetailsAnswers.acquisitionPrice),
                        completeAcquisitionDetailsAnswers.rebasedAcquisitionPrice,
                        Some(completeAcquisitionDetailsAnswers.improvementCosts),
                        Some(completeAcquisitionDetailsAnswers.acquisitionFees),
                        Some(completeAcquisitionDetailsAnswers.shouldUseRebase)
                      )
                    ),
                    Some(sample[AssetType]),
                    Some(sample[Boolean]),
                    userType,
                    individualUserType,
                    Some(sample[DisposalDate])
                  )._1
                )
              }

              checkIsRedirect(performAction(), routes.AcquisitionDetailsController.checkYourAnswers())
          }
        }

      }

  }

  def testFormError(data: (String, String)*)(
    userType: UserType,
    individualUserType: IndividualUserType
  )(expectedErrorMessageKey: String, errorArgs: String*)(pageTitleKey: String, titleArgs: String*)(
    performAction: Seq[(String, String)] => Future[Result],
    currentSession: SessionData = sessionWithState(
      sample[CompleteAcquisitionDetailsAnswers].copy(
        rebasedAcquisitionPrice = Some(sample[AmountInPence]),
        shouldUseRebase         = false
      ),
      sample[AssetType],
      sample[Boolean],
      userType,
      individualUserType
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

}

object AcquisitionDetailsControllerSpec extends Matchers {

  def validateAcquisitionDetailsCheckYourAnswersPage(
    acquisitionDetailsAnswers: CompleteAcquisitionDetailsAnswers,
    doc: Document,
    isUk: Boolean,
    isRebasing: Boolean
  )(implicit messages: MessagesApi, lang: Lang): Unit = {
    val expectedAcquisitionMethodDisplayName = acquisitionDetailsAnswers.acquisitionMethod match {
      case AcquisitionMethod.Bought       => messages("returns.acquisitionMethod.Bought")
      case AcquisitionMethod.Inherited    => messages("returns.acquisitionMethod.Inherited")
      case AcquisitionMethod.Gifted       => messages("returns.acquisitionMethod.Gifted")
      case AcquisitionMethod.Other(value) => value
    }

    doc.select("#acquisitionMethod-answer").text() shouldBe expectedAcquisitionMethodDisplayName

    if (!isRebasing || !isUk) {
      doc.select("#acquisitionPrice-answer").text() shouldBe formatAmountOfMoneyWithPoundSign(
        acquisitionDetailsAnswers.acquisitionPrice.inPounds()
      )
    } else {
      doc.select("#acquisitionPrice-answer").text() shouldBe ""
    }

    if (acquisitionDetailsAnswers.improvementCosts === AmountInPence.zero) {
      doc.select("#improvementCosts-answer").text shouldBe "No"
    } else {
      doc.select("#improvementCosts-answer").text shouldBe "Yes"
      doc.select("#improvementCosts-value-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
        acquisitionDetailsAnswers.improvementCosts.inPounds()
      )
    }

    if (acquisitionDetailsAnswers.acquisitionFees === AmountInPence.zero) {
      doc.select("#acquisitionFees-answer").text shouldBe "No"
    } else {
      doc.select("#acquisitionFees-answer").text shouldBe "Yes"
      doc.select("#acquisitionFees-value-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
        acquisitionDetailsAnswers.acquisitionFees.inPounds()
      )
    }

    acquisitionDetailsAnswers.rebasedAcquisitionPrice.foreach { rebasedAcquisitionPrice =>
      if (!isUk && isRebasing) {
        if (acquisitionDetailsAnswers.shouldUseRebase) {
          doc.select("#shouldRebase-answer").text shouldBe "Yes"
          doc.select("#rebasedAcquisitionPrice-value-answer").text shouldBe formatAmountOfMoneyWithPoundSign(
            rebasedAcquisitionPrice.inPounds()
          )
        } else {
          doc.select("#shouldRebase-answer").text shouldBe "No"
        }
      }
    }
  }
}
