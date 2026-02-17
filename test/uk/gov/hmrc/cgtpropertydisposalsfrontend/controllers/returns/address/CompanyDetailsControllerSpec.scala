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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address

import cats.Eq
import org.jsoup.nodes.Document
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AmountOfMoneyErrorScenarios.amountOfMoneyErrorScenarios
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{ReturnsServiceSupport, StartingToAmendToFillingOutReturnSpecBehaviour}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.{Agent, Individual, Organisation}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExampleCompanyDetailsAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.MoneyGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, UUIDGenerator}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExampleCompanyDetailsAnswers.{CompleteExampleCompanyDetailsAnswers, IncompleteExampleCompanyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class CompanyDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with ReturnsServiceSupport
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with StartingToAmendToFillingOutReturnSpecBehaviour {

  val mockUUIDGenerator: UUIDGenerator = mock[UUIDGenerator]

  private def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
    redirectToStartWhenInvalidJourney(
      performAction,
      {
        case FillingOutReturn(_, _, _, _: DraftSingleIndirectDisposalReturn, _, _)    => true
        case FillingOutReturn(_, _, _, _: DraftMultipleIndirectDisposalsReturn, _, _) => true
        case _: StartingToAmendReturn                                                 => true
        case _                                                                        => false
      }
    )

  protected override val overrideBindings: List[GuiceableModule] = List[GuiceableModule](
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[ReturnsService].toInstance(mockReturnsService),
    bind[UUIDGenerator].toInstance(mockUUIDGenerator)
  )

  private lazy val controller = instanceOf[CompanyDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def sessionWithDraftSingleIndirectDisposal(
    name: Either[TrustName, IndividualName],
    userType: UserType,
    individualUserType: Option[IndividualUserType],
    companyAddress: Option[Address],
    amendReturnData: Option[AmendReturnData] = None,
    previousReturnData: Option[PreviousReturnData] = None
  ): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) = {
    val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
      individualUserType = individualUserType
    )

    val taxYearStartYear: String =
      triageAnswers
        .fold(
          _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
          c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
        )
        .map(_.toString)
        .getOrElse("2020")

    val updatedPreviousReturnData = previousReturnData match {
      case Some(PreviousReturnData(summaries, l, f, c)) =>
        val updatedSummaries = summaries.map(_.copy(taxYear = taxYearStartYear))
        Some(PreviousReturnData(updatedSummaries, l, f, c))
      case _                                            => previousReturnData
    }

    val draftReturn      = sample[DraftSingleIndirectDisposalReturn].copy(
      triageAnswers = triageAnswers,
      representeeAnswers = if (individualUserType.contains(Self)) None else Some(sample[CompleteRepresenteeAnswers]),
      companyAddress = companyAddress
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn,
      subscribedDetails = sample[SubscribedDetails].copy(name = name),
      agentReferenceNumber = if (Eq.eqv(userType, Agent)) Some(sample[AgentReferenceNumber]) else None,
      amendReturnData = amendReturnData,
      previousSentReturns = updatedPreviousReturnData
    )
    val sessionData      = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn),
      userType = Some(userType)
    )
    (sessionData, fillingOutReturn, draftReturn)
  }

  def individualState(
    companyAddress: Option[Address] = None,
    amendReturnData: Option[AmendReturnData] = None,
    previousReturnData: Option[PreviousReturnData] = None
  ): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) =
    sessionWithDraftSingleIndirectDisposal(
      Right(sample[IndividualName]),
      UserType.Individual,
      Some(Self),
      companyAddress,
      amendReturnData,
      previousReturnData
    )

  def sessionWithDraftMultipleIndirectDisposals(
    companyAddress: Address = sample[Address]
  ): (SessionData, FillingOutReturn, DraftMultipleIndirectDisposalsReturn) = {
    val country          = Country("HK")
    val draftReturn      = sample[DraftMultipleIndirectDisposalsReturn].copy(
      triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
        countryOfResidence = country,
        assetTypes = List(AssetType.IndirectDisposal)
      ),
      exampleCompanyDetailsAnswers = Some(
        sample[CompleteExampleCompanyDetailsAnswers].copy(
          address = companyAddress
        )
      )
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn
    )
    val sessionData      = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  def sessionWithDraftMultipleIndirectDisposals(
    individualUserType: IndividualUserType
  ): (SessionData, FillingOutReturn, DraftMultipleIndirectDisposalsReturn) = {
    val companyAddress   = sample[Address]
    val country          = Country("HK")
    val draftReturn      = sample[DraftMultipleIndirectDisposalsReturn].copy(
      triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
        countryOfResidence = country,
        assetTypes = List(AssetType.IndirectDisposal),
        individualUserType = Some(individualUserType)
      ),
      exampleCompanyDetailsAnswers = Some(
        sample[CompleteExampleCompanyDetailsAnswers].copy(
          address = companyAddress
        )
      )
    )
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = draftReturn
    )
    val sessionData      = SessionData.empty.copy(
      journeyStatus = Some(fillingOutReturn)
    )

    (sessionData, fillingOutReturn, draftReturn)
  }

  val allIndividualUserTypeGen: Gen[IndividualUserType] =
    Gen.oneOf(Self, Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin)

  def userMessageKey(individualUserType: IndividualUserType): String =
    individualUserType match {
      case PersonalRepresentativeInPeriodOfAdmin => ".personalRepInPeriodOfAdmin"
      case _                                     => ""
    }

  def capacitorState(): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) =
    sessionWithDraftSingleIndirectDisposal(
      Right(sample[IndividualName]),
      UserType.Individual,
      Some(Capacitor),
      None
    )

  def personalRepState(): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) =
    sessionWithDraftSingleIndirectDisposal(
      Right(sample[IndividualName]),
      UserType.Agent,
      Some(PersonalRepresentative),
      None
    )

  def agentState(): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) =
    sessionWithDraftSingleIndirectDisposal(
      Right(sample[IndividualName]),
      UserType.Agent,
      Some(Self),
      None
    )

  def trustState(): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) =
    sessionWithDraftSingleIndirectDisposal(
      Left(sample[TrustName]),
      UserType.Organisation,
      None,
      None
    )

  "CompanyDetailsController" when {

    "handling requests to display the enter postcode page" must {

      "redirect to the enter uk address page" in {
        checkIsRedirect(
          controller.enterPostcode()(FakeRequest()),
          routes.CompanyDetailsController.enterUkAddress()
        )
      }

    }

    "handling submits on the enter postcode page" must {

      "redirect to the enter uk address page" in {
        checkIsRedirect(
          controller.enterPostcodeSubmit()(FakeRequest()),
          routes.CompanyDetailsController.enterUkAddress()
        )
      }

    }

    "handling requests to display the select address page" must {

      "redirect to the enter uk address page" in {
        checkIsRedirect(
          controller.selectAddress()(FakeRequest()),
          routes.CompanyDetailsController.enterUkAddress()
        )
      }

    }

    "handling submits on the select address page" must {

      "redirect to the enter uk address page" in {
        checkIsRedirect(
          controller.selectAddressSubmit()(FakeRequest()),
          routes.CompanyDetailsController.enterUkAddress()
        )
      }

    }

    "handling requests to display the is uk page" must {

      def performAction(): Future[Result] = controller.isUk()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.isUk(),
        mockUUIDGenerator
      )

      "display the page" when {

        def test(result: Future[Result], expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            doc =>
              doc
                .select("#content > article > form, #main-content form")
                .attr("action") shouldBe routes.CompanyDetailsController
                .isUkSubmit()
                .url
          )

        "handling individuals" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test(performAction(), "companyDetails.isUk.title")
        }

        "handling capacitors" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(performAction(), "companyDetails.isUk.capacitor.title")
        }

        "handling personal representatives" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }

          test(performAction(), "companyDetails.isUk.personalRep.title")
        }

        "handling agents" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(agentState()._1)
          }

          test(performAction(), "companyDetails.isUk.agent.title")
        }

        "handling trusts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(trustState()._1)
          }

          test(performAction(), "companyDetails.isUk.trust.title")

        }

      }

      "display the page for multiple indirect disposals" when {

        def test(result: Future[Result], expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            doc =>
              doc
                .select("#content > article > form, #main-content form")
                .attr("action") shouldBe routes.CompanyDetailsController
                .isUkSubmit()
                .url
          )

        "handling all users" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(
                sessionWithDraftMultipleIndirectDisposals(
                  individualUserType
                )._1
              )
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(performAction(), s"companyDetails.isUk.multipleIndirect$userMsgKey.title")
          }
        }

      }

    }

    "handling submits on the is uk page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.isUkSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.isUkSubmit(),
        mockUUIDGenerator
      )

      "show a form error" when {

        def test(
          formData: (String, String)*
        )(expectedErrorMessageKey: String, expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            performAction(formData*),
            messageFromMessageKey(expectedTitleKey),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )

        "nothing is selected" when {

          "the user is an individual" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(individualState()._1)
            }

            test()(
              "isUk.companyDetails.error.required",
              "companyDetails.isUk.title"
            )
          }

          "the user is a personal representative" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(personalRepState()._1)
            }

            test()(
              "isUk.companyDetails.personalRep.error.required",
              "companyDetails.isUk.personalRep.title"
            )
          }

          "the user is a capacitor" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(capacitorState()._1)
            }

            test()(
              "isUk.companyDetails.capacitor.error.required",
              "companyDetails.isUk.capacitor.title"
            )
          }
        }

        "the user is an agent" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(agentState()._1)
          }

          test()(
            "isUk.companyDetails.agent.error.required",
            "companyDetails.isUk.agent.title"
          )
        }

        "the user is a trust" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(trustState()._1)
          }

          test()(
            "isUk.companyDetails.trust.error.required",
            "companyDetails.isUk.trust.title"
          )
        }

      }

      "show a form error for multiple indirect disposals" when {

        def test(
          formData: (String, String)*
        )(expectedErrorMessageKey: String, expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            performAction(formData*),
            messageFromMessageKey(expectedTitleKey),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )

        "nothing is selected" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userKey = userMessageKey(individualUserType)

            test()(
              "isUk.multipleIndirect.error.required",
              s"companyDetails.isUk.multipleIndirect$userKey.title"
            )
          }
        }

      }

      "redirect to the enter postcode endpoint" when {

        "the user selects yes" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          checkIsRedirect(
            performAction("isUk" -> "true"),
            routes.CompanyDetailsController.enterPostcode()
          )
        }
      }

      "redirect to the enter postcode endpoint for multiple indirect disposals" when {

        "the user selects yes" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithDraftMultipleIndirectDisposals()._1)
          }

          checkIsRedirect(
            performAction("isUk" -> "true"),
            routes.CompanyDetailsController.enterPostcode()
          )
        }
      }

      "redirect to the enter non uk address page" when {

        "the user selects no" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          checkIsRedirect(
            performAction("isUk" -> "false"),
            routes.CompanyDetailsController.enterNonUkAddress()
          )
        }

      }

      "redirect to the enter non uk address page for multiple indirect disposals" when {

        "the user selects no" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionWithDraftMultipleIndirectDisposals()._1)
          }

          checkIsRedirect(
            performAction("isUk" -> "false"),
            routes.CompanyDetailsController.enterNonUkAddress()
          )
        }

      }

    }

    "handling requests to display the enter uk company address page" must {

      def performAction(): Future[Result] =
        controller.enterUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterUkAddress(),
        mockUUIDGenerator
      )

      "display the page" when {

        def test(result: Future[Result], expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc
                .select(".govuk-caption-xl")
                .text() shouldBe messageFromMessageKey(
                "companyDetails.caption"
              )

              doc
                .select("label[for='address-line1']")
                .text() shouldBe messageFromMessageKey(
                "address.uk.companyDetails.line1.label"
              )

              doc
                .select("label[for='address-line2']")
                .text() shouldBe messageFromMessageKey(
                "address.uk.companyDetails.line2.label"
              )

              doc
                .select("#content form, #main-content form")
                .attr("action") shouldBe routes.CompanyDetailsController
                .enterUkAddressSubmit()
                .url
            }
          )

        "handling individuals" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test(performAction(), "address.uk.companyDetails.title")
        }

        "handling capacitors" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(performAction(), "address.uk.companyDetails.capacitor.title")
        }

        "handling personal representatives" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }

          test(performAction(), "address.uk.companyDetails.personalRep.title")
        }

        "handling agents" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(agentState()._1)
          }

          test(performAction(), "address.uk.companyDetails.agent.title")
        }

        "handling trusts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(trustState()._1)
          }

          test(performAction(), "address.uk.companyDetails.trust.title")

        }

      }

      "display the page for multiple indirect disposals" when {

        def test(result: Future[Result], expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc
                .select(".govuk-caption-xl")
                .text() shouldBe messageFromMessageKey(
                "returns.company-details.multipleIndirectDisposals.caption"
              )

              doc
                .select("label[for='address-line1']")
                .text() shouldBe messageFromMessageKey(
                "address.uk.companyDetails.line1.label"
              )

              doc
                .select("label[for='address-line2']")
                .text() shouldBe messageFromMessageKey(
                "address.uk.companyDetails.line2.label"
              )

              doc
                .select("#content form, #main-content form")
                .attr("action") shouldBe routes.CompanyDetailsController
                .enterUkAddressSubmit()
                .url
            }
          )

        "handling all users" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)
            test(
              performAction(),
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

      }

    }

    "handling submitted uk company address" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterUkAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterUkAddressSubmit(),
        mockUUIDGenerator
      )

      "return a form error" when {

        def test(
          formData: (String, String)*
        )(expectedErrorMessageKey: String, expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            performAction(formData*),
            messageFromMessageKey(expectedTitleKey),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
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
            "address-line1.companyDetails.error.required",
            "address.uk.companyDetails.title"
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
            "address-line1.companyDetails.error.tooLong",
            "address.uk.companyDetails.agent.title"
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
            "address-line1.companyDetails.error.pattern",
            "address.uk.companyDetails.trust.title"
          )
        }

        "address line 2 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }
          test(
            "address-line1" -> "Company name",
            "address-line2" -> ("a" * 100),
            "postcode"      -> "W1A2HV"
          )(
            "address-line2.companyDetails.error.tooLong",
            "address.uk.companyDetails.personalRep.title"
          )
        }

        "address line 2 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "address-line1" -> "Company name",
            "address-line2" -> "fsdhio*fde@df",
            "postcode"      -> "W1A2HV"
          )(
            "address-line2.companyDetails.error.pattern",
            "address.uk.companyDetails.capacitor.title"
          )
        }

        "address line 3 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }
          test(
            "address-line1" -> "Company name",
            "address-town"  -> ("a" * 100),
            "postcode"      -> "W1A2HV"
          )(
            "address-town.companyDetails.error.tooLong",
            "address.uk.companyDetails.personalRep.title"
          )
        }

        "address line 3 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "address-line1" -> "Company name",
            "address-town"  -> "fsdhio*fde@df",
            "postcode"      -> "W1A2HV"
          )(
            "address-town.companyDetails.error.pattern",
            "address.uk.companyDetails.capacitor.title"
          )
        }

        "address line 4 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }
          test(
            "address-line1"  -> "Company name",
            "address-county" -> ("a" * 100),
            "postcode"       -> "W1A2HV"
          )(
            "address-county.companyDetails.error.tooLong",
            "address.uk.companyDetails.personalRep.title"
          )
        }

        "address line 4 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "address-line1"  -> "Company name",
            "address-county" -> "fsdhio*fde@df",
            "postcode"       -> "W1A2HV"
          )(
            "address-county.companyDetails.error.pattern",
            "address.uk.companyDetails.capacitor.title"
          )
        }

        "address postcode is empty" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test("address-line1" -> "1 the Street")(
            "postcode.companyDetails.error.required",
            "address.uk.companyDetails.title"
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
            "postcode.companyDetails.error.pattern",
            "address.uk.companyDetails.title"
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
            "postcode.companyDetails.error.pattern",
            "address.uk.companyDetails.title"
          )
        }

      }

      "return a form error for multiple indirect disposals" when {

        def test(
          formData: (String, String)*
        )(expectedErrorMessageKey: String, expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            performAction(formData*),
            messageFromMessageKey(expectedTitleKey),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )

        "address line 1 is empty" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test("postcode" -> "W1A2HV")(
              "address-line1.companyDetails.error.required",
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 1 is too long" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "address-line1" -> ("a" * 100),
              "postcode"      -> "W1A2HV"
            )(
              "address-line1.companyDetails.error.tooLong",
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 1 is invalid" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "address-line1" -> "ab%csd",
              "postcode"      -> "W1A2HV"
            )(
              "address-line1.companyDetails.error.pattern",
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 2 is too long" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "address-line1" -> "Company name",
              "address-line2" -> ("a" * 100),
              "postcode"      -> "W1A2HV"
            )(
              "address-line2.companyDetails.error.tooLong",
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 2 is invalid" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "address-line1" -> "Company name",
              "address-line2" -> "fsdhio*fde@df",
              "postcode"      -> "W1A2HV"
            )(
              "address-line2.companyDetails.error.pattern",
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 3 is too long" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "address-line1" -> "Company name",
              "address-town"  -> ("a" * 100),
              "postcode"      -> "W1A2HV"
            )(
              "address-town.companyDetails.error.tooLong",
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 3 is invalid" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "address-line1" -> "Company name",
              "address-town"  -> "fsdhio*fde@df",
              "postcode"      -> "W1A2HV"
            )(
              "address-town.companyDetails.error.pattern",
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 4 is too long" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "address-line1"  -> "Company name",
              "address-county" -> ("a" * 100),
              "postcode"       -> "W1A2HV"
            )(
              "address-county.companyDetails.error.tooLong",
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 4 is invalid" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "address-line1"  -> "Company name",
              "address-county" -> "fsdhio*fde@df",
              "postcode"       -> "W1A2HV"
            )(
              "address-county.companyDetails.error.pattern",
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address postcode is empty" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test("address-line1" -> "1 the Street")(
              "postcode.companyDetails.error.required",
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "the address postcode contains invalid characters" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "address-line1" -> "1 the Street",
              "postcode"      -> "W1A,2HV"
            )(
              "postcode.companyDetails.error.pattern",
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "the address postcode does not have a valid format" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "address-line1" -> "1 the Street",
              "postcode"      -> "ABC123"
            )(
              "postcode.companyDetails.error.pattern",
              s"address.uk.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

      }

      "show an error page" when {

        val (session, journey, draftReturn) = individualState()
        val address                         =
          UkAddress("The Company", None, None, None, Postcode("ZZ10ZZ"))

        val newDraftReturn = draftReturn.copy(companyAddress = Some(address))
        val newJourney     = journey.copy(draftReturn = newDraftReturn)
        val newSession     = session.copy(journeyStatus = Some(newJourney))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(
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
            mockStoreDraftReturn(newJourney)(
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

        "all updates are successful" when {

          "the user is not an an amend or further return journey" in {
            val (session, journey, draftReturn) = agentState()
            val address                         =
              UkAddress("The Company", None, None, None, Postcode("ZZ10ZZ"))

            val newDraftReturn = draftReturn.copy(companyAddress = Some(address))
            val newJourney     = journey.copy(draftReturn = newDraftReturn)
            val newSession     = session.copy(journeyStatus = Some(newJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(
                Right(())
              )
              mockStoreSession(newSession)(Right(()))
            }

            checkIsRedirect(
              performAction(
                "address-line1" -> address.line1,
                "postcode"      -> address.postcode.value
              ),
              routes.CompanyDetailsController.checkYourAnswers()
            )
          }

          "the user is on an amend journey" in {
            val (session, journey, draftReturn) = individualState(amendReturnData = Some(sample[AmendReturnData]))
            val address                         =
              UkAddress("The Company", None, None, None, Postcode("ZZ10ZZ"))

            val newDraftReturn = draftReturn.copy(
              companyAddress = Some(address),
              exemptionAndLossesAnswers = None,
              yearToDateLiabilityAnswers = None
            )
            val newJourney     = journey.copy(draftReturn = newDraftReturn)
            val newSession     = session.copy(journeyStatus = Some(newJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(
                Right(())
              )
              mockStoreSession(newSession)(Right(()))
            }

            checkIsRedirect(
              performAction(
                "address-line1" -> address.line1,
                "postcode"      -> address.postcode.value
              ),
              routes.CompanyDetailsController.checkYourAnswers()
            )
          }

          "the user is on a further return journey" in {
            val (session, journey, draftReturn) = individualState(
              previousReturnData = Some(
                sample[PreviousReturnData].copy(
                  summaries = List(sample[ReturnSummary])
                )
              )
            )
            val address                         =
              UkAddress("The Company", None, None, None, Postcode("ZZ10ZZ"))

            val newDraftReturn = draftReturn.copy(
              companyAddress = Some(address),
              exemptionAndLossesAnswers = None,
              yearToDateLiabilityAnswers = None
            )
            val newJourney     = journey.copy(draftReturn = newDraftReturn)
            val newSession     = session.copy(journeyStatus = Some(newJourney))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreDraftReturn(newJourney)(
                Right(())
              )
              mockStoreSession(newSession)(Right(()))
            }

            checkIsRedirect(
              performAction(
                "address-line1" -> address.line1,
                "postcode"      -> address.postcode.value
              ),
              routes.CompanyDetailsController.checkYourAnswers()
            )
          }
        }

      }

    }

    "handling requests to display the enter non-uk company address page" must {

      def performAction(): Future[Result] =
        controller.enterNonUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterNonUkAddress(),
        mockUUIDGenerator
      )

      "display the page" when {

        def test(result: Future[Result], expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc
                .select(".govuk-caption-xl")
                .text() shouldBe messageFromMessageKey(
                "companyDetails.caption"
              )

              doc
                .select("label[for='nonUkAddress-line1']")
                .text() shouldBe messageFromMessageKey(
                "nonUkAddress.companyDetails.line1.label"
              )

              doc
                .select("label[for='nonUkAddress-line2']")
                .text() shouldBe messageFromMessageKey(
                "nonUkAddress.companyDetails.line2.label"
              )

              doc
                .select("#content form, #main-content form")
                .attr("action") shouldBe routes.CompanyDetailsController
                .enterNonUkAddressSubmit()
                .url
            }
          )

        "handling individuals" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState()._1)
          }

          test(performAction(), "nonUkAddress.companyDetails.title")
        }

        "handling capacitors" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(performAction(), "nonUkAddress.companyDetails.capacitor.title")
        }

        "handling personal representatives" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }

          test(performAction(), "nonUkAddress.companyDetails.personalRep.title")
        }

        "handling agents" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(agentState()._1)
          }

          test(performAction(), "nonUkAddress.companyDetails.agent.title")
        }

        "handling trusts" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(trustState()._1)
          }

          test(performAction(), "nonUkAddress.companyDetails.trust.title")

        }

      }

      "display the page for multiple indirect disposals" when {

        def test(result: Future[Result], expectedTitleKey: String)(isPeriodOfAdmin: Boolean): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              if (isPeriodOfAdmin) {
                doc
                  .select(".govuk-caption-xl")
                  .text() shouldBe messageFromMessageKey(
                  "companyDetails.caption"
                )
              } else {
                doc
                  .select(".govuk-caption-xl")
                  .text() shouldBe messageFromMessageKey(
                  "returns.company-details.multipleIndirectDisposals.caption"
                )
              }

              doc
                .select("label[for='nonUkAddress-line1']")
                .text() shouldBe messageFromMessageKey(
                "nonUkAddress.companyDetails.line1.label"
              )

              doc
                .select("label[for='nonUkAddress-line2']")
                .text() shouldBe messageFromMessageKey(
                "nonUkAddress.companyDetails.line2.label"
              )

              doc
                .select("#content > article > form, #main-content form")
                .attr("action") shouldBe routes.CompanyDetailsController
                .enterNonUkAddressSubmit()
                .url
            }
          )

        "handling all users" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(performAction(), s"nonUkAddress.companyDetails.multipleIndirect$userMsgKey.title")(isPeriodOfAdmin =
              false
            )
          }
        }
      }
    }

    "handling submitted non-uk company address" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterNonUkAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.enterNonUkAddressSubmit(),
        mockUUIDGenerator
      )

      "return a form error" when {

        def test(
          formData: (String, String)*
        )(expectedErrorMessageKey: String, expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            performAction(formData*),
            messageFromMessageKey(expectedTitleKey),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
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

          test("countryCode" -> "HK")(
            "nonUkAddress-line1.companyDetails.error.required",
            "nonUkAddress.companyDetails.title"
          )
        }

        "address line 1 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(agentState()._1)
          }

          test(
            "nonUkAddress-line1" -> ("a" * 100),
            "countryCode"        -> "HK"
          )(
            "nonUkAddress-line1.companyDetails.error.tooLong",
            "nonUkAddress.companyDetails.agent.title"
          )
        }

        "address line 1 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(trustState()._1)
          }

          test(
            "nonUkAddress-line1" -> "ab%csd",
            "countryCode"        -> "HK"
          )(
            "nonUkAddress-line1.companyDetails.error.pattern",
            "nonUkAddress.companyDetails.trust.title"
          )
        }

        "address line 2 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }
          test(
            "nonUkAddress-line1" -> "Company name",
            "nonUkAddress-line2" -> ("a" * 100),
            "countryCode"        -> "HK"
          )(
            "nonUkAddress-line2.companyDetails.error.tooLong",
            "nonUkAddress.companyDetails.personalRep.title"
          )
        }

        "address line 2 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "nonUkAddress-line1" -> "Company name",
            "nonUkAddress-line2" -> "fsdhio*fde@df",
            "countryCode"        -> "HK"
          )(
            "nonUkAddress-line2.companyDetails.error.pattern",
            "nonUkAddress.companyDetails.capacitor.title"
          )
        }

        "address line 3 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }
          test(
            "nonUkAddress-line1" -> "Company name",
            "nonUkAddress-line3" -> ("a" * 100),
            "countryCode"        -> "HK"
          )(
            "nonUkAddress-line3.companyDetails.error.tooLong",
            "nonUkAddress.companyDetails.personalRep.title"
          )
        }

        "address line 3 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "nonUkAddress-line1" -> "Company name",
            "nonUkAddress-line3" -> "fsdhio*fde@df",
            "countryCode"        -> "HK"
          )(
            "nonUkAddress-line3.companyDetails.error.pattern",
            "nonUkAddress.companyDetails.capacitor.title"
          )
        }

        "address line 4 is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(personalRepState()._1)
          }
          test(
            "nonUkAddress-line1" -> "Company name",
            "nonUkAddress-line4" -> ("a" * 100),
            "countryCode"        -> "HK"
          )(
            "nonUkAddress-line4.companyDetails.error.tooLong",
            "nonUkAddress.companyDetails.personalRep.title"
          )
        }

        "address line 4 is invalid" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "nonUkAddress-line1" -> "Company name",
            "nonUkAddress-line4" -> "fsdhio*fde@df",
            "countryCode"        -> "HK"
          )(
            "nonUkAddress-line4.companyDetails.error.pattern",
            "nonUkAddress.companyDetails.capacitor.title"
          )
        }

        "a country is not selected" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "nonUkAddress-line1" -> "Company name"
          )(
            "countryCode.companyDetails.error.required",
            "nonUkAddress.companyDetails.capacitor.title"
          )
        }

        "the country cannot be found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(capacitorState()._1)
          }

          test(
            "nonUkAddress-line1" -> "Company name",
            "countryCode"        -> "ZZ"
          )(
            "countryCode.companyDetails.error.notFound",
            "nonUkAddress.companyDetails.capacitor.title"
          )
        }

      }

      "return a form error for multiple indirect disposal" when {

        def test(
          formData: (String, String)*
        )(expectedErrorMessageKey: String, expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            performAction(formData*),
            messageFromMessageKey(expectedTitleKey),
            doc =>
              doc
                .select("[data-spec='errorSummaryDisplay'] a")
                .text() shouldBe messageFromMessageKey(
                expectedErrorMessageKey
              ),
            BAD_REQUEST
          )

        "address line 1 is empty" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test("countryCode" -> "HK")(
              "nonUkAddress-line1.companyDetails.error.required",
              s"nonUkAddress.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 1 is too long" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "nonUkAddress-line1" -> ("a" * 100),
              "countryCode"        -> "HK"
            )(
              "nonUkAddress-line1.companyDetails.error.tooLong",
              s"nonUkAddress.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 1 is invalid" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "nonUkAddress-line1" -> "ab%csd",
              "countryCode"        -> "HK"
            )(
              "nonUkAddress-line1.companyDetails.error.pattern",
              s"nonUkAddress.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 2 is too long" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "nonUkAddress-line1" -> "Company name",
              "nonUkAddress-line2" -> ("a" * 100),
              "countryCode"        -> "HK"
            )(
              "nonUkAddress-line2.companyDetails.error.tooLong",
              s"nonUkAddress.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 2 is invalid" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "nonUkAddress-line1" -> "Company name",
              "nonUkAddress-line2" -> "fsdhio*fde@df",
              "countryCode"        -> "HK"
            )(
              "nonUkAddress-line2.companyDetails.error.pattern",
              s"nonUkAddress.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 3 is too long" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "nonUkAddress-line1" -> "Company name",
              "nonUkAddress-line3" -> ("a" * 100),
              "countryCode"        -> "HK"
            )(
              "nonUkAddress-line3.companyDetails.error.tooLong",
              s"nonUkAddress.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 3 is invalid" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "nonUkAddress-line1" -> "Company name",
              "nonUkAddress-line3" -> "fsdhio*fde@df",
              "countryCode"        -> "HK"
            )(
              "nonUkAddress-line3.companyDetails.error.pattern",
              s"nonUkAddress.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 4 is too long" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "nonUkAddress-line1" -> "Company name",
              "nonUkAddress-line4" -> ("a" * 100),
              "countryCode"        -> "HK"
            )(
              "nonUkAddress-line4.companyDetails.error.tooLong",
              s"nonUkAddress.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "address line 4 is invalid" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "nonUkAddress-line1" -> "Company name",
              "nonUkAddress-line4" -> "fsdhio*fde@df",
              "countryCode"        -> "HK"
            )(
              "nonUkAddress-line4.companyDetails.error.pattern",
              s"nonUkAddress.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "a country is not selected" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "nonUkAddress-line1" -> "Company name"
            )(
              "countryCode.companyDetails.error.required",
              s"nonUkAddress.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

        "the country cannot be found" in {
          forAll(allIndividualUserTypeGen) { (individualUserType: IndividualUserType) =>
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionWithDraftMultipleIndirectDisposals(individualUserType)._1)
            }

            val userMsgKey = userMessageKey(individualUserType)

            test(
              "nonUkAddress-line1" -> "Company name",
              "countryCode"        -> "ZZ"
            )(
              "countryCode.companyDetails.error.notFound",
              s"nonUkAddress.companyDetails.multipleIndirect$userMsgKey.title"
            )
          }
        }

      }

      "show an error page" when {

        val (session, journey, draftReturn) = individualState()
        val address                         = NonUkAddress(
          "The Company",
          None,
          None,
          None,
          None,
          Country("HK")
        )

        val newDraftReturn = draftReturn.copy(companyAddress = Some(address))
        val newJourney     = journey.copy(draftReturn = newDraftReturn)
        val newSession     = session.copy(journeyStatus = Some(newJourney))

        "there is an error updating the draft return" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(
            performAction(
              "nonUkAddress-line1" -> address.line1,
              "countryCode"        -> address.country.code
            )
          )
        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(
              Right(())
            )
            mockStoreSession(newSession)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(
            performAction(
              "nonUkAddress-line1" -> address.line1,
              "countryCode"        -> address.country.code
            )
          )
        }

      }

      "redirect to the cya page" when {

        "all updates are successful" in {
          val (session, journey, draftReturn) = trustState()
          val address                         = NonUkAddress(
            "The Company",
            None,
            None,
            None,
            None,
            Country("HK")
          )

          val newDraftReturn = draftReturn.copy(companyAddress = Some(address))
          val newJourney     = journey.copy(draftReturn = newDraftReturn)
          val newSession     = session.copy(journeyStatus = Some(newJourney))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(newJourney)(
              Right(())
            )
            mockStoreSession(newSession)(Right(()))
          }

          checkIsRedirect(
            performAction(
              "nonUkAddress-line1" -> address.line1,
              "countryCode"        -> address.country.code
            ),
            routes.CompanyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the multiple indirect disposal price page" must {

      val key = "multipleIndirectDisposalsDisposalPrice"

      def performAction(): Future[Result] =
        controller.multipleIndirectDisposalPrice()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.multipleIndirectDisposalPrice(),
        mockUUIDGenerator
      )

      "display the page" when {

        def test(
          draftReturn: DraftMultipleIndirectDisposalsReturn,
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
                      name = if (userType === Organisation) Left(sample[TrustName]) else Right(sample[IndividualName])
                    ),
                    agentReferenceNumber = if (userType === Agent) Some(sample[AgentReferenceNumber]) else None
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.title"),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                                  shouldBe routes.CompanyDetailsController
                .multipleIndirectDisposalPriceSubmit()
                .url
              doc
                .select(s"#$key-hint")
                .text()                                          shouldBe messageFromMessageKey(s"$key.helpText")
            }
          )
        }

        "individual user has not started this section before" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = None,
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.isUk(),
            Individual
          )
        }

        "trust user has not started this section before" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = None,
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.isUk(),
            Organisation
          )
        }

        "agent user has not started this section before" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = None,
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.isUk(),
            Agent
          )
        }

        "individual user has started but not completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[IncompleteExampleCompanyDetailsAnswers].copy(
                  address = Some(sample[UkAddress]),
                  disposalPrice = None
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.enterUkAddress(),
            Individual
          )
        }

        "trust user has started but not completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[IncompleteExampleCompanyDetailsAnswers].copy(
                  address = Some(sample[NonUkAddress]),
                  disposalPrice = None
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.enterNonUkAddress(),
            Organisation
          )
        }

        "agent user has started but not completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[IncompleteExampleCompanyDetailsAnswers].copy(
                  address = Some(sample[UkAddress]),
                  disposalPrice = None
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.enterUkAddress(),
            Agent
          )
        }

        "individual user has completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[CompleteExampleCompanyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.checkYourAnswers(),
            Individual
          )
        }

        "trust user has completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[CompleteExampleCompanyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.checkYourAnswers(),
            Organisation
          )
        }

        "agent user has completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[CompleteExampleCompanyDetailsAnswers].copy(
                  disposalPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.checkYourAnswers(),
            Agent
          )
        }

      }

    }

    "handling submitted answers to the multiple Indirect Disposals Guidance" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.multipleIndirectDisposalsGuidance()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.multipleIndirectDisposalsGuidance(),
        mockUUIDGenerator
      )

      "not update the session" when {

        "the data submitted is the same as one that already exists in session" in {

          val disposalPrice = AmountInPence.fromPounds(1000)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleIndirectDisposalsReturn].copy(
                      exampleCompanyDetailsAnswers = Some(
                        sample[IncompleteExampleCompanyDetailsAnswers].copy(
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

          checkPageIsDisplayed(
            controller.multipleIndirectDisposalsGuidance()(FakeRequest()),
            messageFromMessageKey("company-details.multiple-indirect-disposals.guidance.title")
          )
        }
      }

    }

    "handling submitted answers to the multiple indirect disposal guidance page" must {
      val key = "multipleIndirectDisposalsDisposalPrice"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.multipleIndirectDisposalsGuidanceSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.multipleIndirectDisposalsGuidanceSubmit(),
        mockUUIDGenerator
      )

      "not update the session" when {

        "the data submitted is the same as one that already exists in session" in {

          val disposalPrice = AmountInPence.fromPounds(1000)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleIndirectDisposalsReturn].copy(
                      exampleCompanyDetailsAnswers = Some(
                        sample[IncompleteExampleCompanyDetailsAnswers].copy(
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
            routes.CompanyDetailsController.isUk()
          )
        }

      }

    }

    "handling submitted answers to the multiple indirect disposal price page" must {

      val key = "multipleIndirectDisposalsDisposalPrice"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.multipleIndirectDisposalPriceSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.multipleIndirectDisposalPriceSubmit(),
        mockUUIDGenerator
      )

      "not update the session" when {

        "the data submitted is the same as one that already exists in session" in {

          val disposalPrice = AmountInPence.fromPounds(1000)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleIndirectDisposalsReturn].copy(
                      exampleCompanyDetailsAnswers = Some(
                        sample[IncompleteExampleCompanyDetailsAnswers].copy(
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
            routes.CompanyDetailsController.checkYourAnswers()
          )
        }

      }

      "show a form error for amount" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleIndirectDisposalsReturn].copy(
                      exampleCompanyDetailsAnswers = Some(
                        sample[CompleteExampleCompanyDetailsAnswers]
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
            performAction(data*),
            messageFromMessageKey(s"$key.title"),
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
          amountOfMoneyErrorScenarios(key).foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = scenario.formData
              test(data*)(scenario.expectedErrorMessageKey)
            }
          }
        }

      }

      "redirect to the cya page" when {

        def test(
          result: => Future[Result],
          oldDraftReturn: DraftReturn,
          updatedDraftReturn: DraftReturn,
          isAmend: Boolean
        ): Unit = {
          val amendReturnData = sample[AmendReturnData]

          val journey        = sample[FillingOutReturn].copy(
            draftReturn = oldDraftReturn,
            amendReturnData =
              if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = false)) else None
          )
          val session        = SessionData.empty.copy(
            journeyStatus = Some(journey)
          )
          val updatedJourney = journey.copy(
            draftReturn = updatedDraftReturn,
            amendReturnData =
              if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = true)) else None
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

          checkIsRedirect(
            result,
            routes.CompanyDetailsController.checkYourAnswers()
          )
        }

        "the user is on a multiple disposals journey and has completed this section" in {

          val answers = sample[CompleteExampleCompanyDetailsAnswers].copy(
            disposalPrice = AmountInPence.fromPounds(1)
          )

          val oldDraftReturn = sample[DraftMultipleIndirectDisposalsReturn].copy(
            exampleCompanyDetailsAnswers = Some(answers)
          )

          val updatedDraftReturn = oldDraftReturn.copy(
            exampleCompanyDetailsAnswers = Some(
              answers.copy(
                disposalPrice = AmountInPence.fromPounds(10)
              )
            ),
            yearToDateLiabilityAnswers = None,
            gainOrLossAfterReliefs = None
          )

          test(
            performAction(key -> "10"),
            oldDraftReturn,
            updatedDraftReturn,
            isAmend = true
          )
        }

        "the user hasn't ever answered the disposal price question " +
          "and the draft return and session data has been successfully updated" in {

            val answers = sample[IncompleteExampleCompanyDetailsAnswers].copy(
              address = Some(sample[UkAddress]),
              disposalPrice = Some(AmountInPence.fromPounds(1))
            )

            val oldDraftReturn = sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(answers)
            )

            val updatedDraftReturn = oldDraftReturn.copy(
              exampleCompanyDetailsAnswers = Some(
                answers.copy(
                  disposalPrice = Some(AmountInPence.fromPounds(10))
                )
              ),
              yearToDateLiabilityAnswers = None,
              gainOrLossAfterReliefs = None
            )

            test(
              performAction(key -> "10"),
              oldDraftReturn,
              updatedDraftReturn,
              isAmend = false
            )
          }

      }

    }

    "handling requests to display the multiple indirect acquisition price page" must {

      val key = "multipleIndirectDisposalsAcquisitionPrice"

      def performAction(): Future[Result] =
        controller.multipleIndirectAcquisitionPrice()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.multipleIndirectAcquisitionPrice(),
        mockUUIDGenerator
      )

      "display the page" when {

        def test(
          draftReturn: DraftMultipleIndirectDisposalsReturn,
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
                      name = if (userType === Organisation) Left(sample[TrustName]) else Right(sample[IndividualName])
                    ),
                    agentReferenceNumber = if (userType === Agent) Some(sample[AgentReferenceNumber]) else None
                  )
                )
              )
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey(s"$key.title"),
            { doc =>
              doc.select("#back, .govuk-back-link").attr("href") shouldBe expectedBackLink.url
              doc
                .select("#content > article > form, #main-content form")
                .attr("action")                                  shouldBe routes.CompanyDetailsController
                .multipleIndirectAcquisitionPriceSubmit()
                .url
              doc
                .select(s"#$key-hint")
                .text()                                          shouldBe messageFromMessageKey(s"$key.helpText")
            }
          )
        }

        "individual user has not started this section before" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = None,
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.multipleIndirectDisposalPrice(),
            Individual
          )
        }

        "trust user has not started this section before" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = None,
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.multipleIndirectDisposalPrice(),
            Organisation
          )
        }

        "agent user has not started this section before" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = None,
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.multipleIndirectDisposalPrice(),
            Agent
          )
        }

        "capacitor user has not started this section before" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = None,
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(Capacitor))
            ),
            routes.CompanyDetailsController.multipleIndirectDisposalPrice(),
            Individual
          )
        }

        "personal representative user has not started this section before" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = None,
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentative))
            ),
            routes.CompanyDetailsController.multipleIndirectDisposalPrice(),
            Individual
          )
        }

        "individual user has started but not completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[IncompleteExampleCompanyDetailsAnswers].copy(
                  acquisitionPrice = None
                )
              )
            ),
            routes.CompanyDetailsController.multipleIndirectDisposalPrice(),
            Individual
          )
        }

        "trust user has started but not completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[IncompleteExampleCompanyDetailsAnswers].copy(
                  acquisitionPrice = None
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.multipleIndirectDisposalPrice(),
            Organisation
          )
        }

        "agent user has started but not completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[IncompleteExampleCompanyDetailsAnswers].copy(
                  acquisitionPrice = None
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.multipleIndirectDisposalPrice(),
            Agent
          )
        }

        "capacitor user has started but not completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[IncompleteExampleCompanyDetailsAnswers].copy(
                  acquisitionPrice = None
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(Capacitor))
            ),
            routes.CompanyDetailsController.multipleIndirectDisposalPrice(),
            Individual
          )
        }

        "personal representative user has started but not completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[IncompleteExampleCompanyDetailsAnswers].copy(
                  acquisitionPrice = None
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentative))
            ),
            routes.CompanyDetailsController.multipleIndirectDisposalPrice(),
            Individual
          )
        }

        "individual user has completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[CompleteExampleCompanyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.checkYourAnswers(),
            Individual
          )
        }

        "trust user has completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[CompleteExampleCompanyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.checkYourAnswers(),
            Organisation
          )
        }

        "agent user has completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[CompleteExampleCompanyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = None)
            ),
            routes.CompanyDetailsController.checkYourAnswers(),
            Agent
          )
        }

        "capacitor user has completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[CompleteExampleCompanyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(Capacitor))
            ),
            routes.CompanyDetailsController.checkYourAnswers(),
            Individual
          )
        }

        "personal representative user has completed this section" in {
          test(
            sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(
                sample[CompleteExampleCompanyDetailsAnswers].copy(
                  acquisitionPrice = sample[AmountInPence]
                )
              ),
              triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentative))
            ),
            routes.CompanyDetailsController.checkYourAnswers(),
            Individual
          )
        }

      }

    }

    "handling submitted answers to the multiple indirect acquisition price page" must {

      val key = "multipleIndirectDisposalsAcquisitionPrice"

      def performAction(formData: (String, String)*): Future[Result] =
        controller.multipleIndirectAcquisitionPriceSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.multipleIndirectAcquisitionPriceSubmit(),
        mockUUIDGenerator
      )

      "not update the session" when {

        "the data submitted is the same as one that already exists in session" in {

          val acquisitionPrice = AmountInPence.fromPounds(1000)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleIndirectDisposalsReturn].copy(
                      exampleCompanyDetailsAnswers = Some(
                        sample[IncompleteExampleCompanyDetailsAnswers].copy(
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
            routes.CompanyDetailsController.checkYourAnswers()
          )
        }

      }

      "show a form error for amount" when {

        def test(data: (String, String)*)(expectedErrorMessageKey: String): Unit = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  sample[FillingOutReturn].copy(
                    draftReturn = sample[DraftMultipleIndirectDisposalsReturn].copy(
                      exampleCompanyDetailsAnswers = Some(
                        sample[CompleteExampleCompanyDetailsAnswers]
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
            performAction(data*),
            messageFromMessageKey(s"$key.title"),
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
          amountOfMoneyErrorScenarios(key).foreach { scenario =>
            withClue(s"For $scenario: ") {
              val data = scenario.formData
              test(data*)(scenario.expectedErrorMessageKey)
            }
          }
        }

      }

      "redirect to the cya page" when {

        def test(
          result: => Future[Result],
          oldDraftReturn: DraftReturn,
          updatedDraftReturn: DraftReturn,
          isAmend: Boolean
        ): Unit = {
          val amendReturnData = sample[AmendReturnData]
          val journey         =
            sample[FillingOutReturn].copy(
              draftReturn = oldDraftReturn,
              amendReturnData =
                if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = false)) else None
            )
          val session         = SessionData.empty.copy(journeyStatus = Some(journey))
          val updatedJourney  = journey.copy(
            draftReturn = updatedDraftReturn,
            amendReturnData =
              if (isAmend) Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = true)) else None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreDraftReturn(updatedJourney)(Right(()))
            mockStoreSession(
              session.copy(journeyStatus = Some(updatedJourney))
            )(Right(()))
          }

          checkIsRedirect(
            result,
            routes.CompanyDetailsController.checkYourAnswers()
          )
        }

        "the user is on a multiple disposals journey and has completed this section" in {
          val answers = sample[CompleteExampleCompanyDetailsAnswers].copy(
            acquisitionPrice = AmountInPence.fromPounds(10)
          )

          val oldDraftReturn = sample[DraftMultipleIndirectDisposalsReturn].copy(
            exampleCompanyDetailsAnswers = Some(answers)
          )

          val newDraftReturn = oldDraftReturn.copy(
            exampleCompanyDetailsAnswers = Some(
              answers.copy(
                acquisitionPrice = AmountInPence.fromPounds(100)
              )
            ),
            yearToDateLiabilityAnswers = None,
            gainOrLossAfterReliefs = None
          )

          test(
            performAction(key -> "100"),
            oldDraftReturn,
            newDraftReturn,
            isAmend = false
          )
        }

        "the user hasn't ever answered the acquisition price question " +
          "and the draft return and session data has been successfully updated" in {

            val answers = sample[IncompleteExampleCompanyDetailsAnswers].copy(
              acquisitionPrice = Some(AmountInPence.fromPounds(10))
            )

            val oldDraftReturn = sample[DraftMultipleIndirectDisposalsReturn].copy(
              exampleCompanyDetailsAnswers = Some(answers)
            )

            val newDraftReturn = oldDraftReturn.copy(
              exampleCompanyDetailsAnswers = Some(
                answers.copy(
                  acquisitionPrice = Some(AmountInPence.fromPounds(100))
                )
              ),
              yearToDateLiabilityAnswers = None,
              gainOrLossAfterReliefs = None
            )

            test(
              performAction(key -> "100"),
              oldDraftReturn,
              newDraftReturn,
              isAmend = true
            )
          }

      }

    }

    "handling requests to display the cya page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswers(),
        mockUUIDGenerator
      )

      "redirect to the is uk page" when {

        "there is no address in the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState(companyAddress = None)._1)
          }

          checkIsRedirect(
            performAction(),
            routes.CompanyDetailsController.isUk()
          )
        }

      }

      "display the page" when {

        "there is an address in session" in {
          val address = sample[Address]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualState(companyAddress = Some(address))._1)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("companyDetails.cya.title"),
            doc =>
              CompanyDetailsControllerSpec
                .validatePropertyAddressPage(address, doc)
          )

        }
      }

      "display the page for multiple indirect" when {

        "there is an address in session" in {
          val address = sample[Address]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionWithDraftMultipleIndirectDisposals(
                companyAddress = address
              )._1
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("companyDetails.cya.title"),
            doc =>
              CompanyDetailsControllerSpec
                .validatePropertyAddressPage(address, doc)
          )

        }
      }

    }

    "handling submits on the cya page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like amendReturnToFillingOutReturnSpecBehaviour(
        controller.checkYourAnswersSubmit(),
        mockUUIDGenerator
      )

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

      "redirect to the tasklist for multiple indirect journey" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            sessionWithDraftMultipleIndirectDisposals(
              companyAddress = sample[Address]
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

object CompanyDetailsControllerSpec extends Matchers {

  def validatePropertyAddressPage(
    address: Address,
    doc: Document
  )(implicit messagesApi: MessagesApi, lang: Lang): Unit = {
    val addressLines: List[String] = {
      val lines = address match {
        case UkAddress(line1, line2, town, county, postcode) =>
          List(Some(line1), line2, town, county, Some(postcode.value))
        case Address
              .NonUkAddress(line1, line2, line3, line4, postcode, country) =>
          List(Some(line1), line2, line3, line4, postcode, messagesApi.translate(s"country.${country.code}", Seq.empty))
      }

      lines.collect { case Some(s) => s.trim }
    }

    doc.select("#company-address-answer").text() shouldBe addressLines.mkString(
      " "
    )
  }

}
