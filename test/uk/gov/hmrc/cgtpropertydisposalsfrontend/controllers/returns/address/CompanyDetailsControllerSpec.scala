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
import org.jsoup.nodes.Document
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.BAD_REQUEST
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.Agent
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Country, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftSingleIndirectDisposalReturn, IndividualUserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import scala.concurrent.Future

class CompanyDetailsControllerSpec
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
        case FillingOutReturn(_, _, _, _: DraftSingleIndirectDisposalReturn) =>
          true
        case _                                                               => false
      }
    )

  override val overrideBindings = List[GuiceableModule](
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[ReturnsService].toInstance(mockReturnsService)
  )

  lazy val controller = instanceOf[CompanyDetailsController]

  implicit lazy val messagesApi: MessagesApi = controller.messagesApi

  def sessionWithDraftSingleIndirectDisposal(
    name: Either[TrustName, IndividualName],
    userType: UserType,
    individualUserType: Option[IndividualUserType],
    companyAddress: Option[Address]
  ): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) = {
    val draftReturn      = sample[DraftSingleIndirectDisposalReturn].copy(
      triageAnswers = sample[CompleteSingleDisposalTriageAnswers]
        .copy(individualUserType = individualUserType),
      companyAddress = companyAddress
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
    companyAddress: Option[Address] = None
  ): (SessionData, FillingOutReturn, DraftSingleIndirectDisposalReturn) =
    sessionWithDraftSingleIndirectDisposal(
      Right(sample[IndividualName]),
      UserType.Individual,
      Some(Self),
      companyAddress
    )

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

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        def test(result: Future[Result], expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            doc =>
              doc
                .select("#content > article > form")
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

    }

    "handling submits on the is uk page" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.isUkSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData: _*)
        )

      behave like redirectToStartBehaviour(() => performAction())

      "show a form error" when {

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

    }

    "handling requests to display the enter uk company address page" must {

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
                "companyDetails.caption"
              )

              doc
                .select("#address > div:nth-child(2) > label")
                .text() shouldBe messageFromMessageKey(
                "address.uk.companyDetails.line1.label"
              )

              doc
                .select("#address > div:nth-child(3) > label")
                .text() shouldBe messageFromMessageKey(
                "address.uk.companyDetails.line2.label"
              )

              doc
                .select("#content > article > form")
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

    }

    "handling submitted uk company address" must {

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
            "postcode.companyDetails.error.invalidCharacters",
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
            UkAddress("The Company", None, None, None, Postcode("ZZ10ZZ"))

          val newDraftReturn = draftReturn.copy(companyAddress = Some(address))
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
            routes.CompanyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the enter non-uk company address page" must {

      def performAction(): Future[Result] =
        controller.enterNonUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "display the page" when {

        def test(result: Future[Result], expectedTitleKey: String): Unit =
          checkPageIsDisplayed(
            result,
            messageFromMessageKey(expectedTitleKey),
            { doc =>
              doc
                .select("#nonUkAddress > legend > h1 > span")
                .text() shouldBe messageFromMessageKey(
                "companyDetails.caption"
              )

              doc
                .select("#nonUkAddress > div:nth-child(2) > label")
                .text() shouldBe messageFromMessageKey(
                "nonUkAddress.companyDetails.line1.label"
              )

              doc
                .select("#nonUkAddress > div:nth-child(3) > label")
                .text() shouldBe messageFromMessageKey(
                "nonUkAddress.companyDetails.line2.label"
              )

              doc
                .select("#content > article > form")
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

    }

    "handling submitted non-uk company address" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterNonUkAddressSubmit()(
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

      "show an error page" when {

        val (session, journey, draftReturn) = individualState()
        val address                         = NonUkAddress(
          "The Company",
          None,
          None,
          None,
          None,
          Country("HK", Some("Hong Kong"))
        )

        val newDraftReturn = draftReturn.copy(companyAddress = Some(address))
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
              "nonUkAddress-line1" -> address.line1,
              "countryCode"        -> address.country.code
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
            Country("HK", Some("Hong Kong"))
          )

          val newDraftReturn = draftReturn.copy(companyAddress = Some(address))
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
              "nonUkAddress-line1" -> address.line1,
              "countryCode"        -> address.country.code
            ),
            routes.CompanyDetailsController.checkYourAnswers()
          )
        }

      }

    }

    "handling requests to display the cya page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

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

object CompanyDetailsControllerSpec extends Matchers {

  def validatePropertyAddressPage(
    address: Address,
    doc: Document
  ): Unit = {
    val addressLines: List[String] = {
      val lines = address match {
        case UkAddress(line1, line2, town, county, postcode) =>
          List(Some(line1), line2, town, county, Some(postcode.value))
        case Address
              .NonUkAddress(line1, line2, line3, line4, postcode, country) =>
          List(Some(line1), line2, line3, line4, postcode, country.name)
      }

      lines.collect { case Some(s) => s.trim }
    }

    doc.select("#company-address-answer").text() shouldBe addressLines.mkString(
      " "
    )
  }

}
