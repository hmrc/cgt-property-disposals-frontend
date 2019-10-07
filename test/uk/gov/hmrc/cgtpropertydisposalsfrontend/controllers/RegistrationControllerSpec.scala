/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import cats.Eq
import org.scalacheck.ScalacheckShapeless._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.CSRFTokenHelper._
import play.api.test.Helpers._
import shapeless.{Lens, lens}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.name.{routes => nameroutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.TryingToGetIndividualsFootprint
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Email, Error, JourneyStatus, SessionData, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore

import scala.concurrent.Future

class RegistrationControllerSpec
  extends ControllerSpec with AuthSupport with SessionSupport with ScalaCheckDrivenPropertyChecks with NameFormValidationTests with RedirectToStartBehaviour {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  lazy val controller = instanceOf[RegistrationController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  implicit val subscriptionStatusEq: Eq[SubscriptionStatus] = Eq.fromUniversalEquals

  val name = sample[IndividualName]

  val individualWithInsufficentCLSubscriptionStatus =
    TryingToGetIndividualsFootprint(Some(false), Some(false), None, GGCredId("id"))

  val journeyStatusLens: Lens[SessionData, Option[JourneyStatus]] = lens[SessionData].journeyStatus

  "RegistrationController" when {

    def redirectToStartBehaviour(performAction: () => Future[Result]): Unit = {
      redirectToStartWhenInvalidJourney(
        performAction,
        {
          case TryingToGetIndividualsFootprint(Some(false), Some(false), _, _) => true
          case r: RegistrationStatus => true
          case _ => false
        }
      )
    }

    "handling requests to show the registration start page" must {

      def performAction(): Future[Result] =
        controller.startRegistration()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "show the page" when {

        "the session data indicates that the user has no digital footprint and " +
          "they have indicated that they have no NINO or SA UTR" in {
          val sessionData =
            SessionData.empty.copy(journeyStatus = Some(individualWithInsufficentCLSubscriptionStatus))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("registration.start.title"))
        }

      }

    }

    "handling requests to show the select entity type page" must {

      def performAction(): Future[Result] =
        controller.selectEntityType()(FakeRequest())

      behave like redirectToStartBehaviour(performAction)

      "show the page" when {

        "the session data indicates that the user has no digital footprint and " +
          "the user has opted to start registration" in {
          val sessionData =
            SessionData.empty.copy(journeyStatus = Some(individualWithInsufficentCLSubscriptionStatus))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("entityType.title"))
        }
      }

      "prepopulate the form if the user has previously answered the question" in {
        List(
          RegistrationStatus.IndividualWantsToRegisterTrust,
          RegistrationStatus.IndividualSupplyingInformation(None, None, None)
        ).foreach{ journeyStatus =>
          val sessionData =
            SessionData.empty.copy(
              journeyStatus = Some(journeyStatus)
            )

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("entityType.title"))
          contentAsString(result) should include("""checked="checked"""")
        }

      }

    }

    "handling requests to submit the selected entity type" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.selectEntityTypeSubmit()(FakeRequest().withFormUrlEncodedBody(formData: _*).withCSRFToken)

      val sessionData =
        SessionData.empty.copy(journeyStatus = Some(individualWithInsufficentCLSubscriptionStatus))

      behave like redirectToStartBehaviour(() => performAction())

      "show the page with errors" when {
        "the request submits no selection" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }

          val result = performAction()
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("entityType.error.required"))
        }

        "the request submits an invalid value" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction("entityType" -> "2")
          status(result) shouldBe BAD_REQUEST
          contentAsString(result) should include(message("entityType.invalid"))
        }
      }

      "redirect to the wrong gg account page" when {
        "the request selects trust" in {
          val updatedSession =
            sessionData.copy(journeyStatus = Some(RegistrationStatus.IndividualWantsToRegisterTrust))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
            mockStoreSession(updatedSession)(Future.successful(Right(())))
          }
          val result = performAction("entityType" -> "1")
          checkIsRedirect(result, routes.RegistrationController.wrongGGAccountForTrusts())
        }

      "continue the registration journey" when {
        "the request selects individual" in {
          val updatedSession =
            sessionData.copy(journeyStatus = Some(RegistrationStatus.IndividualSupplyingInformation(None, None, None)))

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
            mockStoreSession(updatedSession)(Future.successful(Right(())))
          }
          val result = performAction("entityType" -> "0")
          checkIsRedirect(result, nameroutes.RegistrationEnterIndividualNameController.enterIndividualName())
        }

      }

      "display an error page" when {

        "the session cannot be updated" in {
          List[(String,RegistrationStatus)](
            "0" -> RegistrationStatus.IndividualSupplyingInformation(None, None, None),
            "1" -> RegistrationStatus.IndividualWantsToRegisterTrust
          ).foreach{ case (entityType, registrationStatus) =>

            inSequence{
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(sessionData))))
              mockStoreSession(sessionData.copy(journeyStatus = Some(registrationStatus)))(Future.successful(Left(Error(""))))
            }

            checkIsTechnicalErrorPage(performAction("entityType" -> entityType))
          }


        }

      }

        "not update the session" when {

          "the user selects trust and has previously indicated that they wish to register a trust" in {
            val session =
              sessionData.copy(journeyStatus = Some(RegistrationStatus.IndividualWantsToRegisterTrust))

            inSequence{
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(session))))
            }
            val result = performAction("entityType" -> "1")
            checkIsRedirect(result, routes.RegistrationController.wrongGGAccountForTrusts())

          }

          "the user selects individual and has previously indicated that they wish to register as an individual" in {
            val session =
              sessionData.copy(journeyStatus = Some(RegistrationStatus.IndividualSupplyingInformation(None, None, None)))

            inSequence{
              mockAuthWithNoRetrievals()
              mockGetSession(Future.successful(Right(Some(session))))
            }
            val result = performAction("entityType" -> "0")
            checkIsRedirect(result, nameroutes.RegistrationEnterIndividualNameController.enterIndividualName())

          }

        }

      }

    }

    "handling requests to view the wrong gg account page" must {

      def performAction(): Future[Result] =
        controller.wrongGGAccountForTrusts()(FakeRequest())

      val sessionData =
        SessionData.empty.copy(
          journeyStatus = Some(RegistrationStatus.IndividualWantsToRegisterTrust)
        )

      behave like redirectToStartBehaviour(performAction)

      "show the page" when {
        "the endpoint is requested" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(sessionData))))
          }
          val result = performAction()
          status(result) shouldBe OK
          contentAsString(result) should include(message("wrongAccountForTrusts.title"))
        }
      }

      "redirect to the start registration endpoint" when {

        "the session indicates that they want to register as an individual" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              sessionData.copy(journeyStatus = Some(RegistrationStatus.IndividualSupplyingInformation(None, None, None))))
            )))
          }
          checkIsRedirect(performAction(), routes.RegistrationController.startRegistration())
        }

      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] = controller.checkYourAnswers()(FakeRequest())

      redirectToStartWhenInvalidJourney(
        performAction,
        {
          case _: RegistrationStatus.RegistrationReady | _: RegistrationStatus.IndividualSupplyingInformation => true
          case _ => false
        }
      )

      "redirect to the enter name page" when {

        "no name can be found" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              SessionData.empty.copy(journeyStatus = Some(
                RegistrationStatus.IndividualSupplyingInformation(None, None, None)
              ))
            ))))
          }

          checkIsRedirect(performAction(), nameroutes.RegistrationEnterIndividualNameController.enterIndividualName())
        }

      }

      "redirect to the enter address journey" when {

        "no address can be found" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              SessionData.empty.copy(journeyStatus = Some(
                RegistrationStatus.IndividualSupplyingInformation(Some(sample[IndividualName]), None, None)
              ))
            ))))
          }

          checkIsRedirect(performAction(), address.routes.RegistrationEnterAddressController.isUk())
        }

      }

      "redirect to the enter email journey" when {

        "no email can be found" in {
          val name = sample[IndividualName]
          val address = sample[Address]

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              SessionData.empty.copy(journeyStatus = Some(
                RegistrationStatus.IndividualSupplyingInformation(Some(name), Some(address), None)
              ))
            ))))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(
              RegistrationStatus.IndividualMissingEmail(name, address)
            )))(Future.successful(Right(())))
          }

          checkIsRedirect(performAction(), email.routes.RegistrationEnterEmailController.enterEmail())
        }

      }

      "show an error page" when {

        "the session cannot be updated when there is no email" in {
          val name = sample[IndividualName]
          val address = sample[Address]

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              SessionData.empty.copy(journeyStatus = Some(
                RegistrationStatus.IndividualSupplyingInformation(Some(name), Some(address), None)
              ))
            ))))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(
              RegistrationStatus.IndividualMissingEmail(name, address)
            )))(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "the session cannot be updated when all the necessary details can be found" in {
          val name = sample[IndividualName]
          val address = sample[Address]
          val email = sample[Email]

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              SessionData.empty.copy(journeyStatus = Some(
                RegistrationStatus.IndividualSupplyingInformation(Some(name), Some(address), Some(email))
              ))
            ))))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(
              RegistrationStatus.RegistrationReady(name, address, email)
            )))(Future.successful(Left(Error(""))))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "display the page" when {

        "the session data indicates the user us ready to register" in {
          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              SessionData.empty.copy(journeyStatus = Some(
                RegistrationStatus.RegistrationReady(sample[IndividualName], sample[Address], sample[Email])
              ))
            ))))
          }

          val result = performAction()
          status(result) shouldBe OK
        }

        "the user has just finished supplying all the necessary details" in {
          val name = sample[IndividualName]
          val address = sample[Address]
          val email = sample[Email]

          inSequence{
            mockAuthWithNoRetrievals()
            mockGetSession(Future.successful(Right(Some(
              SessionData.empty.copy(journeyStatus = Some(
                RegistrationStatus.IndividualSupplyingInformation(Some(name), Some(address), Some(email))
              ))
            ))))
            mockStoreSession(SessionData.empty.copy(journeyStatus = Some(
              RegistrationStatus.RegistrationReady(name, address, email)
            )))(Future.successful(Right(())))
          }

          val result = performAction()
          status(result) shouldBe OK
        }


      }

    }

  }

}
