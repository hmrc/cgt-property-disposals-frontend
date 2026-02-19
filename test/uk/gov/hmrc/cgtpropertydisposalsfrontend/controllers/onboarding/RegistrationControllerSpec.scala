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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding

import cats.data.EitherT
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.address.{routes => addressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.email.{routes => emailRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.name.{routes => nameRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.{routes => onboardingRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AuthSupport, ControllerSpec, SessionSupport, onboarding => _, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.RegistrationReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.TryingToGetIndividualsFootprint
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{AlreadySubscribedWithDifferentGGAccount, Registering, RegistrationStatus, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, AddressSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.EmailGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId, SapNumber}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, ContactNameSource, IndividualName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionResponse.{AlreadySubscribed, SubscriptionSuccessful}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
class RegistrationControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with SampledScalaCheck
    with NameFormValidationTests
    with RedirectToStartBehaviour {

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[SubscriptionService].toInstance(mockSubscriptionService)
    )

  private lazy val controller = instanceOf[RegistrationController]

  implicit val messagesApi: MessagesApi = controller.messagesApi

  private val ggCredId = sample[GGCredId]

  private val individualWithInsufficientCLSubscriptionStatus =
    TryingToGetIndividualsFootprint(Some(false), Some(false), None, ggCredId)

  private def mockRegisterWithoutId(
    registrationDetails: RegistrationDetails
  )(result: Either[Error, RegisteredWithoutId]) =
    (mockSubscriptionService
      .registerWithoutId(_: RegistrationDetails)(using _: HeaderCarrier))
      .expects(registrationDetails, *)
      .returning(EitherT(Future.successful(result)))

  private def mockSubscribe(
    subscriptionDetails: SubscriptionDetails,
    lang: Lang
  )(result: Either[Error, SubscriptionResponse]) =
    (mockSubscriptionService
      .subscribe(_: SubscriptionDetails, _: Lang)(using _: HeaderCarrier))
      .expects(subscriptionDetails, lang, *)
      .returning(EitherT(Future.successful(result)))

  "RegistrationController" when {

    def redirectToStartBehaviour(performAction: () => Future[Result]): Unit =
      redirectToStartWhenInvalidJourney(
        () => performAction(),
        {
          case TryingToGetIndividualsFootprint(
                Some(false),
                Some(false),
                _,
                _
              ) =>
            true
          case _: RegistrationStatus => true
          case _                     => false
        }
      )

    "handling requests to show the select entity type page" must {

      def performAction(): Future[Result] =
        controller.selectEntityType()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      "show the page" when {

        "the session data indicates that the user has no digital footprint and " +
          "the user has opted to start registration" in {
            val sessionData =
              SessionData.empty.copy(journeyStatus = Some(individualWithInsufficientCLSubscriptionStatus))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
            }

            val result = performAction()
            status(result)        shouldBe OK
            contentAsString(result) should include(
              messageFromMessageKey("entityType.title")
            )
          }
      }

      "prepopulate the form if the user has previously answered the question" in {
        List(
          RegistrationStatus.IndividualWantsToRegisterTrust(ggCredId),
          RegistrationStatus
            .IndividualSupplyingInformation(None, None, None, None, ggCredId)
        ).foreach { journeyStatus =>
          val sessionData =
            SessionData.empty.copy(
              journeyStatus = Some(journeyStatus)
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          val result = performAction()
          status(result)        shouldBe OK
          contentAsString(result) should include(
            messageFromMessageKey("entityType.title")
          )
          contentAsString(result) should include("checked")
        }

      }

    }

    "handling requests to submit the selected entity type" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.selectEntityTypeSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      val sessionData =
        SessionData.empty.copy(journeyStatus = Some(individualWithInsufficientCLSubscriptionStatus))

      behave like redirectToStartBehaviour(() => performAction())

      "show the page with errors" when {
        "the request submits no selection" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }

          val result = performAction()
          status(result)        shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("entityType.error.required")
          )
        }

        "the request submits an invalid value" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }
          val result = performAction("entityType" -> "2")
          status(result)        shouldBe BAD_REQUEST
          contentAsString(result) should include(
            messageFromMessageKey("entityType.invalid")
          )
        }
      }

      "redirect to the wrong gg account page" when {
        "the request selects trust" in {
          val updatedSession =
            sessionData.copy(journeyStatus = Some(RegistrationStatus.IndividualWantsToRegisterTrust(ggCredId)))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(updatedSession)(Right(()))
          }
          val result = performAction("entityType" -> "0")
          checkIsRedirect(
            result,
            onboardingRoutes.RegistrationController.wrongGGAccountForTrusts()
          )
        }

        "continue the registration journey" when {
          "the request selects individual" in {
            val updatedSession =
              sessionData.copy(
                journeyStatus = Some(
                  RegistrationStatus.IndividualSupplyingInformation(
                    None,
                    None,
                    None,
                    None,
                    ggCredId
                  )
                )
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
              mockStoreSession(updatedSession)(Right(()))
            }
            val result = performAction("entityType" -> "1")
            checkIsRedirect(
              result,
              controllers.onboarding.name.routes.RegistrationEnterIndividualNameController
                .enterIndividualName()
            )
          }

        }

        "display an error page" when {

          "the session cannot be updated" in {
            List[(String, RegistrationStatus)](
              "1" -> RegistrationStatus.IndividualSupplyingInformation(
                None,
                None,
                None,
                None,
                ggCredId
              ),
              "0" -> RegistrationStatus.IndividualWantsToRegisterTrust(ggCredId)
            ).foreach { case (entityType, registrationStatus) =>
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(sessionData)
                mockStoreSession(
                  sessionData.copy(journeyStatus = Some(registrationStatus))
                )(Left(Error("")))
              }

              checkIsTechnicalErrorPage(
                performAction("entityType" -> entityType)
              )
            }

          }

        }

        "not update the session" when {

          "the user selects trust and has previously indicated that they wish to register a trust" in {
            val session =
              sessionData.copy(journeyStatus =
                Some(
                  RegistrationStatus.IndividualWantsToRegisterTrust(ggCredId)
                )
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }
            val result = performAction("entityType" -> "0")
            checkIsRedirect(
              result,
              onboardingRoutes.RegistrationController.wrongGGAccountForTrusts()
            )

          }

          "the user selects individual and has previously indicated that they wish to register as an individual" in {
            val session =
              sessionData.copy(
                journeyStatus = Some(
                  RegistrationStatus.IndividualSupplyingInformation(
                    None,
                    None,
                    None,
                    None,
                    ggCredId
                  )
                )
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }
            val result = performAction("entityType" -> "1")
            checkIsRedirect(
              result,
              nameRoutes.RegistrationEnterIndividualNameController
                .enterIndividualName()
            )

          }

        }

      }

    }

    "handling requests to view the wrong gg account page" must {

      def performAction(): Future[Result] =
        controller.wrongGGAccountForTrusts()(FakeRequest())

      val sessionData =
        SessionData.empty.copy(
          journeyStatus = Some(RegistrationStatus.IndividualWantsToRegisterTrust(ggCredId))
        )

      behave like redirectToStartBehaviour(() => performAction())

      "show the page" when {
        "the endpoint is requested" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
          }
          val result = performAction()
          status(result)        shouldBe OK
          contentAsString(result) should include(
            messageFromMessageKey("wrongAccountForTrusts.title")
          )
        }
      }

      "redirect to the start registration endpoint" when {

        "the session indicates that they want to register as an individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              sessionData
                .copy(
                  journeyStatus = Some(
                    RegistrationStatus.IndividualSupplyingInformation(
                      None,
                      None,
                      None,
                      None,
                      ggCredId
                    )
                  )
                )
            )
          }
          checkIsRedirect(
            performAction(),
            onboardingRoutes.RegistrationController.selectEntityType()
          )
        }

      }

    }

    "handling requests to display the check your answers page" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswers()(FakeRequest())

      redirectToStartWhenInvalidJourney(
        () => performAction(),
        {
          case _: RegistrationStatus.RegistrationReady | _: RegistrationStatus.IndividualSupplyingInformation |
              Registering =>
            true
          case _ => false
        }
      )

      "redirect to the enter name page" when {

        "no name can be found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  RegistrationStatus.IndividualSupplyingInformation(
                    None,
                    None,
                    None,
                    None,
                    ggCredId
                  )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            nameRoutes.RegistrationEnterIndividualNameController
              .enterIndividualName()
          )
        }

      }

      "redirect to the enter address journey" when {

        "no address can be found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  RegistrationStatus
                    .IndividualSupplyingInformation(
                      Some(sample[IndividualName]),
                      None,
                      None,
                      None,
                      ggCredId
                    )
                )
              )
            )
          }

          checkIsRedirect(
            performAction(),
            addressRoutes.RegistrationEnterAddressController.isUk()
          )
        }

      }

      "redirect to the enter email journey" when {

        "no email can be found" in {
          val name    = sample[IndividualName]
          val address = sample[Address]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  RegistrationStatus.IndividualSupplyingInformation(
                    Some(name),
                    Some(address),
                    None,
                    None,
                    ggCredId
                  )
                )
              )
            )
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  RegistrationStatus
                    .IndividualMissingEmail(name, address, ggCredId)
                )
              )
            )(Right(()))
          }

          checkIsRedirect(
            performAction(),
            emailRoutes.RegistrationEnterEmailController.enterEmail()
          )
        }

      }

      "show an error page" when {

        "the session cannot be updated when there is no email" in {
          val name    = sample[IndividualName]
          val address = sample[Address]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  RegistrationStatus.IndividualSupplyingInformation(
                    Some(name),
                    Some(address),
                    None,
                    None,
                    ggCredId
                  )
                )
              )
            )
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  RegistrationStatus
                    .IndividualMissingEmail(name, address, ggCredId)
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "the session cannot be updated when all the necessary details can be found" in {
          val name        = sample[IndividualName]
          val address     = sample[Address]
          val email       = sample[Email]
          val emailSource = sample[EmailSource]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  RegistrationStatus
                    .IndividualSupplyingInformation(
                      Some(name),
                      Some(address),
                      Some(email),
                      Some(emailSource),
                      ggCredId
                    )
                )
              )
            )
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  RegistrationStatus.RegistrationReady(
                    RegistrationDetails(name, email, address, emailSource),
                    ggCredId
                  )
                )
              )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "display the page" when {

        "the session data indicates the user us ready to register" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  RegistrationStatus.RegistrationReady(
                    RegistrationDetails(
                      sample[IndividualName],
                      sample[Email],
                      sample[Address],
                      sample[EmailSource]
                    ),
                    ggCredId
                  )
                )
              )
            )
          }

          val result = performAction()
          status(result) shouldBe OK
        }

        "the user has just finished supplying all the necessary details" in {
          val name        = sample[IndividualName]
          val address     = sample[Address]
          val email       = sample[Email]
          val emailSource = sample[EmailSource]

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  RegistrationStatus
                    .IndividualSupplyingInformation(
                      Some(name),
                      Some(address),
                      Some(email),
                      Some(emailSource),
                      ggCredId
                    )
                )
              )
            )
            mockStoreSession(
              SessionData.empty.copy(
                journeyStatus = Some(
                  RegistrationStatus.RegistrationReady(
                    RegistrationDetails(name, email, address, emailSource),
                    ggCredId
                  )
                )
              )
            )(Right(()))
          }

          val result = performAction()
          status(result) shouldBe OK
        }

      }

    }

    "handling submit check your answers requests" must {

      def performAction(): Future[Result] =
        controller.checkYourAnswersSubmit()(FakeRequest())

      behave like redirectToStartWhenInvalidJourney(
        () => performAction(),
        {
          case RegistrationReady(_, _) => true
          case _                       => false
        }
      )

      val registrationReady              = sample[RegistrationReady]
      val sessionData                    =
        SessionData.empty.copy(journeyStatus = Some(registrationReady))
      val subscriptionSuccessfulResponse = sample[SubscriptionSuccessful]
      val sapNumber                      = sample[SapNumber]
      val subscriptionDetails            = {
        val details = registrationReady.registrationDetails
        SubscriptionDetails(
          Right(details.name),
          details.emailAddress,
          details.address,
          ContactName(s"${details.name.firstName} ${details.name.lastName}"),
          sapNumber,
          details.emailSource,
          AddressSource.ManuallyEntered,
          ContactNameSource.ManuallyEntered
        )
      }

      val subscribedDetails =
        SubscribedDetails(
          subscriptionDetails.name,
          subscriptionDetails.emailAddress,
          subscriptionDetails.address,
          subscriptionDetails.contactName,
          CgtReference(subscriptionSuccessfulResponse.cgtReferenceNumber),
          None,
          registeredWithId = false
        )

      val sessionWithJourneyStatusRegistering =
        SessionData.empty.copy(journeyStatus = Some(Registering))

      "show an error page" when {

        "the call to register without id fails" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(sessionWithJourneyStatusRegistering)(Right(()))
            mockRegisterWithoutId(registrationReady.registrationDetails)(
              Left(Error(""))
            )
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "the call to subscribe fails" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(sessionWithJourneyStatusRegistering)(Right(()))
            mockRegisterWithoutId(registrationReady.registrationDetails)(
              Right(RegisteredWithoutId(sapNumber))
            )
            mockSubscribe(subscriptionDetails, lang)(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

        "the session data cannot be updated" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(sessionWithJourneyStatusRegistering)(Right(()))
            mockRegisterWithoutId(registrationReady.registrationDetails)(
              Right(RegisteredWithoutId(sapNumber))
            )
            mockSubscribe(subscriptionDetails, lang)(
              Right(subscriptionSuccessfulResponse)
            )
            mockStoreSession(
              SessionData.empty
                .copy(journeyStatus =
                  Some(
                    Subscribed(
                      subscribedDetails,
                      registrationReady.ggCredId,
                      None,
                      List.empty,
                      List.empty
                    )
                  )
                )
            )(Left(Error("")))
          }

          checkIsTechnicalErrorPage(performAction())
        }

      }

      "redirect to the subscription confirmation page" when {

        "the call to register without id and subscribe succeeds and the " +
          "session data has been updated" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(sessionData)
              mockStoreSession(sessionWithJourneyStatusRegistering)(Right(()))
              mockRegisterWithoutId(registrationReady.registrationDetails)(
                Right(RegisteredWithoutId(sapNumber))
              )
              mockSubscribe(subscriptionDetails, lang)(
                Right(subscriptionSuccessfulResponse)
              )
              mockStoreSession(
                SessionData.empty
                  .copy(journeyStatus =
                    Some(
                      Subscribed(
                        subscribedDetails,
                        registrationReady.ggCredId,
                        None,
                        List.empty,
                        List.empty
                      )
                    )
                  )
              )(Right(()))
            }

            checkIsRedirect(
              performAction(),
              controllers.onboarding.routes.SubscriptionController.subscribed()
            )
          }

      }

      "redirect to the already subscribed with different gg account page" when {

        "the subscription response indicates that the user has already subscribed" in {
          val sessionWithAlreadySubscribed =
            SessionData.empty.copy(
              journeyStatus = Some(
                AlreadySubscribedWithDifferentGGAccount(
                  registrationReady.ggCredId,
                  None
                )
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(sessionData)
            mockStoreSession(sessionWithJourneyStatusRegistering)(Right(()))
            mockRegisterWithoutId(registrationReady.registrationDetails)(
              Right(RegisteredWithoutId(sapNumber))
            )
            mockSubscribe(subscriptionDetails, lang)(Right(AlreadySubscribed))
            mockStoreSession(sessionWithAlreadySubscribed)(Right(()))
          }

          checkIsRedirect(
            performAction(),
            controllers.onboarding.routes.SubscriptionController
              .alreadySubscribedWithDifferentGGAccount()
          )
        }
      }

    }

  }

}
