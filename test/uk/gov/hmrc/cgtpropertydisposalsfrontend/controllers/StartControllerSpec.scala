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

import java.time.LocalDate

import org.joda.time.{LocalDate => JodaLocalDate}
import cats.data.EitherT
import cats.instances.future._
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class StartControllerSpec extends ControllerSpec with AuthSupport with SessionSupport {

  val mockService = mock[BusinessPartnerRecordService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[BusinessPartnerRecordService].toInstance(mockService)
    )

  lazy val controller = instanceOf[StartController]

  def mockGetBusinessPartnerRecord(nino: NINO, name: models.Name, dob: DateOfBirth)(
    result: Either[Error, BusinessPartnerRecord]
  ) =
    (mockService
      .getBusinessPartnerRecord(_: NINO, _: models.Name, _: DateOfBirth)(
        _: HeaderCarrier
      ))
      .expects(nino, name, dob, *)
      .returning(EitherT.fromEither[Future](result))

  val nino                 = NINO("AB123456C")
  val name                 = Name("forename", "surname")
  val retrievedDateOfBirth = JodaLocalDate.parse("2000-04-10")
  val dateOfBirth          = DateOfBirth(LocalDate.of(2000, 4, 10))
  val emailAddress         = "email"
  val bpr = BusinessPartnerRecord(
    Some(emailAddress),
    UkAddress("line1", None, None, None, "postcode"),
    "sap"
  )
  val subscriptionDetails = SubscriptionDetails(name.forename, name.surname, emailAddress, bpr.address, bpr.sapNumber)

  "The StartController" when {

    "handling requests to start a journey" when {

      def performAction(): Future[Result] =
        controller.start()(FakeRequest())

      "the user is not enrolled and is not subscribed in ETMP" must {

        "redirect to check subscription details" when {

          "there are subscription details in session" in {
            val session = SessionData.empty.copy(subscriptionStatus = Some(SubscriptionReady(subscriptionDetails)))
            inSequence {
              mockAuthWithCl200AndWithAllRetrievals(nino.value, name, retrievedDateOfBirth, None)
              mockGetSession(Future.successful(Right(Some(session))))
            }

            checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
          }

        }

        "redirect to the subscription confirmation page" when {

          "the session data indicates the user has already subscribed" in {
            val subscriptionStatus = SubscriptionComplete(subscriptionDetails, SubscriptionResponse(""))
            val session            = SessionData.empty.copy(subscriptionStatus = Some(subscriptionStatus))

            inSequence {
              mockAuthWithCl200AndWithAllRetrievals(nino.value, name, retrievedDateOfBirth, None)
              mockGetSession(Future.successful(Right(Some(session))))
            }

            checkIsRedirect(performAction(), routes.SubscriptionController.subscribed())
          }

        }

        "display an error page" when {
          "the call to get the BPR fails" in {
            inSequence {
              mockAuthWithCl200AndWithAllRetrievals(nino.value, name, retrievedDateOfBirth, None)
              mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              mockGetBusinessPartnerRecord(nino, name, dateOfBirth)(Left(Error("error")))
            }
            checkIsTechnicalErrorPage(performAction())
          }

          "the call to get BPR succeeds but it cannot be written to session" in {
            val session = SessionData.empty.copy(subscriptionStatus = Some(SubscriptionReady(subscriptionDetails)))

            inSequence {
              mockAuthWithCl200AndWithAllRetrievals(nino.value, name, retrievedDateOfBirth, None)
              mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              mockGetBusinessPartnerRecord(nino, name, dateOfBirth)(Right(bpr))
              mockStoreSession(session)(Future.successful(Left(Error("Oh no!"))))
            }

            checkIsTechnicalErrorPage(performAction())
          }

        }

        "redirect to check subscription details" when {

          "one doesn't exist in session and it is successfully retrieved using the retrieved auth NINO, name and date of birth" in {
            List(
              Some(SessionData.empty),
              None
            ).foreach { maybeSession =>
              val session = SessionData.empty.copy(subscriptionStatus = Some(SubscriptionReady(subscriptionDetails)))

              inSequence {
                mockAuthWithCl200AndWithAllRetrievals(nino.value, name, retrievedDateOfBirth, None)
                mockGetSession(Future.successful(Right(maybeSession)))
                mockGetBusinessPartnerRecord(nino, name, dateOfBirth)(Right(bpr))
                mockStoreSession(session)(Future.successful(Right(())))
              }

              checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
            }
          }

          "an email does not exist in the retrieved BPR but one has been retrieved in the auth record" in {
            val bprWithNoEmail = bpr.copy(emailAddress = None)
            val ggEmail = "email"
            val session = SessionData.empty.copy(subscriptionStatus = Some(
              SubscriptionReady(subscriptionDetails.copy(emailAddress = ggEmail))
            ))

            inSequence {
              mockAuthWithCl200AndWithAllRetrievals(nino.value, name, retrievedDateOfBirth, Some(ggEmail))
              mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              mockGetBusinessPartnerRecord(nino, name, dateOfBirth)(Right(bprWithNoEmail))
              mockStoreSession(session)(Future.successful(Right(())))
            }

            checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
          }

          "one doesn't exist in session and it is successfully retrieved using the retrieved auth NINO and " +
            "there is a BPR already in session" in {
            val session = SessionData.empty.copy(subscriptionStatus = Some(SubscriptionMissingData(bpr)))
            val updatedSession =
              SessionData.empty.copy(subscriptionStatus = Some(SubscriptionReady(subscriptionDetails)))

            inSequence {
              mockAuthWithCl200AndWithAllRetrievals(nino.value, name, retrievedDateOfBirth, None)
              mockGetSession(Future.successful(Right(Some(session))))
              mockStoreSession(updatedSession)(Future.successful(Right(())))
            }
            checkIsRedirect(performAction(), routes.SubscriptionController.checkYourDetails())
          }

        }

        "redirect to enter email" when {

          "there is no email in the BPR or the auth record" in {
            val bprWithNoEmail = bpr.copy(emailAddress = None)
            val updatedSession =
              SessionData.empty.copy(subscriptionStatus = Some(SubscriptionMissingData(bprWithNoEmail)))

            inSequence {
              mockAuthWithCl200AndWithAllRetrievals(nino.value, name, retrievedDateOfBirth, None)
              mockGetSession(Future.successful(Right(Some(SessionData.empty))))
              mockGetBusinessPartnerRecord(nino, name, dateOfBirth)(Right(bprWithNoEmail))
              mockStoreSession(updatedSession)(Future.successful(Right(())))
            }

            checkIsRedirect(performAction(), routes.EmailController.enterEmail().url)
          }

        }

      }
    }

  }

}
