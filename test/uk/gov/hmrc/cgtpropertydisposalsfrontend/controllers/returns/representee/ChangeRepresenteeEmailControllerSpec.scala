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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee

import cats.syntax.either._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.EmailControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailJourneyType.Returns.ChangingRepresenteeEmail
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService

import java.util.UUID
import scala.concurrent.Future

trait ChangeRepresenteeEmailControllerSpec
    extends EmailControllerSpec[ChangingRepresenteeEmail]
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with ReturnsServiceSupport {

  override def toJourneyStatus(
    journeyType: ChangingRepresenteeEmail
  ): JourneyStatus = journeyType.journey.merge

  val validJourneyStatus: ChangingRepresenteeEmail

  val validVerificationCompleteJourneyStatus: ChangingRepresenteeEmail

  val mockUpdateEmail: Option[(ChangingRepresenteeEmail, ChangingRepresenteeEmail, Either[Error, Unit]) => Unit]

  override lazy val controller: ChangeRepresenteeEmailController = instanceOf[ChangeRepresenteeEmailController]

  protected override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[EmailVerificationService].toInstance(mockService),
      bind[UUIDGenerator].toInstance(mockUuidGenerator),
      bind[SubscriptionService].toInstance(mockSubscriptionService),
      bind[ReturnsService].toInstance(mockReturnsService)
    )

  override def updateEmail(
    changingRepresenteeEmail: ChangingRepresenteeEmail,
    email: Email
  ): ChangingRepresenteeEmail = {
    val newAnswers = updateAnswers(
      changingRepresenteeEmail.answers,
      changingRepresenteeEmail.contactDetails,
      email
    )

    ChangingRepresenteeEmail(
      changingRepresenteeEmail.journey.bimap(
        _.copy(representeeAnswers = Some(newAnswers)),
        updateFillingOutReturn(_, newAnswers)
      ),
      newAnswers,
      changingRepresenteeEmail.contactDetails.copy(emailAddress = email)
    )
  }

  implicit val messagesApi: MessagesApi = controller.messagesApi

  protected def updateAnswers(
    answers: RepresenteeAnswers,
    contactDetails: RepresenteeContactDetails,
    newEmail: Email
  ): IncompleteRepresenteeAnswers = {
    val newContactDetails = contactDetails.copy(emailAddress = newEmail)

    answers.fold(
      _.copy(
        contactDetails = Some(newContactDetails),
        hasConfirmedContactDetails = false
      ),
      complete =>
        IncompleteRepresenteeAnswers(
          Some(complete.name),
          Some(complete.id),
          complete.dateOfDeath,
          Some(newContactDetails),
          hasConfirmedPerson = true,
          hasConfirmedContactDetails = false,
          Some(complete.isFirstReturn)
        )
    )
  }

  protected def updateFillingOutReturn(
    fillingOutReturn: FillingOutReturn,
    newAnswers: RepresenteeAnswers
  ): FillingOutReturn =
    fillingOutReturn.copy(draftReturn =
      fillingOutReturn.draftReturn.fold(
        _.copy(representeeAnswers = Some(newAnswers)),
        _.copy(representeeAnswers = Some(newAnswers)),
        _.copy(representeeAnswers = Some(newAnswers)),
        _.copy(representeeAnswers = Some(newAnswers)),
        _.copy(representeeAnswers = Some(newAnswers))
      )
    )

  def redirectToStartBehaviour(performAction: () => Future[Result]): Unit = {
    def isDefinedAndContainsContactDetails(
      answers: Option[RepresenteeAnswers]
    ): Boolean =
      answers.exists(_.fold(_.contactDetails.isDefined, _ => true))

    redirectToStartWhenInvalidJourney(
      () => performAction(),
      {
        case StartingNewDraftReturn(_, _, _, _, representeeAnswers, _)
            if isDefinedAndContainsContactDetails(representeeAnswers) =>
          true
        case FillingOutReturn(_, _, _, s: DraftSingleDisposalReturn, _, _)
            if isDefinedAndContainsContactDetails(s.representeeAnswers) =>
          true
        case FillingOutReturn(_, _, _, m: DraftMultipleDisposalsReturn, _, _)
            if isDefinedAndContainsContactDetails(m.representeeAnswers) =>
          true
        case FillingOutReturn(_, _, _, i: DraftSingleIndirectDisposalReturn, _, _)
            if isDefinedAndContainsContactDetails(i.representeeAnswers) =>
          true
        case FillingOutReturn(_, _, _, i: DraftSingleMixedUseDisposalReturn, _, _)
            if isDefinedAndContainsContactDetails(i.representeeAnswers) =>
          true
        case FillingOutReturn(_, _, _, i: DraftMultipleIndirectDisposalsReturn, _, _)
            if isDefinedAndContainsContactDetails(i.representeeAnswers) =>
          true
        case _ => false
      }
    )
  }

  "SubscribedChangeEmailController" when {

    "handling requests to display the enter email page" must {

      def performAction(): Future[Result] =
        controller.enterEmail()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())
      behave like enterEmailPage(() => performAction())

    }

    "handling submitted email addresses" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.enterEmailSubmit()(
          FakeRequest().withFormUrlEncodedBody(data*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like enterEmailSubmit(
        performAction,
        validJourneyStatus.journey
          .fold(_.subscribedDetails, _.subscribedDetails)
          .contactName,
        routes.ChangeRepresenteeEmailController.verifyEmail,
        routes.ChangeRepresenteeEmailController.checkYourInbox()
      )
    }

    "handling requests to display the check your inbox page" must {

      def performAction(): Future[Result] =
        controller.checkYourInbox()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like checkYourInboxPage(
        () => performAction(),
        routes.ChangeRepresenteeEmailController.enterEmail(),
        routes.ChangeRepresenteeEmailController.enterEmail().url
      )
    }

    "handling requests to verify an email" must {

      def performAction(id: UUID): Future[Result] =
        controller.verifyEmail(id)(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction(UUID.randomUUID()))

      behave like verifyEmail(
        performAction,
        routes.ChangeRepresenteeEmailController.enterEmail(),
        routes.ChangeRepresenteeEmailController.emailVerified()
      )

    }

    "handling requests to display the email verified page" must {

      def performAction(): Future[Result] =
        controller.emailVerified()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like emailVerifiedPage(
        () => performAction(),
        routes.RepresenteeController.checkYourAnswers(),
        routes.ChangeRepresenteeEmailController.enterEmail()
      )
    }

  }

}

class StartingNewDraftReturnChangeRepresenteeEmailSpec extends ChangeRepresenteeEmailControllerSpec {

  override val validJourneyStatus: ChangingRepresenteeEmail = {
    val answers                = sample[CompleteRepresenteeAnswers]
    val startingNewDraftReturn =
      sample[StartingNewDraftReturn].copy(representeeAnswers = Some(answers))
    ChangingRepresenteeEmail(
      Left(startingNewDraftReturn),
      answers,
      answers.contactDetails
    )
  }

  override val validVerificationCompleteJourneyStatus: ChangingRepresenteeEmail = validJourneyStatus

  override val mockUpdateEmail
    : Option[(ChangingRepresenteeEmail, ChangingRepresenteeEmail, Either[Error, Unit]) => Unit] = None

}

class FillingOutReturnChangeRepresenteeEmailSpec extends ChangeRepresenteeEmailControllerSpec {

  override val validJourneyStatus: ChangingRepresenteeEmail = {
    val contactDetails   = sample[RepresenteeContactDetails]
    val answers          = sample[IncompleteRepresenteeAnswers]
      .copy(contactDetails = Some(contactDetails))
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = sample[DraftSingleDisposalReturn].copy(
        representeeAnswers = Some(answers)
      )
    )
    ChangingRepresenteeEmail(
      Right(fillingOutReturn),
      answers,
      contactDetails
    )
  }

  override val validVerificationCompleteJourneyStatus: ChangingRepresenteeEmail = validJourneyStatus

  override val mockUpdateEmail
    : Option[(ChangingRepresenteeEmail, ChangingRepresenteeEmail, Either[Error, Unit]) => Unit] = Some {
    case (
          journey: ChangingRepresenteeEmail,
          newDetails: ChangingRepresenteeEmail,
          mockResult: Either[Error, Unit]
        ) =>
      journey.journey match {
        case Left(_)                 => ()
        case Right(fillingOutReturn) =>
          val newEmail            = newDetails.contactDetails.emailAddress
          val newAnswers          =
            updateAnswers(journey.answers, journey.contactDetails, newEmail)
          val newFillingOutReturn =
            updateFillingOutReturn(fillingOutReturn, newAnswers)

          mockStoreDraftReturn(newFillingOutReturn)(mockResult)
      }

  }

}
