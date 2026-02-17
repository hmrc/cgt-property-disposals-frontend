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

import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.i18n.MessagesApi
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.AddressControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.RedirectToStartBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.ReturnsServiceSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.ChangingRepresenteeContactAddressJourney

import scala.concurrent.Future

trait ChangeRepresenteeContactAddressControllerSpec
    extends AddressControllerSpec[ChangingRepresenteeContactAddressJourney]
    with SampledScalaCheck
    with RedirectToStartBehaviour
    with ReturnsServiceSupport {

  val validJourneyStatus: ChangingRepresenteeContactAddressJourney

  override def overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[ReturnsService].toInstance(mockReturnsService)
    ) ::: super.overrideBindings

  protected override val controller: ChangeRepresenteeContactAddressController =
    instanceOf[ChangeRepresenteeContactAddressController]

  lazy implicit val messagesApi: MessagesApi = controller.messagesApi

  protected def updateAnswers(
    answers: RepresenteeAnswers,
    contactDetails: RepresenteeContactDetails,
    newAddress: Address
  ): IncompleteRepresenteeAnswers = {
    val newContactDetails = contactDetails.copy(address = newAddress)
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
    answers: RepresenteeAnswers
  ): FillingOutReturn =
    fillingOutReturn.copy(draftReturn =
      fillingOutReturn.draftReturn.fold(
        _.copy(
          representeeAnswers = Some(answers)
        ),
        _.copy(
          representeeAnswers = Some(answers)
        ),
        _.copy(
          representeeAnswers = Some(answers)
        ),
        _.copy(
          representeeAnswers = Some(answers)
        ),
        _.copy(
          representeeAnswers = Some(answers)
        )
      )
    )

  override def updateAddress(
    journey: ChangingRepresenteeContactAddressJourney,
    address: Address
  ): JourneyStatus = {
    val newAnswers =
      updateAnswers(journey.answers, journey.contactDetails, address)

    journey.journey.fold(
      _.copy(representeeAnswers = Some(newAnswers)),
      updateFillingOutReturn(_, newAnswers)
    )
  }

  override val mockUpdateAddress: Option[
    (ChangingRepresenteeContactAddressJourney, Address, Either[Error, Unit]) => Unit
  ]

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

        case FillingOutReturn(_, _, _, i: DraftMultipleIndirectDisposalsReturn, _, _)
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

  "ChangeRepresenteeAddressController" when {

    "handling requests to display the is UK page" must {
      def performAction(): Future[Result] = controller.isUk()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like displayIsUkBehaviour(() => performAction())
    }

    "handling requests to submit the is UK page" must {
      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.isUkSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitIsUkBehaviour(
        performAction,
        routes.ChangeRepresenteeContactAddressController.enterPostcode(),
        routes.ChangeRepresenteeContactAddressController.enterNonUkAddress(),
        None
      )

    }

    "handling requests to display the enter UK address page" must {

      def performAction(): Future[Result] =
        controller.enterUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like displayEnterUkAddressPage(UserType.Individual, None, () => performAction())
      behave like displayEnterUkAddressPage(UserType.Agent, None, () => performAction())

    }

    "handling submitted addresses from enter UK address page" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterUkAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitEnterUkAddress(performAction, routes.RepresenteeController.checkYourAnswers())

    }

    "handling requests to display the enter non UK address page" must {

      def performAction(): Future[Result] =
        controller.enterNonUkAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like displayEnterNonUkPage(() => performAction())

    }

    "handling requests to submit the enter non UK address page" must {
      def performAction(formData: (String, String)*): Future[Result] =
        controller.enterNonUkAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction())

      behave like submitEnterNonUkAddress(performAction, routes.RepresenteeController.checkYourAnswers())
    }

    "handling requests to display the enter postcode page" must {

      def performAction(): Future[Result] =
        controller.enterPostcode()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like enterPostcodePage(UserType.Individual, None, () => performAction())

    }

    "handling submitted postcodes and filters" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.enterPostcodeSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitEnterPostcode(performAction, routes.ChangeRepresenteeContactAddressController.selectAddress())

    }

    "handling requests to display the select address page" must {

      def performAction(): Future[Result] =
        controller.selectAddress()(FakeRequest())

      behave like redirectToStartBehaviour(() => performAction())

      behave like displaySelectAddress(
        UserType.Individual,
        None,
        () => performAction(),
        routes.RepresenteeController.checkYourAnswers()
      )

    }

    "handling submitted selected addresses" must {

      def performAction(formData: Seq[(String, String)]): Future[Result] =
        controller.selectAddressSubmit()(
          FakeRequest().withFormUrlEncodedBody(formData*).withCSRFToken.withMethod("POST")
        )

      behave like redirectToStartBehaviour(() => performAction(Seq.empty))

      behave like submitSelectAddress(
        performAction,
        routes.RepresenteeController.checkYourAnswers(),
        routes.RepresenteeController.checkYourAnswers()
      )

    }
  }

}

class StartingNewDraftReturnChangeRepresenteeAddressSpec extends ChangeRepresenteeContactAddressControllerSpec {

  override val validJourneyStatus: ChangingRepresenteeContactAddressJourney = {
    val answers                = sample[CompleteRepresenteeAnswers]
    val startingNewDraftReturn =
      sample[StartingNewDraftReturn].copy(representeeAnswers = Some(answers))
    ChangingRepresenteeContactAddressJourney(
      Left(startingNewDraftReturn),
      answers,
      answers.contactDetails
    )
  }

  override val mockUpdateAddress: Option[
    (
      ChangingRepresenteeContactAddressJourney,
      Address,
      Either[Error, Unit]
    ) => Unit
  ] = None

}

class FillingOutReturnChangeRepresenteeAddressSpec extends ChangeRepresenteeContactAddressControllerSpec {

  override val validJourneyStatus: ChangingRepresenteeContactAddressJourney = {
    val contactDetails   = sample[RepresenteeContactDetails]
    val answers          = sample[IncompleteRepresenteeAnswers]
      .copy(contactDetails = Some(contactDetails))
    val fillingOutReturn = sample[FillingOutReturn].copy(
      draftReturn = sample[DraftSingleDisposalReturn].copy(
        representeeAnswers = Some(answers)
      )
    )
    ChangingRepresenteeContactAddressJourney(
      Right(fillingOutReturn),
      answers,
      contactDetails
    )
  }

  override val mockUpdateAddress: Option[
    (
      ChangingRepresenteeContactAddressJourney,
      Address,
      Either[Error, Unit]
    ) => Unit
  ] = Some {
    case (
          journey: ChangingRepresenteeContactAddressJourney,
          newAddress: Address,
          mockResult: Either[Error, Unit]
        ) =>
      journey.journey match {
        case Left(_)                 => ()
        case Right(fillingOutReturn) =>
          val newAnswers          =
            updateAnswers(journey.answers, journey.contactDetails, newAddress)
          val newFillingOutReturn =
            updateFillingOutReturn(fillingOutReturn, newAnswers)

          mockStoreDraftReturn(newFillingOutReturn)(mockResult)
      }

  }

}
