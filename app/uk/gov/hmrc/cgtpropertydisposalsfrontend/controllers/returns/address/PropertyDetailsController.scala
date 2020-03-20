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

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => addressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsExamplePropertyDetailsAnswers.{CompleteMultipleDisposalsExamplePropertyDetailsAnswers, IncompleteMultipleDisposalsExamplePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{MultipleDisposalsDraftReturn, SingleDisposalDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.FillingOutReturnAddressJourney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PropertyDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val ukAddressLookupService: UKAddressLookupService,
  returnsService: ReturnsService,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val enterPostcodePage: views.html.address.enter_postcode,
  val selectAddressPage: views.html.address.select_address,
  val addressDisplay: views.html.components.address_display,
  val enterUkAddressPage: views.html.address.enter_uk_address,
  val enterNonUkAddressPage: views.html.address.enter_nonUk_address,
  val isUkPage: views.html.address.isUk,
  multipleDisposalsGuidancePage: views.html.returns.address.multiple_disposals_guidance,
  singleDisposalCheckYourAnswersPage: views.html.returns.address.single_disposal_check_your_answers,
  multipleDisposalsCheckYourAnswersPage: views.html.returns.address.multiple_disposals_check_your_answers
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with AddressController[FillingOutReturn] {

  override val toAddressJourneyType: FillingOutReturn => FillingOutReturnAddressJourney =
    FillingOutReturnAddressJourney.apply

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, FillingOutReturn)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, r: FillingOutReturn)) => Right(sessionData -> r)
      case _                                        => Left(Redirect(controllers.routes.StartController.start()))
    }

  def updateAddress(journey: FillingOutReturn, address: Address, isManuallyEnteredAddress: Boolean)(
    implicit hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, FillingOutReturn] =
    address match {
      case _: NonUkAddress =>
        EitherT.leftT[Future, FillingOutReturn](Error("Got non uk address in returns journey but expected uk address"))
      case a: UkAddress =>
        journey.draftReturn match {
          case m: MultipleDisposalsDraftReturn =>
            if (m.examplePropertyDetailsAnswers.flatMap(_.fold(_.address, c => Some(c.address))).contains(a))
              EitherT.pure(journey)
            else {
              val updatedDraftReturn = m.copy(
                examplePropertyDetailsAnswers = Some(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(Some(a)))
              )
              returnsService
                .storeDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )
                .map(_ => journey.copy(draftReturn = updatedDraftReturn))
            }

          case d: SingleDisposalDraftReturn =>
            if (d.propertyAddress.contains(a))
              EitherT.pure(journey)
            else {
              val updatedDraftReturn = d.copy(propertyAddress = Some(a))
              returnsService
                .storeDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )
                .map(_ => journey.copy(draftReturn = updatedDraftReturn))
            }
        }

    }

  protected lazy val enterUkAddressCall: Call       = addressRoutes.PropertyDetailsController.enterUkAddress()
  protected lazy val enterUkAddressSubmitCall: Call = addressRoutes.PropertyDetailsController.enterUkAddressSubmit()

  protected lazy val enterPostcodeCall: Call       = addressRoutes.PropertyDetailsController.enterPostcode()
  protected lazy val enterPostcodeSubmitCall: Call = addressRoutes.PropertyDetailsController.enterPostcodeSubmit()
  protected lazy val selectAddressCall: Call       = addressRoutes.PropertyDetailsController.selectAddress()
  protected lazy val selectAddressSubmitCall: Call = addressRoutes.PropertyDetailsController.selectAddressSubmit()
  protected lazy val continueCall: Call            = addressRoutes.PropertyDetailsController.checkYourAnswers()

  override protected val enterPostcodePageBackLink: FillingOutReturn => Call =
    _.draftReturn.fold(
      _.examplePropertyDetailsAnswers.fold(
        routes.PropertyDetailsController.multipleDisposalsGuidance()
      )(
        _.fold(
          _ => routes.PropertyDetailsController.multipleDisposalsGuidance(),
          _ => routes.PropertyDetailsController.checkYourAnswers()
        )
      ),
      _.propertyAddress.fold(
        returnsRoutes.TaskListController.taskList()
      )(_ => addressRoutes.PropertyDetailsController.checkYourAnswers())
    )

  def multipleDisposalsGuidance(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, r) =>
        r.draftReturn match {
          case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case m: MultipleDisposalsDraftReturn =>
            val backLink =
              m.examplePropertyDetailsAnswers
                .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
                .fold(
                  _ => controllers.returns.routes.TaskListController.taskList(),
                  _ => routes.PropertyDetailsController.checkYourAnswers()
                )
            Ok(multipleDisposalsGuidancePage(backLink))
        }
    }
  }

  def multipleDisposalsGuidanceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withValidJourney(request) {
        case (_, r) =>
          r.draftReturn match {
            case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
            case m: MultipleDisposalsDraftReturn =>
              val redirectTo =
                m.examplePropertyDetailsAnswers
                  .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
                  .fold(
                    _ => routes.PropertyDetailsController.enterPostcode(),
                    _ => routes.PropertyDetailsController.checkYourAnswers()
                  )

              Redirect(redirectTo)
          }
      }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, r) =>
        r.draftReturn match {
          case m: MultipleDisposalsDraftReturn =>
            m.examplePropertyDetailsAnswers.fold[Future[Result]](
              Redirect(routes.PropertyDetailsController.multipleDisposalsGuidance())
            ) {
              case IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(None) =>
                Redirect(routes.PropertyDetailsController.multipleDisposalsGuidance())

              case IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(Some(a)) =>
                val completeAnswers    = CompleteMultipleDisposalsExamplePropertyDetailsAnswers(a)
                val updatedDraftReturn = m.copy(examplePropertyDetailsAnswers = Some(completeAnswers))
                val result = for {
                  _ <- returnsService.storeDraftReturn(
                        updatedDraftReturn,
                        r.subscribedDetails.cgtReference,
                        r.agentReferenceNumber
                      )
                  _ <- EitherT(
                        updateSession(sessionStore, request)(
                          _.copy(journeyStatus = Some(r.copy(draftReturn = updatedDraftReturn)))
                        )
                      )
                } yield ()

                result.fold(
                  { e =>
                    logger.warn("Could not update draft return", e)
                    errorHandler.errorResult()
                  },
                  _ => Ok(multipleDisposalsCheckYourAnswersPage(completeAnswers))
                )

              case c: CompleteMultipleDisposalsExamplePropertyDetailsAnswers =>
                Ok(multipleDisposalsCheckYourAnswersPage(c))

            }

          case s: SingleDisposalDraftReturn =>
            s.propertyAddress.fold(
              Redirect(routes.PropertyDetailsController.enterPostcode())
            )(address => Ok(singleDisposalCheckYourAnswersPage(address)))
        }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, _) =>
        Redirect(returnsRoutes.TaskListController.taskList())
    }
  }

  // the following aren't used for the returns journey - the returns journey only handles uk addresses
  protected lazy val isUkCall: Call                    = enterPostcodeCall
  protected lazy val isUkSubmitCall: Call              = enterPostcodeCall
  protected lazy val enterNonUkAddressCall: Call       = enterPostcodeCall
  protected lazy val enterNonUkAddressSubmitCall: Call = enterPostcodeCall
  protected lazy val backLinkCall: Call                = enterPostcodeCall

}
