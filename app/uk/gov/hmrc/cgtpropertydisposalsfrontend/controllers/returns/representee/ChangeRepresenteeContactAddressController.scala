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

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import com.google.inject.Inject
import play.api.mvc.{Call, MessagesControllerComponents, Request, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.IncompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{RepresenteeAnswers, RepresenteeContactDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.{AuditService, UKAddressLookupService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.ChangingRepresenteeContactAddressJourney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class ChangeRepresenteeContactAddressController @Inject() (
  val errorHandler: ErrorHandler,
  val ukAddressLookupService: UKAddressLookupService,
  val sessionStore: SessionStore,
  val auditService: AuditService,
  returnsService: ReturnsService,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  cc: MessagesControllerComponents,
  val enterPostcodePage: views.html.address.enter_postcode,
  val selectAddressPage: views.html.address.select_address,
  val enterUkAddressPage: views.html.address.enter_uk_address,
  val enterNonUkAddressPage: views.html.address.enter_nonUk_address,
  val isUkPage: views.html.address.isUk,
  val exitPage: views.html.address.exit_page
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with Logging
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with AddressController[ChangingRepresenteeContactAddressJourney] {

  override val toJourneyStatus: ChangingRepresenteeContactAddressJourney => JourneyStatus =
    _.journey.merge

  def isATrust(journey: ChangingRepresenteeContactAddressJourney): Boolean =
    journey.journey.fold(
      _.subscribedDetails.isATrust,
      _.subscribedDetails.isATrust
    )

  private def extractAnswersAndContactDetails(
    answers: Option[RepresenteeAnswers]
  ): Option[(RepresenteeAnswers, RepresenteeContactDetails)] =
    answers.flatMap(r =>
      r.fold(
        incomplete => incomplete.contactDetails.map(r -> _),
        complete => Some(complete -> complete.contactDetails)
      )
    )

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Future[Result], (SessionData, ChangingRepresenteeContactAddressJourney)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, s: StartingNewDraftReturn)) =>
        Either.fromOption(
          extractAnswersAndContactDetails(s.representeeAnswers)
            .map { case (answers, contactDetails) =>
              sessionData -> ChangingRepresenteeContactAddressJourney(
                Left(s),
                answers,
                contactDetails
              )
            },
          Redirect(controllers.routes.StartController.start())
        )

      case Some((sessionData, f: FillingOutReturn)) =>
        Either.fromOption(
          extractAnswersAndContactDetails(
            f.draftReturn.representeeAnswers
          ).map { case (answers, contactDetails) =>
            sessionData -> ChangingRepresenteeContactAddressJourney(
              Right(f),
              answers,
              contactDetails
            )
          },
          Redirect(controllers.routes.StartController.start())
        )

      case _ => Left(Redirect(controllers.routes.StartController.start()))
    }

  def updateAddress(
    journey: ChangingRepresenteeContactAddressJourney,
    address: Address,
    isManuallyEnteredAddress: Boolean
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, JourneyStatus] = {
    val newContactDetails = journey.contactDetails.copy(address = address)
    val newAnswers        = journey.answers.fold(
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

    val newJourney = journey.journey.bimap(
      _.copy(representeeAnswers = Some(newAnswers)),
      fillingOutReturn =>
        fillingOutReturn.copy(draftReturn =
          fillingOutReturn.draftReturn.fold(
            _.copy(representeeAnswers = Some(newAnswers)),
            _.copy(representeeAnswers = Some(newAnswers)),
            _.copy(representeeAnswers = Some(newAnswers)),
            _.copy(representeeAnswers = Some(newAnswers)),
            _.copy(representeeAnswers = Some(newAnswers))
          )
        )
    )

    newJourney.fold(
      EitherT.pure[Future, Error](_),
      fillingOutReturn =>
        returnsService
          .storeDraftReturn(fillingOutReturn)
          .map(_ => fillingOutReturn)
    )
  }

  protected lazy val backLinkCall: ChangingRepresenteeContactAddressJourney => Call =
    _ => routes.RepresenteeController.checkYourAnswers()

  protected lazy val isUkCall: Call                                =
    routes.ChangeRepresenteeContactAddressController.isUk()
  protected lazy val isUkSubmitCall: Call                          =
    routes.ChangeRepresenteeContactAddressController.isUkSubmit()
  protected lazy val enterUkAddressCall: Call                      =
    routes.ChangeRepresenteeContactAddressController.enterUkAddress()
  protected lazy val enterUkAddressSubmitCall: Call                =
    routes.ChangeRepresenteeContactAddressController.enterUkAddressSubmit()
  protected lazy val enterNonUkAddressCall: Call                   =
    routes.ChangeRepresenteeContactAddressController.enterNonUkAddress()
  protected lazy val enterNonUkAddressSubmitCall: Call             =
    routes.ChangeRepresenteeContactAddressController.enterNonUkAddressSubmit()
  protected lazy val enterPostcodeCall: Call                       =
    routes.ChangeRepresenteeContactAddressController.enterPostcode()
  protected lazy val enterPostcodeSubmitCall: Call                 =
    routes.ChangeRepresenteeContactAddressController.enterPostcodeSubmit()
  protected lazy val selectAddressCall: Call                       =
    routes.ChangeRepresenteeContactAddressController.selectAddress()
  protected lazy val selectAddressSubmitCall: Call                 =
    routes.ChangeRepresenteeContactAddressController.selectAddressSubmit()
  protected lazy val ukAddressNotAllowedExitPageCall: Option[Call] =
    None
  protected lazy val continueCall: Call                            =
    routes.RepresenteeController.checkYourAnswers()
}
