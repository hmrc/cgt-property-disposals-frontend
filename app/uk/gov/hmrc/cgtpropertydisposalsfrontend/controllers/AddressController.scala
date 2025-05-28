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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import cats.data.EitherT
import cats.instances.future._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.eq._
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{RequestWithSessionData, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresentativeType
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, given}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.{EnteringCompanyDetails, EnteringSingleMixedUsePropertyDetails, FillingOutReturnAddressJourney}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.{ManagingSubscription, Onboarding}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

trait AddressController[A <: AddressJourneyType] {
  this: FrontendController & Logging & WithAuthAndSessionDataAction & SessionUpdates =>

  val errorHandler: ErrorHandler
  val ukAddressLookupService: UKAddressLookupService
  val sessionStore: SessionStore
  val enterPostcodePage: views.html.address.enter_postcode
  val selectAddressPage: views.html.address.select_address
  val enterUkAddressPage: views.html.address.enter_uk_address
  val enterNonUkAddressPage: views.html.address.enter_nonUk_address
  val isUkPage: views.html.address.isUk
  val exitPage: views.html.address.exit_page
  val toJourneyStatus: A => JourneyStatus
  implicit val viewConfig: ViewConfig
  implicit val ec: ExecutionContext

  def validJourney(
    request: RequestWithSessionData[?]
  ): Either[Future[Result], (SessionData, A)]

  def isATrust(journey: A): Boolean

  def updateAddress(
    journey: A,
    address: Address,
    isManuallyEnteredAddress: Boolean
  )(implicit
    hc: HeaderCarrier,
    request: Request[?]
  ): EitherT[Future, Error, JourneyStatus]

  protected def backLinkCall: A => Call

  protected lazy val isUkCall: Call
  protected lazy val isUkSubmitCall: Call
  protected lazy val enterUkAddressCall: Call
  protected lazy val enterUkAddressSubmitCall: Call
  protected lazy val enterNonUkAddressCall: Call
  protected lazy val enterNonUkAddressSubmitCall: Call
  protected lazy val enterPostcodeCall: Call
  protected lazy val enterPostcodeSubmitCall: Call
  protected lazy val selectAddressCall: Call
  protected lazy val selectAddressSubmitCall: Call
  protected lazy val continueCall: Call
  protected lazy val ukAddressNotAllowedExitPageCall: Option[Call]

  protected val enterUkAddressBackLinkCall: A => Call = backLinkCall
  protected val enterPostcodePageBackLink: A => Call  = _ => isUkCall

  protected def withValidJourney(request: RequestWithSessionData[?])(
    f: (SessionData, A) => Future[Result]
  ): Future[Result] =
    validJourney(request).map[Future[Result]](f.tupled).merge

  def showExitPage(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      Ok(exitPage(isUkCall))
    }

  def isUk: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { case (sessionData, journey) =>
        val form = request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
          case Some((_, subscribed: Subscribed)) =>
            subscribed.subscribedDetails.address match {
              case _: UkAddress => Address.isUkForm.fill(true)
              case _            => Address.isUkForm.fill(false)
            }
          case _                                 => Address.isUkForm
        }
        if (sessionData.addressLookupResult.nonEmpty) {
          updateSession(sessionStore, request.toSession)(
            _.copy(addressLookupResult = None)
          ).map {
            case Left(e)  =>
              logger.warn(s"Could not clear addressLookupResult", e)
              errorHandler.errorResult()
            case Right(_) =>
              Ok(
                isUkPage(
                  form,
                  backLinkCall(journey),
                  isUkSubmitCall,
                  journey
                )
              )
          }
        } else {
          Ok(
            isUkPage(
              form,
              backLinkCall(journey),
              isUkSubmitCall,
              journey
            )
          )
        }
      }
    }

  def isUkSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { case (_, journey) =>
        Address.isUkForm
          .bindFromRequest()
          .fold[Future[Result]](
            formWithErrors =>
              BadRequest(
                isUkPage(
                  formWithErrors,
                  backLinkCall(journey),
                  isUkSubmitCall,
                  journey
                )
              ),
            {
              case true  =>
                if (registeredWithId(journey)) {
                  Redirect(enterPostcodeCall)
                } else {
                  Redirect(ukAddressNotAllowedExitPageCall.getOrElse(enterPostcodeCall))
                }
              case false => Redirect(enterNonUkAddressCall)
            }
          )
      }
    }

  def enterUkAddress(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      val form = request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
        case Some((_, subscribed: Subscribed)) =>
          subscribed.subscribedDetails.address match {
            case a: UkAddress => Address.ukAddressForm.fill(UkAddress(a.line1, a.line2, a.town, a.county, a.postcode))
            case _            => Address.ukAddressForm
          }
        case _                                 => Address.ukAddressForm
      }
      withValidJourney(request) { case (_, journey) =>
        Ok(
          enterUkAddressPage(
            form,
            enterUkAddressBackLinkCall(journey),
            enterUkAddressSubmitCall,
            enterPostcodeCall,
            journey,
            isATrust(journey),
            extractRepresentativeType(journey),
            extractIsAmend(journey)
          )
        )
      }
    }

  def enterUkAddressSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { case (_, journey) =>
        Address.ukAddressForm
          .bindFromRequest()
          .fold[Future[Result]](
            formWithErrors =>
              BadRequest(
                enterUkAddressPage(
                  formWithErrors,
                  enterUkAddressBackLinkCall(journey),
                  enterUkAddressSubmitCall,
                  enterPostcodeCall,
                  journey,
                  isATrust(journey),
                  extractRepresentativeType(journey),
                  extractIsAmend(journey)
                )
              ),
            storeAddress(continueCall, journey, isManuallyEnteredAddress = true)
          )
      }
    }

  def enterNonUkAddress(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      val form = request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
        case Some((_, subscribed: Subscribed)) =>
          subscribed.subscribedDetails.address match {
            case a: NonUkAddress =>
              Address.nonUkAddressForm.fill(NonUkAddress(a.line1, a.line2, a.line3, a.line4, a.postcode, a.country))
            case _               => Address.nonUkAddressForm
          }
        case _                                 => Address.nonUkAddressForm
      }
      withValidJourney(request) { case (_, journey) =>
        Ok(
          enterNonUkAddressPage(
            form,
            isUkCall,
            enterNonUkAddressSubmitCall,
            journey,
            extractIsAmend(journey)
          )
        )
      }
    }

  def enterNonUkAddressSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { case (_, journey) =>
        Address.nonUkAddressForm
          .bindFromRequest()
          .fold[Future[Result]](
            formWithErrors =>
              BadRequest(
                enterNonUkAddressPage(
                  formWithErrors,
                  isUkCall,
                  enterNonUkAddressSubmitCall,
                  journey,
                  extractIsAmend(journey)
                )
              ),
            storeAddress(continueCall, journey, isManuallyEnteredAddress = true)
          )
      }
    }

  def enterPostcode(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { case (sessionData, journey) =>
        val form = sessionData.addressLookupResult
          .fold(AddressLookupRequest.form)(r =>
            AddressLookupRequest.form.fill(
              AddressLookupRequest(r.postcode, r.filter)
            )
          )
        Ok(
          enterPostcodePage(
            form,
            enterPostcodePageBackLink(journey),
            enterPostcodeSubmitCall,
            enterUkAddressCall,
            journey,
            isATrust(journey),
            extractRepresentativeType(journey)
          )
        )
      }
    }

  def enterPostcodeSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { case (sessionData, journey) =>
        AddressLookupRequest.form
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                enterPostcodePage(
                  formWithErrors,
                  enterPostcodePageBackLink(journey),
                  enterPostcodeSubmitCall,
                  enterUkAddressCall,
                  journey,
                  isATrust(journey),
                  extractRepresentativeType(journey)
                )
              ),
            { case AddressLookupRequest(postcode, filter) =>
              def handleEmptyAddresses(r: AddressLookupResult) = {
                val errorKey = r.filter.fold("postcode")(_ => "filter")
                BadRequest(
                  enterPostcodePage(
                    AddressLookupRequest.form
                      .bindFromRequest()
                      .withError(errorKey, "error.noResults"),
                    isUkCall,
                    enterPostcodeSubmitCall,
                    enterUkAddressCall,
                    journey,
                    isATrust(journey),
                    extractRepresentativeType(journey)
                  )
                )
              }

              sessionData.addressLookupResult match {
                case Some(a: AddressLookupResult) if a.postcode.value === postcode.value && a.filter === filter =>
                  if (a.addresses.isEmpty) handleEmptyAddresses(a) else Redirect(selectAddressCall)

                case _ =>
                  val result = for {
                    addressLookupResult <- ukAddressLookupService.lookupAddress(postcode, filter)
                    _                   <- EitherT(
                                             updateSession(sessionStore, request.toSession)(
                                               _.copy(addressLookupResult = Some(addressLookupResult))
                                             )
                                           )
                  } yield addressLookupResult

                  result.fold(
                    { e =>
                      logger.warn(s"Could not do address lookup for postcode", e)
                      errorHandler.errorResult()
                    },
                    r =>
                      if (r.addresses.isEmpty) {
                        handleEmptyAddresses(r)
                      } else {
                        Redirect(selectAddressCall)
                      }
                  )
              }
            }
          )
      }
    }

  def selectAddress(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { case (sessionData, journey) =>
        sessionData.addressLookupResult match {
          case None =>
            Redirect(backLinkCall(journey))

          case Some(AddressLookupResult(_, _, addresses)) =>
            val form = Address.addressSelectForm(addresses)
            Ok(
              selectAddressPage(
                addresses,
                form,
                enterPostcodeCall,
                selectAddressSubmitCall,
                enterUkAddressCall,
                journey,
                isATrust(journey),
                extractRepresentativeType(journey),
                extractIsAmend(journey)
              )
            )
        }
      }
    }

  def selectAddressSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { case (sessionData, journey) =>
        sessionData.addressLookupResult match {
          case None =>
            Redirect(backLinkCall(journey))

          case Some(AddressLookupResult(_, _, addresses)) =>
            Address
              .addressSelectForm(addresses)
              .bindFromRequest()
              .fold(
                e =>
                  BadRequest(
                    selectAddressPage(
                      addresses,
                      e,
                      enterPostcodeCall,
                      selectAddressSubmitCall,
                      enterUkAddressCall,
                      journey,
                      isATrust(journey),
                      extractRepresentativeType(journey),
                      extractIsAmend(journey)
                    )
                  ),
                storeAddress(continueCall, journey, isManuallyEnteredAddress = false)
              )
        }
      }
    }

  protected def storeAddress(
    continue: Call,
    currentJourneyStatus: A,
    isManuallyEnteredAddress: Boolean
  )(
    address: Address
  )(implicit request: RequestWithSessionData[?]): Future[Result] = {
    val result = for {
      journeyWithUpdatedAddress <- updateAddress(currentJourneyStatus, address, isManuallyEnteredAddress)
      _                         <- if (journeyWithUpdatedAddress === toJourneyStatus(currentJourneyStatus)) {
                                     EitherT.pure[Future, Error](())
                                   } else {
                                     EitherT[Future, Error, Unit](
                                       updateSession(sessionStore, request.toSession)(
                                         _.copy(journeyStatus = Some(journeyWithUpdatedAddress))
                                       )
                                     )
                                   }
    } yield ()

    result.fold(
      { e =>
        logger.warn("Could not update address", e)
        errorHandler.errorResult()
      },
      _ => Redirect(continue)
    )

  }

  private def extractIsAmend(
    journey: AddressJourneyType
  ): Boolean =
    journey match {
      case j: FillingOutReturnAddressJourney => j.journey.isAmendReturn

      case c: EnteringCompanyDetails => c.journey.isAmendReturn

      case m: EnteringSingleMixedUsePropertyDetails => m.journey.isAmendReturn

      case _ => false
    }

  private def extractRepresentativeType(
    journey: AddressJourneyType
  ): Option[RepresentativeType] =
    journey match {
      case j: FillingOutReturnAddressJourney =>
        j.draftReturn.fold(_.triageAnswers.representativeType(), _.triageAnswers.representativeType())

      case c: EnteringCompanyDetails =>
        c.representativeType

      case m: EnteringSingleMixedUsePropertyDetails =>
        m.representativeType

      case _ => None
    }

  private def registeredWithId(journey: AddressJourneyType): Boolean =
    journey match {
      case onboarding: AddressJourneyType.Onboarding             =>
        onboarding match {
          case Onboarding.RegistrationReadyAddressJourney(_)              => false
          case Onboarding.IndividualSupplyingInformationAddressJourney(_) => false
          case Onboarding.SubscriptionReadyAddressJourney(_)              => true
          case Onboarding.SubscriptionEnterAddressJourney(_)              => true
        }
      case subscription: AddressJourneyType.ManagingSubscription =>
        subscription match {
          case ManagingSubscription.SubscribedAddressJourney(subscribed) =>
            subscribed.subscribedDetails.registeredWithId
        }
      case _: AddressJourneyType.Returns                         => true
    }
}
