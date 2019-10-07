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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.address

import cats.data.EitherT
import cats.instances.future._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.eq._
import play.api.mvc.{Action, AnyContent, Call, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{RequestWithSessionData, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

trait AddressController[J <: JourneyStatus] {
  this: FrontendController with Logging with WithAuthAndSessionDataAction with SessionUpdates =>

  val errorHandler: ErrorHandler
  val ukAddressLookupService: UKAddressLookupService
  val sessionStore: SessionStore
  val enterPostcodePage: views.html.address.enter_postcode
  val selectAddressPage: views.html.address.select_address
  val addressDisplay: views.html.components.address_display
  val enterUkAddressPage: views.html.address.enter_uk_address
  val enterNonUkAddressPage: views.html.address.enter_nonUk_address
  val isUkPage: views.html.address.isUk
  implicit val viewConfig: ViewConfig
  implicit val ec: ExecutionContext

  def validJourney(request: RequestWithSessionData[_]): Either[Result, (SessionData, J)]

  def updateAddress(journey: J, address: Address): JourneyStatus

  protected val backLinkCall: Call
  protected val isUkCall: Call
  protected val isUkSubmitCall: Call
  protected val enterUkAddressCall: Call
  protected val enterUkAddressSubmitCall: Call
  protected val enterNonUkAddressCall: Call
  protected val enterNonUkAddressSubmitCall: Call
  protected val enterPostcodeCall: Call
  protected val enterPostcodeSubmitCall: Call
  protected val selectAddressCall: Call
  protected val selectAddressSubmitCall: Call
  protected val continueCall: Call

  private def withValidJourney(request: RequestWithSessionData[_])(
    f: (SessionData, J) => Future[Result]
  ): Future[Result] =
    validJourney(request).fold[Future[Result]](toFuture, f.tupled)

  def isUk(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (sessionData, _) =>
          if (sessionData.addressLookupResult.nonEmpty) {
            updateSession(sessionStore, request)(_.copy(addressLookupResult = None)).map {
              case Left(e) =>
                logger.warn(s"Could not clear addressLookupResult", e)
                errorHandler.errorResult()
              case Right(_) =>
                Ok(isUkPage(Address.isUkForm, backLinkCall, isUkSubmitCall))
            }
          } else {
            Ok(isUkPage(Address.isUkForm, backLinkCall, isUkSubmitCall))
          }
      }
    }

  def isUkSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (_, _) =>
          Address.isUkForm
            .bindFromRequest()
            .fold[Future[Result]](
              formWithErrors => BadRequest(isUkPage(formWithErrors, backLinkCall, isUkSubmitCall)), {
                case true => Redirect(enterPostcodeCall)
                case false => Redirect(enterNonUkAddressCall)
              }
            )
      }
    }

  def enterUkAddress(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (_, _) =>
          Ok(enterUkAddressPage(Address.ukAddressForm, backLinkCall, enterUkAddressSubmitCall, enterPostcodeCall))
      }
    }

  def enterUkAddressSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (_, journeyStatus) =>
          Address.ukAddressForm
            .bindFromRequest()
            .fold[Future[Result]](
              formWithErrors =>
                BadRequest(
                  enterUkAddressPage(formWithErrors, backLinkCall, enterUkAddressSubmitCall, enterPostcodeCall)
                ),
              storeAddress(continueCall, journeyStatus)
            )
      }
    }

  def enterNonUkAddress(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (_, _) =>
          Ok(enterNonUkAddressPage(Address.nonUkAddressForm, isUkCall, enterNonUkAddressSubmitCall))
      }
    }

  def enterNonUkAddressSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (_, journeyStatus) =>
          Address.nonUkAddressForm
            .bindFromRequest()
            .fold[Future[Result]](
              formWithErrors => BadRequest(enterNonUkAddressPage(formWithErrors, isUkCall, enterNonUkAddressSubmitCall)),
              storeAddress(continueCall, journeyStatus)
            )
      }
    }

  def enterPostcode(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (sessionData, _) =>
          val form = sessionData.addressLookupResult
            .fold(AddressLookupRequest.form)(
              r => AddressLookupRequest.form.fill(AddressLookupRequest(r.postcode, r.filter))
            )
          Ok(enterPostcodePage(form, isUkCall, enterPostcodeSubmitCall, enterUkAddressCall))
      }
    }

  def enterPostcodeSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (sessionData, _) =>
          AddressLookupRequest.form
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  enterPostcodePage(formWithErrors, isUkCall, enterPostcodeSubmitCall, enterUkAddressCall)
                ), {
                case AddressLookupRequest(postcode, filter) =>
                  def handleEmptyAddresses(r: AddressLookupResult) = {
                    val errorKey = r.filter.fold("postcode")(_ => "filter")
                    BadRequest(
                      enterPostcodePage(
                        AddressLookupRequest.form.bindFromRequest().withError(errorKey, "error.noResults"),
                        isUkCall,
                        enterPostcodeSubmitCall,
                        enterUkAddressCall
                      )
                    )
                  }

                  sessionData.addressLookupResult match {
                    case Some(a: AddressLookupResult) if a.postcode.value === postcode.value && a.filter === filter =>
                      if (a.addresses.isEmpty) handleEmptyAddresses(a) else Redirect(selectAddressCall)

                    case _ =>
                      val result = for {
                        addressLookupResult <- ukAddressLookupService.lookupAddress(postcode, filter)
                        _ <- EitherT(
                          updateSession(sessionStore, request)(
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
                          } else Redirect(selectAddressCall)
                      )
                  }

              }
            )
      }
    }

  def selectAddress(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (sessionData, _) =>
          sessionData.addressLookupResult match {
            case None =>
              Redirect(backLinkCall)

            case Some(AddressLookupResult(_, _, addresses)) =>
              val form = Address.addressSelectForm(addresses)
              Ok(selectAddressPage(addresses, form, enterPostcodeCall, selectAddressSubmitCall, enterUkAddressCall))
          }
      }
    }

  def selectAddressSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (sessionData, journeyStatus) =>
          sessionData.addressLookupResult match {
            case None =>
              Redirect(backLinkCall)

            case Some(AddressLookupResult(_, _, addresses)) =>
              Address
                .addressSelectForm(addresses)
                .bindFromRequest()
                .fold(
                  e =>
                    BadRequest(
                      selectAddressPage(addresses, e, enterPostcodeCall, selectAddressSubmitCall, enterUkAddressCall)
                    ),
                  storeAddress(continueCall, journeyStatus)
                )
          }
      }
    }

  private def storeAddress(
    continue: Call,
    currentJourneyStatus: J
  )(address: Address)(implicit request: RequestWithSessionData[_]): Future[Result] =
    updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updateAddress( currentJourneyStatus, address))))
      .map(
        _.fold(
          { e =>
            logger.warn("Could not store selected address in session", e)
            errorHandler.errorResult()
          },
          _ => Redirect(continue)
        )
      )

}


