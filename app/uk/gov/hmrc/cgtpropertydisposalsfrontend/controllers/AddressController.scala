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

import cats.data.EitherT
import cats.instances.future._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.mvc._
import shapeless.{Lens, lens}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSubscriptionReady, SubscriptionReadyAction, WithSubscriptionDetailsActions}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, AddressLookupRequest, AddressLookupResult}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AddressController @Inject()(
  val authenticatedAction: AuthenticatedAction,
  val subscriptionDetailsAction: SubscriptionReadyAction,
  cc: MessagesControllerComponents,
  errorHandler: ErrorHandler,
  ukAddressLookupService: UKAddressLookupService,
  sessionStore: SessionStore,
  enterPostcodePage: views.html.subscription.enter_postcode,
  selectAddressPage: views.html.subscription.select_address,
  addressDisplay: views.html.components.address_display,
  enterUkAddressPage: views.html.address.enter_uk_address,
  enterNonUkAddressPage: views.html.address.enter_nonUk_address,
  isUkPage: views.html.address.isUk
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with Logging
    with WithSubscriptionDetailsActions
    with SessionUpdates {

  def isUk: Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
      if(request.sessionData.addressLookupResult.nonEmpty) {
        updateSession(sessionStore, request)(_.copy(addressLookupResult = None)).map {
          case Left(e) =>
            logger.warn(s"Could not clear addressLookupResult", e)
            errorHandler.errorResult()
          case Right(_) =>
            Ok(isUkPage(Address.isUkForm, routes.SubscriptionController.checkYourDetails()))
        }
      } else {
        Ok(isUkPage(Address.isUkForm, routes.SubscriptionController.checkYourDetails()))
      }
    }

  def isUkSubmit: Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
      Address.isUkForm
        .bindFromRequest()
        .fold[Future[Result]](
          formWithErrors => BadRequest(isUkPage(formWithErrors, routes.SubscriptionController.checkYourDetails())),
          {
            case true  => Redirect(routes.AddressController.enterPostcode())
            case false => Redirect(routes.AddressController.enterNonUkAddress())
          }
        )
    }

  def enterUkAddress(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
      Ok(enterUkAddressPage(Address.ukAddressForm, routes.SubscriptionController.checkYourDetails()))
    }

  def enterUkAddressSubmit(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
      Address.ukAddressForm
        .bindFromRequest()
        .fold[Future[Result]](
          formWithErrors => BadRequest(enterUkAddressPage(formWithErrors, routes.SubscriptionController.checkYourDetails())),
          storeAddress
        )
    }

  def enterNonUkAddress(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
      Ok(enterNonUkAddressPage(Address.nonUkAddressForm, routes.AddressController.isUk()))
    }

  def enterNonUkAddressSubmit(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
      Address.nonUkAddressForm
        .bindFromRequest()
        .fold[Future[Result]](
          formWithErrors => BadRequest(enterNonUkAddressPage(formWithErrors, routes.AddressController.isUk())),
          storeAddress
        )
    }

  def enterPostcode(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady { implicit request =>
      val form = request.sessionData.addressLookupResult
        .fold(AddressLookupRequest.form)(r => AddressLookupRequest.form.fill(AddressLookupRequest(r.postcode, r.filter)))
      Ok(enterPostcodePage(form, routes.AddressController.isUk()))
    }

  def enterPostcodeSubmit(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
      AddressLookupRequest.form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(enterPostcodePage(formWithErrors, routes.AddressController.isUk())),
          { case AddressLookupRequest(postcode, filter) =>

            def handleEmptyAddresses(r: AddressLookupResult) = {
              val errorKey = r.filter.fold("postcode")(_ => "filter")
              BadRequest(enterPostcodePage(
                AddressLookupRequest.form.bindFromRequest().withError(errorKey, "error.noResults"),
                routes.AddressController.isUk()
              ))
            }

            request.sessionData.addressLookupResult match {
              case Some(a: AddressLookupResult) if a.postcode.value === postcode.value && a.filter === filter =>
                if (a.addresses.isEmpty) handleEmptyAddresses(a) else SeeOther(routes.AddressController.selectAddress().url)
              case _ =>
                val result = for {
                  addressLookupResult <- ukAddressLookupService.lookupAddress(postcode, filter)
                  _ <- EitherT(
                    updateSession(sessionStore, request)(_.copy(addressLookupResult = Some(addressLookupResult)))
                  )
                } yield addressLookupResult

                result.fold({ e =>
                  logger.warn(s"Could not do address lookup for postcode", e)
                  errorHandler.errorResult()
                }, r =>
                  if (r.addresses.isEmpty) {
                      handleEmptyAddresses(r)
                  } else Redirect(routes.AddressController.selectAddress())
                )
              }

          }
        )
    }

  def selectAddress(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady { implicit request =>
      request.sessionData.addressLookupResult match {
        case None =>
          SeeOther(routes.SubscriptionController.checkYourDetails().url)

        case Some(AddressLookupResult(_, _, addresses)) =>
          val form = Address.addressSelectForm(addresses)
          Ok(selectAddressPage(addresses, form, routes.AddressController.enterPostcode()))
      }
    }

  def selectAddressSubmit(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
      request.sessionData.addressLookupResult match {
        case None =>
          SeeOther(routes.SubscriptionController.checkYourDetails().url)

        case Some(AddressLookupResult(_, _, addresses)) =>
          Address
            .addressSelectForm(addresses)
            .bindFromRequest()
            .fold(
              e => BadRequest(selectAddressPage(addresses, e, routes.AddressController.enterPostcode())),
              storeAddress
            )
      }
    }

  // this is coupled with subscription journey and session store
  val addressLens: Lens[SubscriptionReady, Address] =
    lens[SubscriptionReady].subscriptionDetails.address

  private def storeAddress(address: Address)(implicit request: RequestWithSubscriptionReady[_]): Future[Result] =
    updateSession(sessionStore, request)(
      _.copy(journeyStatus = Some(addressLens.set(request.subscriptionReady)(address)))
    ).map(
      _.fold(
        { e =>
          logger.warn("Could not store selected address in session", e)
          errorHandler.errorResult()
        },
        _ => SeeOther(routes.SubscriptionController.checkYourDetails().url)
      )
    )
}
