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
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import shapeless.{Lens, lens}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSubscriptionReady, SubscriptionReadyAction, WithSubscriptionDetailsActions}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.DisplayFormat.Line
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Address, AddressLookupRequest, AddressLookupResult, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AddressLookupService
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
                                   addressLookupService: AddressLookupService,
                                   sessionStore: SessionStore,
                                   enterPostcodePage: views.html.subscription.enter_postcode,
                                   selectAddressPage: views.html.subscription.select_address,
                                   enterAddressPage: views.html.subscription.enter_address,
                                   addressDisplay: views.html.components.address_display
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with Logging
    with WithSubscriptionDetailsActions
    with SessionUpdates {

  def enterAddress(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
    lazy val showEnterAddressPage =
      request.subscriptionReady.subscriptionDetails.address match {
        case u: UkAddress =>
          Ok(enterAddressPage(Address.ukAddressForm.fill(u), routes.SubscriptionController.checkYourDetails()))
        case _: NonUkAddress =>
          logger.warn("Expected UK addresses but found non-UK address")
          errorHandler.errorResult()
      }
      if(request.sessionData.addressLookupResult.nonEmpty) {
        updateSession(sessionStore, request)(_.copy(addressLookupResult = None)).map {
          case Left(e) =>
            logger.warn(s"Could not clear addressLookupResult", e)
            errorHandler.errorResult()
          case Right(_) =>
            showEnterAddressPage
        }
      } else {
        showEnterAddressPage
      }

    }

  def enterAddressSubmit(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
      Address.ukAddressForm
        .bindFromRequest()
        .fold[Future[Result]](
          formWithErrors => BadRequest(enterAddressPage(formWithErrors, routes.SubscriptionController.checkYourDetails())),
          storeAddress
        )
    }

  def enterPostcode(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady { implicit request =>
      val form = request.sessionData.addressLookupResult
        .fold(AddressLookupRequest.form)(r => AddressLookupRequest.form.fill(AddressLookupRequest(r.postcode, r.filter)))
      Ok(enterPostcodePage(form, routes.SubscriptionController.checkYourDetails()))
    }

  def enterPostcodeSubmit(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
      AddressLookupRequest.form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(enterPostcodePage(formWithErrors, routes.SubscriptionController.checkYourDetails())),
          { case AddressLookupRequest(postcode, filter) =>
            // TODO: need to search session data with filter also
            if (request.sessionData.addressLookupResult.map(_.postcode).contains(postcode)
              && request.sessionData.addressLookupResult.map(_.filter).contains(filter)
            ) {
              SeeOther(routes.AddressController.selectAddress().url)
            } else {
              val result = for {
                addressLookupResult <- addressLookupService.lookupAddress(postcode, filter)
                _ <- EitherT(
                      updateSession(sessionStore, request)(_.copy(addressLookupResult = Some(addressLookupResult)))
                    )
              } yield addressLookupResult

              result.fold({ e =>
                logger.warn(s"Could not do address lookup for postcode", e)
                errorHandler.errorResult()
              }, _ => SeeOther(routes.AddressController.selectAddress().url))
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

  val addressLens: Lens[SubscriptionReady, Address] =
    lens[SubscriptionReady].subscriptionDetails.address

  private def storeAddress(address: Address)(implicit request: RequestWithSubscriptionReady[_]): Future[Result] =
    updateSession(sessionStore, request)(
      _.copy(subscriptionStatus = Some(addressLens.set(request.subscriptionReady)(address)))
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
