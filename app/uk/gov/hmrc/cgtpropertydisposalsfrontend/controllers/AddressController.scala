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
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, SessionDataAction, WithActions}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AddressLookupService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.toFuture
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class AddressController @Inject() (
    val authenticatedAction: AuthenticatedAction,
    val sessionDataAction: SessionDataAction,
    cc: MessagesControllerComponents,
    errorHandler: ErrorHandler,
    addressLookupService: AddressLookupService,
    sessionStore: SessionStore,
    enterPostcodePage: views.html.subscription.enter_postcode,
    selectAddressPage: views.html.subscription.select_address
)(implicit viewConfig: ViewConfig, ec: ExecutionContext) extends FrontendController(cc) with Logging with WithActions {

  def enterPostcode(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    (request.sessionData.flatMap(_.businessPartnerRecord), request.sessionData.flatMap(_.addressLookupResult)) match {
      case (None, _) =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)

      case (Some(_), addressLookupResult) =>
        val form = addressLookupResult.fold(Postcode.form)(r => Postcode.form.fill(r.postcode))
        Ok(enterPostcodePage(form))
    }
  }

  def enterPostcodeSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    (request.sessionData.flatMap(_.businessPartnerRecord), request.sessionData.flatMap(_.addressLookupResult)) match {
      case (None, _) =>
        SeeOther(routes.SubscriptionController.checkYourDetails().url)

      case (Some(_), addressLookupResult) =>
        Postcode.form.bindFromRequest().fold(
          formWithErrors => BadRequest(enterPostcodePage(formWithErrors)), {
            postcode =>
              if (addressLookupResult.map(_.postcode).contains(postcode)) {
                SeeOther(routes.AddressController.selectAddress().url)
              } else {
                val result = for {
                  addressLookupResult <- EitherT(addressLookupService.lookupAddress(postcode))
                  _ <- EitherT(updateSession(sessionStore)(_.copy(addressLookupResult = Some(addressLookupResult))))
                } yield addressLookupResult

                result.fold({
                  e =>
                    logger.warn("Could not do address lookup", e)
                    errorHandler.errorResult()
                }, _ =>
                  SeeOther(routes.AddressController.selectAddress().url))
              }
          }
        )
    }
  }

  def selectAddress(): Action[AnyContent] = Action { implicit request =>
      def address(i: Int) =
        UkAddress(s"$i the street", Some("The Town"), Some("The County"), None, "postcode")

    val addresses = (0 to 100).map(address).toList
    Ok(selectAddressPage(addresses))
  }

}
