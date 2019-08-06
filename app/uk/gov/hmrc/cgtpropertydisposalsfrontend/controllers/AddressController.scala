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

import com.google.inject.Inject
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

class AddressController @Inject() (
                                    cc: MessagesControllerComponents,
                                    enterPostcodePage: views.html.subscription.enter_postcode,
                                    selectAddressPage: views.html.subscription.select_address
                                  )(implicit viewConfig: ViewConfig) extends FrontendController(cc) with Logging {

  def enterPostcode(): Action[AnyContent] = Action { implicit request =>
    Ok(enterPostcodePage(Postcode.form))
  }

  def enterPostcodeSubmit(): Action[AnyContent] = Action { implicit request =>
    Postcode.form.bindFromRequest().fold(
      formWithErrors => BadRequest(enterPostcodePage(formWithErrors)),
      { postcode =>
        logger.info(s"Got postcode $postcode")
        SeeOther(routes.AddressController.selectAddress().url)
      }
    )
  }


  def selectAddress(): Action[AnyContent] = Action { implicit request =>
    def address(i: Int) =
      UkAddress(s"$i the street", Some("The Town"), Some("The County"), None, "postcode")

    val addresses = (0 to 100).map(address).toList
    Ok(selectAddressPage(addresses))
  }

}
