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

import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

final case class SubscriptionUpdateDetails(
  name: Either[TrustName, IndividualName],
  emailAddress: Email,
  address: Address,
  contactName: ContactName,
  telephone: Option[String],
  cgtReference: String,
  registerWithId: Boolean
)

@Singleton
class TmpAccountController @Inject()(
  cc: MessagesControllerComponents,
  manageYourDetailsPage: views.html.account.manage_your_details
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
  extends FrontendController(cc) {

  def manageYourDetails(): Action[AnyContent] =
    Action { implicit request =>
      Ok(manageYourDetailsPage(SubscriptionUpdateDetails(
        Right(IndividualName("Ali", "Macdonald")),
        Email("alistair.macdonald@digital.hmrc.gov.uk"),
        Address.UkAddress("8 Oxford Road", None, Some("Worthing"), Some("West Sussex"), "BN11 1XG"),
        ContactName("Ali Macdonald"),
        telephone = None,
        cgtReference = "XYCGTP123456789",
        registerWithId = true
      )))
    }
}
