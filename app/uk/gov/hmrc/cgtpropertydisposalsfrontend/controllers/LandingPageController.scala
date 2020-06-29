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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

@Singleton
class LandingPageController @Inject() (
  cc: MessagesControllerComponents,
  val config: Configuration,
  landing_page: views.html.landing_page,
  agents_landing_page: views.html.agents_landing_page
)(implicit viewConfig: ViewConfig)
    extends FrontendController(cc) {

  private val indirectDisposalsEnabled: Boolean =
    config.underlying.getBoolean("indirect-disposals.enabled")

  private val mixedUseEnabled: Boolean =
    config.underlying.getBoolean("mixed-use.enabled")

  def landingPage(): Action[AnyContent] =
    Action(implicit request => Ok(landing_page(indirectDisposalsEnabled, mixedUseEnabled)))

  def agentsLandingPage(): Action[AnyContent] =
    Action(implicit request =>
      Ok(
        agents_landing_page(indirectDisposalsEnabled, mixedUseEnabled)
      )
    )

  def signInPage(): Action[AnyContent] =
    Action(_ => Redirect(routes.LandingPageController.landingPage()))

}
