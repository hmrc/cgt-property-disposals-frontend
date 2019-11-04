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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.config

import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

@Singleton
class ViewConfig @Inject()(servicesConfig: ServicesConfig) {

  private def getString(key: String): String = servicesConfig.getString(key)

  private val contactHost                  = getString("contact-frontend.host")
  private val companyAuthUrl: String       = getString("company-auth-frontend.url")
  private val signOutUri: String           = getString("sign-out.uri")
  private val contactFormServiceIdentifier = "CGTPD"

  val assetsPrefix: String   = getString("assets.url") + getString("assets.version")
  val analyticsToken: String = getString("google-analytics.token")
  val analyticsHost: String  = getString("google-analytics.host")
  val reportAProblemPartialUrl: String =
    s"$contactHost/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"
  val reportAProblemNonJSUrl: String =
    s"$contactHost/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"
  val betaFeedbackUrlNoAuth: String =
    s"$contactHost/contact/beta-feedback-unauthenticated?service=$contactFormServiceIdentifier"
  val signOutUrl: String           = s"$companyAuthUrl$signOutUri"
  val trustRegistrationUrl: String = getString("external-url.trust-registration")
  val callChargesUrl: String = getString("external-url.gov-call-charges")
  val additionalNeedsUrl: String = getString("external-url.additional-needs")
  val taxSellHomeUrl: String = getString("external-url.tax-sell-home")
  val taxSellPropertyUrl: String = getString("external-url.tax-sell-property")
  val cgtUrl: String = getString("external-url.capital-gains-tax")
  val legacyCgtNonResidentUrl: String = getString("external-url.legacy-cgt-non-resident")
  val agentsSignInUrl: String = getString("external-url.sign-in-to-agents")
  val createAgentsAccountUrl: String = getString("external-url.create-agents-account")
  val ninoGuidanceUrl: String = getString("external-url.nino-guidance")
}
