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
class AppConfig @Inject() (servicesConfig: ServicesConfig) {

  private def loadConfig(key: String): String = servicesConfig.getString(key)

  private val contactHost = servicesConfig.getString(s"contact-frontend.host")
  private val contactFormServiceIdentifier = "MyService"

  val assetsPrefix: String = loadConfig(s"assets.url") + loadConfig(s"assets.version")
  val analyticsToken: String = loadConfig(s"google-analytics.token")
  val analyticsHost: String = loadConfig(s"google-analytics.host")
  val reportAProblemPartialUrl: String = s"$contactHost/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"
  val reportAProblemNonJSUrl: String = s"$contactHost/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"
}
