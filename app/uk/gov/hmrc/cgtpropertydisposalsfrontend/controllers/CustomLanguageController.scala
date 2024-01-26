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

import com.google.inject.Inject
import play.api.Configuration
import play.api.i18n.Lang
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.play.language.{LanguageController, LanguageUtils}

class CustomLanguageController @Inject() (
  configuration: Configuration,
  languageUtils: LanguageUtils,
  cc: MessagesControllerComponents
) extends LanguageController(languageUtils, cc) {
  private val english = Lang("en")
  private val welsh   = Lang("cy")

  override protected def fallbackURL: String = configuration.get[String]("self.url")

  override protected def languageMap: Map[String, Lang] = Map("en" -> english, "cy" -> welsh)

  def switchToEnglish: Action[AnyContent] = switchToLanguage("en")

  def switchToWelsh: Action[AnyContent] = switchToLanguage("cy")
}
