/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.http
import cats.instances.string._
import cats.syntax.eq._
import play.api.i18n.Lang
sealed trait AcceptLanguage

object AcceptLanguage {
  case object EN extends AcceptLanguage
  case object CY extends AcceptLanguage

  def fromPlayLang(lang: Lang): Option[AcceptLanguage] =
    if (lang.language === "cy") Some(CY) else if (lang.language === "en") Some(EN) else None
}
