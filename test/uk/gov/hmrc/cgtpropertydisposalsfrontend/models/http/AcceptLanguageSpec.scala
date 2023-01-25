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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.http

import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.i18n.Lang
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.http.AcceptLanguage.{CY, EN, fromPlayLang}

class AcceptLanguageSpec extends AnyWordSpec with Matchers with MockFactory {

  "AcceptLanguage" should {

    "return the welsh " in {
      val Language: Lang = Lang("cy")
      val function       = fromPlayLang(Language)
      function shouldBe Some(CY)
    }

    "return the english " in {
      val Language: Lang = Lang("en")
      val function       = fromPlayLang(Language)
      function shouldBe Some(EN)
    }

    "return None" in {
      val Language: Lang = Lang("sp")
      val function       = fromPlayLang(Language)
      function shouldBe None
    }
  }
}
