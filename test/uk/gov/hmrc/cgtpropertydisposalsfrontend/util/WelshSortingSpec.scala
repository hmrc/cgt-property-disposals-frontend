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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.util

import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.WelshSorting.WelshStringOps

class WelshSortingSpec extends WordSpec with Matchers with MockFactory {

  "WelshSortingSpec" when {

    "Welsh sorting" must {
      "Must be sorted correctly" in {
        List(
          (List("co", "cho"), List("co", "cho")),
          (List("cho", "co"), List("co", "cho")),
          (List("afo", "affo"), List("afo", "affo")),
          (List("affo", "afo"), List("afo", "affo")),
          (List("wp", "wph"), List("wp", "wph")),
          (List("wph", "wp"), List("wp", "wph")),
          (List("a po", "a pho"), List("a po", "a pho")),
          (List("a pho", "a po"), List("a po", "a pho")),
          (List("Af", "Yr Ab"), List("Yr Ab", "Af")),
          (List("Yr Af", "Yr Am"), List("Yr Af", "Yr Am")),
          (List("aaaaaaaaaaaaaaaaaaaaaaaaaaaa", "a"), List("a", "aaaaaaaaaaaaaaaaaaaaaaaaaaaa")),
          (List("a", "aaaaaaaaaaaaaaaaaaaaaaaaaaaa"), List("a", "aaaaaaaaaaaaaaaaaaaaaaaaaaaa")),
          (List("a b c d e hd ", "a ch as g"), List("a b c d e hd ", "a ch as g")),
          (List("a ch as g", "a b c d e hd "), List("a b c d e hd ", "a ch as g")),
          (List("yr achff", "yr achfflkjdsflkjsd"), List("yr achff", "yr achfflkjdsflkjsd")),
          (List("yr achfflkjdsflkjsd", "yr achff"), List("yr achff", "yr achfflkjdsflkjsd")),
          (List("yr same llch", "yr samee llch"), List("yr same llch", "yr samee llch"))
        ).foreach { input =>
          withClue(input) {
            val list = input._1.sortWith { case (countryName1, countryName2) =>
              countryName1.isBeforeInWelsh(countryName2)
            }
            list shouldBe input._2
          }
        }
      }
    }

  }

}
