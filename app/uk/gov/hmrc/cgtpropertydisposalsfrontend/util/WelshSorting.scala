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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.util

import cats.instances.int._
import cats.instances.string._
import cats.syntax.eq._
import org.apache.commons.lang3.StringUtils

object WelshSorting {
  val WELSH_ALPHABET = Map(
    " "  -> 0,
    "a"  -> 1,
    "b"  -> 2,
    "c"  -> 3,
    "ch" -> 4,
    "d"  -> 5,
    "dd" -> 6,
    "e"  -> 7,
    "f"  -> 8,
    "ff" -> 9,
    "g"  -> 10,
    "ng" -> 11,
    "h"  -> 12,
    "i"  -> 13,
    "j"  -> 14,
    "k"  -> 15,
    "l"  -> 16,
    "ll" -> 17,
    "m"  -> 18,
    "n"  -> 19,
    "o"  -> 20,
    "p"  -> 21,
    "ph" -> 22,
    "q"  -> 23,
    "r"  -> 24,
    "rh" -> 25,
    "s"  -> 26,
    "t"  -> 27,
    "th" -> 28,
    "u"  -> 29,
    "v"  -> 30,
    "w"  -> 31,
    "x"  -> 32,
    "y"  -> 33,
    "z"  -> 34
  )

  val YR = "yr "

  val welshCharacters = WELSH_ALPHABET.filter(s => s._1.length === 2)

  implicit class WelshStringOps(private val s: String) extends AnyVal {

    def isBeforeInWelsh(other: String): Boolean =
      if (other === s) false
      else {
        val listOfWeights        = convertToCharacterValues(clean(s), List.empty)
        val compareListOfWeights = convertToCharacterValues(clean(other), List.empty)

        val maxLength      = Math.min(listOfWeights.length, compareListOfWeights.length)
        var divergentIndex = 0
        while (divergentIndex < maxLength && listOfWeights(divergentIndex) === compareListOfWeights(divergentIndex))
          divergentIndex = divergentIndex + 1

        //Is a substring, and return shorter
        if (divergentIndex === maxLength)
          compareListOfWeights.length > listOfWeights.length
        else
          listOfWeights(divergentIndex) < compareListOfWeights(divergentIndex)
      }

    def clean(code: String): String =
      //Some welsh country codes have the prefix "Yr" - e.g "Yr foo". We are only interested 'foo'
      if (code.toLowerCase().startsWith(YR)) {
        val truncated = code.substring(2, code.length)
        StringUtils.stripAccents(truncated).replaceAll("[^A-Za-z0-9] ", "").toLowerCase()
      } else StringUtils.stripAccents(code).replaceAll("[^A-Za-z0-9] ", "").toLowerCase()

    @scala.annotation.tailrec
    def convertToCharacterValues(str: String, acc: List[Int]): List[Int] =
      str match {
        case ""                                        => acc
        case s if validMultiCharacterWelshCharacter(s) =>
          convertToCharacterValues(s.substring(2), acc ::: List(WELSH_ALPHABET(s.substring(0, 2))))
        case s                                         => convertToCharacterValues(s.substring(1), acc ::: List(WELSH_ALPHABET(s.substring(0, 1))))

      }
  }

  def validMultiCharacterWelshCharacter(str: String) = str.length > 1 && welshCharacters.contains(str.substring(0, 2))
}
