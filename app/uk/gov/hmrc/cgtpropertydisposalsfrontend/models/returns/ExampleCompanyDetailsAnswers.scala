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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns

import com.github.ghik.silencer.silent
import julienrf.json.derived
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

sealed trait ExampleCompanyDetailsAnswers extends Product with Serializable

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object ExampleCompanyDetailsAnswers {

  @Lenses
  final case class IncompleteExampleCompanyDetailsAnswers(
    address: Option[Address],
    disposalPrice: Option[AmountInPence],
    acquisitionPrice: Option[AmountInPence]
  ) extends ExampleCompanyDetailsAnswers

  object IncompleteExampleCompanyDetailsAnswers {
    val empty: IncompleteExampleCompanyDetailsAnswers =
      IncompleteExampleCompanyDetailsAnswers(None, None, None)

    def fromCompleteAnswers(
      c: CompleteExampleCompanyDetailsAnswers
    ): IncompleteExampleCompanyDetailsAnswers =
      IncompleteExampleCompanyDetailsAnswers(
        Some(c.address),
        Some(c.disposalPrice),
        Some(c.acquisitionPrice)
      )

  }

  final case class CompleteExampleCompanyDetailsAnswers(
    address: Address,
    disposalPrice: AmountInPence,
    acquisitionPrice: AmountInPence
  ) extends ExampleCompanyDetailsAnswers

  implicit class ExampleCompanyDetailsAnswersOps(
    private val m: ExampleCompanyDetailsAnswers
  ) extends AnyVal {

    def fold[A](
      whenIncomplete: IncompleteExampleCompanyDetailsAnswers => A,
      whenComplete: CompleteExampleCompanyDetailsAnswers => A
    ): A =
      m match {
        case i: IncompleteExampleCompanyDetailsAnswers => whenIncomplete(i)
        case c: CompleteExampleCompanyDetailsAnswers   => whenComplete(c)
      }

    def unset[A](
      fieldLens: IncompleteExampleCompanyDetailsAnswers.type => Lens[
        IncompleteExampleCompanyDetailsAnswers,
        Option[
          A
        ]
      ]
    ): IncompleteExampleCompanyDetailsAnswers =
      fieldLens(IncompleteExampleCompanyDetailsAnswers).set(None)(
        fold(
          identity,
          IncompleteExampleCompanyDetailsAnswers.fromCompleteAnswers
        )
      )

  }

  @silent
  implicit val format: OFormat[ExampleCompanyDetailsAnswers] = derived.oformat()

}
