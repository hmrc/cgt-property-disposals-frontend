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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns

import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence

sealed trait MultipleDisposalsExamplePropertyDetailsAnswers extends Product with Serializable

object MultipleDisposalsExamplePropertyDetailsAnswers {

  final case class IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(
    address: Option[UkAddress],
    disposalDate: Option[DisposalDate],
    disposalPrice: Option[AmountInPence],
    acquisitionPrice: Option[AmountInPence]
  ) extends MultipleDisposalsExamplePropertyDetailsAnswers

  object IncompleteMultipleDisposalsExamplePropertyDetailsAnswers {
    val empty: IncompleteMultipleDisposalsExamplePropertyDetailsAnswers =
      IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(None, None, None, None)
  }

  final case class CompleteMultipleDisposalsExamplePropertyDetailsAnswers(
    address: UkAddress,
    disposalDate: DisposalDate,
    disposalPrice: AmountInPence,
    acquisitionPrice: AmountInPence
  ) extends MultipleDisposalsExamplePropertyDetailsAnswers

  implicit class MultipleDisposalsExamplePropertyDetailsAnswersOps(
    private val m: MultipleDisposalsExamplePropertyDetailsAnswers
  ) extends AnyVal {

    def fold[A](
      whenIncomplete: IncompleteMultipleDisposalsExamplePropertyDetailsAnswers => A,
      whenComplete: CompleteMultipleDisposalsExamplePropertyDetailsAnswers => A
    ): A = m match {
      case i: IncompleteMultipleDisposalsExamplePropertyDetailsAnswers => whenIncomplete(i)
      case c: CompleteMultipleDisposalsExamplePropertyDetailsAnswers   => whenComplete(c)
    }

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[MultipleDisposalsExamplePropertyDetailsAnswers] = {
    implicit val ukAddressFormat: OFormat[UkAddress] = Json.format
    derived.oformat()
  }

}
