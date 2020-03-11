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
import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.TaxYear
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country

sealed trait MultipleDisposalsTriageAnswers

object MultipleDisposalsTriageAnswers {

  final case class IncompleteMultipleDisposalsAnswers(
    individualUserType: Option[IndividualUserType],
    numberOfProperties: Option[Int],
    wasAUKResident: Option[Boolean],
    countryOfResidence: Option[Country],
    wereAllPropertiesResidential: Option[Boolean],
    assetType: Option[AssetType],
    taxYearAfter6April2020: Option[Boolean],
    taxYear: Option[TaxYear]
  ) extends MultipleDisposalsTriageAnswers

  object IncompleteMultipleDisposalsAnswers {
    val empty: IncompleteMultipleDisposalsAnswers =
      IncompleteMultipleDisposalsAnswers(None, None, None, None, None, None, None, None)
  }

  final case class CompleteMultipleDisposalsAnswers(
    individualUserType: Option[IndividualUserType],
    numberOfProperties: Int,
    countryOfResidence: Country,
    assetType: AssetType,
    taxYear: TaxYear
  ) extends MultipleDisposalsTriageAnswers

  implicit class MultipleDisposalsTriageAnswersOps(private val m: MultipleDisposalsTriageAnswers) extends AnyVal {
    def fold[A](
      ifIncomplete: IncompleteMultipleDisposalsAnswers => A,
      ifComplete: CompleteMultipleDisposalsAnswers => A
    ): A = m match {
      case i: IncompleteMultipleDisposalsAnswers => ifIncomplete(i)
      case c: CompleteMultipleDisposalsAnswers   => ifComplete(c)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[MultipleDisposalsTriageAnswers] = derived.oformat()
}
