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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators

import org.scalacheck.Gen
import org.scalacheck.ScalacheckShapeless._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, Charge, Payment, PaymentsJourney}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AmountInPenceWithSource
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SubmitReturnResponse.ReturnCharge

object MoneyGen extends GenUtils {

  implicit val amountInPenceGen: Gen[AmountInPence] = longArb.arbitrary.map(AmountInPence(_))

  implicit val chargeGen: Gen[Charge] = gen[Charge]

  implicit val returnCharge: Gen[ReturnCharge] = gen[ReturnCharge]

  implicit val paymentGen: Gen[Payment] = gen[Payment]

  implicit val paymentsJourneyGen: Gen[PaymentsJourney] = gen[PaymentsJourney]

  implicit val amountInPenceWithSourceGen: Gen[AmountInPenceWithSource] =
    gen[AmountInPenceWithSource]

}
