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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance

import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait PaymentMethod extends Product with Serializable

object PaymentMethod {

  case object BACS extends PaymentMethod

  case object CHAPS extends PaymentMethod

  case object Cheque extends PaymentMethod

  case object DebitCardByTelephone extends PaymentMethod

  case object CreditCardByTelephone extends PaymentMethod

  case object PTAOnlineWorldpayDebitCardPayment extends PaymentMethod

  case object PTAOnlineWorldpayCreditCardPayment extends PaymentMethod

  case object GIROReceipts extends PaymentMethod

  case object GIROCredits extends PaymentMethod

  case object ChequeReceipts extends PaymentMethod

  case object DirectDebit extends PaymentMethod

  case object FasterPayment extends PaymentMethod

  case object GiroBankReceipts extends PaymentMethod

  case object GiroBankPostOffice extends PaymentMethod

  case object Paymaster extends PaymentMethod

  case object BankLodgement extends PaymentMethod

  case object Incentive extends PaymentMethod

  case object LocalOfficePayment extends PaymentMethod

  case object NilDeclarations extends PaymentMethod

  case object OverpaymentsToDuty extends PaymentMethod

  case object ReallocationFromOASToDuty extends PaymentMethod

  case object PaymentNotExpected extends PaymentMethod

  case object Reallocation extends PaymentMethod

  case object RepaymentInterestAllocated extends PaymentMethod

  case object VoluntaryDirectPayments extends PaymentMethod

  implicit val format: OFormat[PaymentMethod] = derived.oformat()

}
