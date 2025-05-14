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

import play.api.libs.json.*

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

  implicit val format: Format[PaymentMethod] = new Format[PaymentMethod] {
    override def reads(json: JsValue): JsResult[PaymentMethod] = json match {
      case JsObject(fields) if fields.size == 1 =>
        fields.head._1 match {
          case "BACS"                               => JsSuccess(BACS)
          case "CHAPS"                              => JsSuccess(CHAPS)
          case "Cheque"                             => JsSuccess(Cheque)
          case "DebitCardByTelephone"               => JsSuccess(DebitCardByTelephone)
          case "CreditCardByTelephone"              => JsSuccess(CreditCardByTelephone)
          case "PTAOnlineWorldpayDebitCardPayment"  => JsSuccess(PTAOnlineWorldpayDebitCardPayment)
          case "PTAOnlineWorldpayCreditCardPayment" => JsSuccess(PTAOnlineWorldpayCreditCardPayment)
          case "GIROReceipts"                       => JsSuccess(GIROReceipts)
          case "GIROCredits"                        => JsSuccess(GIROCredits)
          case "ChequeReceipts"                     => JsSuccess(ChequeReceipts)
          case "DirectDebit"                        => JsSuccess(DirectDebit)
          case "FasterPayment"                      => JsSuccess(FasterPayment)
          case "GiroBankReceipts"                   => JsSuccess(GiroBankReceipts)
          case "GiroBankPostOffice"                 => JsSuccess(GiroBankPostOffice)
          case "Paymaster"                          => JsSuccess(Paymaster)
          case "BankLodgement"                      => JsSuccess(BankLodgement)
          case "Incentive"                          => JsSuccess(Incentive)
          case "LocalOfficePayment"                 => JsSuccess(LocalOfficePayment)
          case "NilDeclarations"                    => JsSuccess(NilDeclarations)
          case "OverpaymentsToDuty"                 => JsSuccess(OverpaymentsToDuty)
          case "ReallocationFromOASToDuty"          => JsSuccess(ReallocationFromOASToDuty)
          case "PaymentNotExpected"                 => JsSuccess(PaymentNotExpected)
          case "Reallocation"                       => JsSuccess(Reallocation)
          case "RepaymentInterestAllocated"         => JsSuccess(RepaymentInterestAllocated)
          case "VoluntaryDirectPayments"            => JsSuccess(VoluntaryDirectPayments)
          case other                                => JsError(s"Invalid payment method: $other")
        }
      case _                                    => JsError("Expected a JSON object with one PaymentMethod key")
    }

    override def writes(o: PaymentMethod): JsValue = o match {
      case BACS                               => Json.obj("BACS" -> Json.obj())
      case CHAPS                              => Json.obj("CHAPS" -> Json.obj())
      case Cheque                             => Json.obj("Cheque" -> Json.obj())
      case DebitCardByTelephone               => Json.obj("DebitCardByTelephone" -> Json.obj())
      case CreditCardByTelephone              => Json.obj("CreditCardByTelephone" -> Json.obj())
      case PTAOnlineWorldpayDebitCardPayment  => Json.obj("PTAOnlineWorldpayDebitCardPayment" -> Json.obj())
      case PTAOnlineWorldpayCreditCardPayment => Json.obj("PTAOnlineWorldpayCreditCardPayment" -> Json.obj())
      case GIROReceipts                       => Json.obj("GIROReceipts" -> Json.obj())
      case GIROCredits                        => Json.obj("GIROCredits" -> Json.obj())
      case ChequeReceipts                     => Json.obj("ChequeReceipts" -> Json.obj())
      case DirectDebit                        => Json.obj("DirectDebit" -> Json.obj())
      case FasterPayment                      => Json.obj("FasterPayment" -> Json.obj())
      case GiroBankReceipts                   => Json.obj("GiroBankReceipts" -> Json.obj())
      case GiroBankPostOffice                 => Json.obj("GiroBankPostOffice" -> Json.obj())
      case Paymaster                          => Json.obj("Paymaster" -> Json.obj())
      case BankLodgement                      => Json.obj("BankLodgement" -> Json.obj())
      case Incentive                          => Json.obj("Incentive" -> Json.obj())
      case LocalOfficePayment                 => Json.obj("LocalOfficePayment" -> Json.obj())
      case NilDeclarations                    => Json.obj("NilDeclarations" -> Json.obj())
      case OverpaymentsToDuty                 => Json.obj("OverpaymentsToDuty" -> Json.obj())
      case ReallocationFromOASToDuty          => Json.obj("ReallocationFromOASToDuty" -> Json.obj())
      case PaymentNotExpected                 => Json.obj("PaymentNotExpected" -> Json.obj())
      case Reallocation                       => Json.obj("Reallocation" -> Json.obj())
      case RepaymentInterestAllocated         => Json.obj("RepaymentInterestAllocated" -> Json.obj())
      case VoluntaryDirectPayments            => Json.obj("VoluntaryDirectPayments" -> Json.obj())
    }
  }
}
