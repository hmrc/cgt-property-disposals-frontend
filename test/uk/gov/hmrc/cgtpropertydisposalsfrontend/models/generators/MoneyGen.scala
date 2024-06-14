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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators

import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SubmitReturnResponse.ReturnCharge
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AmountInPenceWithSource, Source}

import java.time.LocalDate

object MoneyGen extends GenUtils {

  implicit val amountInPenceGen: Gen[AmountInPence] = longArb.arbitrary.map(AmountInPence(_))

  implicit val returnCharge: Gen[ReturnCharge] = for {
    chargeReference <- Generators.stringGen
    amount          <- amountInPenceGen
    dueDate         <- Arbitrary.arbitrary[LocalDate]
  } yield ReturnCharge(chargeReference, amount, dueDate)

  implicit val paymentGen: Gen[Payment] = for {
    amount         <- amountInPenceGen
    method         <-
      Gen.oneOf(
        None,
        Some(PaymentMethod.BACS),
        Some(PaymentMethod.CHAPS),
        Some(PaymentMethod.Cheque),
        Some(PaymentMethod.DebitCardByTelephone),
        Some(PaymentMethod.CreditCardByTelephone),
        Some(PaymentMethod.PTAOnlineWorldpayDebitCardPayment),
        Some(PaymentMethod.PTAOnlineWorldpayCreditCardPayment),
        Some(PaymentMethod.GIROReceipts),
        Some(PaymentMethod.GIROCredits),
        Some(PaymentMethod.ChequeReceipts),
        Some(PaymentMethod.DirectDebit),
        Some(PaymentMethod.FasterPayment),
        Some(PaymentMethod.GiroBankReceipts),
        Some(PaymentMethod.GiroBankPostOffice),
        Some(PaymentMethod.Paymaster),
        Some(PaymentMethod.BankLodgement),
        Some(PaymentMethod.Incentive),
        Some(PaymentMethod.LocalOfficePayment),
        Some(PaymentMethod.NilDeclarations),
        Some(PaymentMethod.OverpaymentsToDuty),
        Some(PaymentMethod.ReallocationFromOASToDuty),
        Some(PaymentMethod.PaymentNotExpected),
        Some(PaymentMethod.Reallocation),
        Some(PaymentMethod.RepaymentInterestAllocated),
        Some(PaymentMethod.VoluntaryDirectPayments)
      )
    clearingDate   <- Arbitrary.arbitrary[LocalDate]
    clearingReason <- Gen.oneOf(
                        None,
                        Some(ClearingReason.IncomingPayment),
                        Some(ClearingReason.OutgoingPayment),
                        Some(ClearingReason.WriteOff),
                        Some(ClearingReason.Reversal),
                        Some(ClearingReason.MassWriteOff),
                        Some(ClearingReason.AutomaticClearing),
                        Some(ClearingReason.SomeOtherClearingReason)
                      )
  } yield Payment(amount, method, clearingDate, clearingReason)

  private val chargeType = Gen.oneOf(
    ChargeType.UkResidentReturn,
    ChargeType.NonUkResidentReturn,
    ChargeType.DeltaCharge,
    ChargeType.Interest,
    ChargeType.LateFilingPenalty,
    ChargeType.SixMonthLateFilingPenalty,
    ChargeType.TwelveMonthLateFilingPenalty,
    ChargeType.LatePaymentPenalty,
    ChargeType.SixMonthLatePaymentPenalty,
    ChargeType.TwelveMonthLatePaymentPenalty,
    ChargeType.PenaltyInterest
  )

  implicit val chargeGen: Gen[Charge] = for {
    chargeType      <- chargeType
    chargeReference <- Generators.stringGen
    amount          <- amountInPenceGen
    dueDate         <- Arbitrary.arbitrary[LocalDate]
    payment         <- paymentGen
    payments        <- Gen.oneOf(List(), List(payment))
  } yield Charge(chargeType, chargeReference, amount, dueDate, payments)

  for {
    chargeType      <- chargeType
    chargeReference <- Generators.stringGen
    amount          <- amountInPenceGen
    dueDate         <- Arbitrary.arbitrary[LocalDate]
    payment         <- paymentGen
    payments        <- Gen.oneOf(List(), List(payment))
  } yield Charge(chargeType, chargeReference, amount, dueDate, payments)

  implicit val paymentsJourneyGen: Gen[PaymentsJourney] = for {
    fst <- Generators.stringGen
    snd <- Generators.stringGen
  } yield PaymentsJourney(fst, snd)

  implicit val amountInPenceWithSourceGen: Gen[AmountInPenceWithSource] = {
    for {
      amount <- amountInPenceGen
      source <- Gen.oneOf(Source.Calculated, Source.UserSupplied)
    } yield AmountInPenceWithSource(amount, source)
  }

}
