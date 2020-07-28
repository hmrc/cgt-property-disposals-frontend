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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance

import cats.Eq
import cats.instances.int._
import cats.syntax.eq._
import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait ClearingReason

object ClearingReason {
  case object IncomingPayment extends ClearingReason
  case object OutgoingPayment extends ClearingReason
  case object WriteOff extends ClearingReason
  case object Reversal extends ClearingReason
  case object MassWriteOff extends ClearingReason
  case object AutomaticClearing extends ClearingReason
  case object SomeOtherClearingReason extends ClearingReason
  case object InvalidClearingReason extends ClearingReason

  def fromString(clearingReason: String): ClearingReason =
    clearingReason match {
      case "Incoming Payment"            => IncomingPayment
      case "Outgoing Payment"            => OutgoingPayment
      case "Write-Off"                   => WriteOff
      case "Reversal"                    => Reversal
      case "Mass Write-Off"              => MassWriteOff
      case "Automatic Clearing"          => AutomaticClearing
      case reason if reason.length =!= 0 => SomeOtherClearingReason
      case reason if reason.length === 0 => InvalidClearingReason
    }

  implicit val eq: Eq[ClearingReason]          = Eq.fromUniversalEquals
  implicit val format: OFormat[ClearingReason] = derived.oformat()
}
