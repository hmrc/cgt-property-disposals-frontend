/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.iv

sealed trait IvErrorResponse extends Product with Serializable

object IvErrorResponse {

  final case object Incomplete extends IvErrorResponse

  final case object FailedMatching extends IvErrorResponse

  final case object FailedIV extends IvErrorResponse

  final case object InsufficientEvidence extends IvErrorResponse

  final case object LockedOut extends IvErrorResponse

  final case object UserAborted extends IvErrorResponse

  final case object Timeout extends IvErrorResponse

  final case object TechnicalIssue extends IvErrorResponse

  final case object PreconditionFailed extends IvErrorResponse

  final case class Unknown(value: String) extends IvErrorResponse

  def fromString(s: String): IvErrorResponse = {
    s match {
      case "Incomplete"           ⇒ Incomplete
      case "FailedMatching"       ⇒ FailedMatching
      case "FailedIV"             ⇒ FailedIV
      case "InsufficientEvidence" ⇒ InsufficientEvidence
      case "LockedOut"            ⇒ LockedOut
      case "UserAborted"          ⇒ UserAborted
      case "Timeout"              ⇒ Timeout
      case "TechnicalIssue"       ⇒ TechnicalIssue
      case "PreconditionFailed"   ⇒ PreconditionFailed
      case other                  ⇒ Unknown(other)
    }
  }

}