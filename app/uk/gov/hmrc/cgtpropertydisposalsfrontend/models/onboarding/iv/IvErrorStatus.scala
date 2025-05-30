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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.iv

sealed trait IvErrorStatus extends Product with Serializable

object IvErrorStatus {

  case object Incomplete extends IvErrorStatus

  case object FailedMatching extends IvErrorStatus

  case object FailedIV extends IvErrorStatus

  case object InsufficientEvidence extends IvErrorStatus

  case object LockedOut extends IvErrorStatus

  case object UserAborted extends IvErrorStatus

  case object Timeout extends IvErrorStatus

  case object TechnicalIssue extends IvErrorStatus

  case object PreconditionFailed extends IvErrorStatus

  final case class Unknown(value: String) extends IvErrorStatus

  def fromString(s: String): IvErrorStatus =
    s match {
      case "Incomplete"           => Incomplete
      case "FailedMatching"       => FailedMatching
      case "FailedIV"             => FailedIV
      case "InsufficientEvidence" => InsufficientEvidence
      case "LockedOut"            => LockedOut
      case "UserAborted"          => UserAborted
      case "Timeout"              => Timeout
      case "TechnicalIssue"       => TechnicalIssue
      case "PreconditionFailed"   => PreconditionFailed
      case other                  => Unknown(other)
    }

}
