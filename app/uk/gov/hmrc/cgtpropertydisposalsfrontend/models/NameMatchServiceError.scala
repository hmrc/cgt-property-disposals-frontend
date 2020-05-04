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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails

sealed trait NameMatchServiceError[+A <: NameMatchDetails] extends Product with Serializable

object NameMatchServiceError {

  final case class BackendError(error: Error) extends NameMatchServiceError[Nothing]

  final case class TooManyUnsuccessfulAttempts() extends NameMatchServiceError[Nothing]

  final case class NameMatchFailed[A <: NameMatchDetails](
    unsuccessfulNameMatchAttempts: UnsuccessfulNameMatchAttempts[A]
  ) extends NameMatchServiceError[A]

}
