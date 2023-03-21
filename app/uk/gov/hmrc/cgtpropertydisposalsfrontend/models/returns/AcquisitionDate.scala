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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns

import play.api.libs.functional.syntax._
import play.api.libs.json.Format

import java.time.LocalDate

final case class AcquisitionDate(value: LocalDate) extends AnyVal

object AcquisitionDate {

  implicit val format: Format[AcquisitionDate] =
    implicitly[Format[LocalDate]].inmap(AcquisitionDate(_), _.value)

}
