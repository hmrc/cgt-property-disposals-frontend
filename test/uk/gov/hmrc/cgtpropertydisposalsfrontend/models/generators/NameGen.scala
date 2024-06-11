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

import org.scalacheck.Gen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName, TrustName}

object NameGen extends GenUtils {

  implicit val contactNameGen: Gen[ContactName] = Generators.stringGen.map(ContactName(_))

  implicit val individualNameGen: Gen[IndividualName] =
    for {
      firstName <- Generators.stringGen
      lastName  <- Generators.stringGen
    } yield IndividualName(firstName, lastName)

  implicit val trustNameGen: Gen[TrustName] = Generators.stringGen.map(TrustName(_))

}
