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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns

import julienrf.json.derived

import play.api.libs.json.OFormat
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName

sealed trait RepresenteeAnswers extends Product with Serializable

object RepresenteeAnswers {

  final case class IncompleteRepresenteeAnswers(name: Option[IndividualName], id: Option[RepresenteeReferenceId])
      extends RepresenteeAnswers

  object IncompleteRepresenteeAnswers {

    val empty: IncompleteRepresenteeAnswers = IncompleteRepresenteeAnswers(None, None)

  }

  final case class CompleteRepresenteeAnswers(name: IndividualName, id: RepresenteeReferenceId)
      extends RepresenteeAnswers

  implicit class RepresenteeAnswersOps(private val r: RepresenteeAnswers) extends AnyVal {
    def fold[A](ifIncomplete: IncompleteRepresenteeAnswers => A, ifComplete: CompleteRepresenteeAnswers => A): A =
      r match {
        case i: IncompleteRepresenteeAnswers => ifIncomplete(i)
        case c: CompleteRepresenteeAnswers   => ifComplete(c)
      }
  }

  implicit val format: OFormat[RepresenteeAnswers] = derived.oformat()

}
