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

import java.time.LocalDateTime

import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}

sealed trait UploadSupportingEvidenceAnswers extends Product with Serializable

object UploadSupportingEvidenceAnswers {

  final case class SupportingEvidence(
    reference: String,
    fileName: String,
    createdOn: LocalDateTime
  )

  object SupportingEvidence {
    implicit val format: OFormat[SupportingEvidence] = Json.format
  }

  final case class IncompleteUploadSupportingEvidenceAnswers(
    doYouWantToUploadSupportingEvidence: Option[Boolean],
    evidences: List[SupportingEvidence],
    expiredEvidences: List[SupportingEvidence]
  ) extends UploadSupportingEvidenceAnswers

  object IncompleteUploadSupportingEvidenceAnswers {
    val empty: IncompleteUploadSupportingEvidenceAnswers =
      IncompleteUploadSupportingEvidenceAnswers(None, List.empty, List.empty)
  }

  final case class CompleteUploadSupportingEvidenceAnswers(
    doYouWantToUploadSupportingEvidence: Boolean,
    evidences: List[SupportingEvidence]
  ) extends UploadSupportingEvidenceAnswers

  implicit class UploadSupportingDocumentsOps(private val a: UploadSupportingEvidenceAnswers) extends AnyVal {

    def fold[A](
      ifIncomplete: IncompleteUploadSupportingEvidenceAnswers => A,
      ifComplete: CompleteUploadSupportingEvidenceAnswers => A
    ): A = a match {
      case i: IncompleteUploadSupportingEvidenceAnswers => ifIncomplete(i)
      case c: CompleteUploadSupportingEvidenceAnswers   => ifComplete(c)
    }

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val format: OFormat[UploadSupportingEvidenceAnswers] = derived.oformat()

}
