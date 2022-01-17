/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan

import julienrf.json.derived
import play.api.libs.json.{Json, OFormat}

sealed trait UpscanCallBack extends Product with Serializable

object UpscanCallBack {

  final case class UpscanSuccess(
    reference: String,
    fileStatus: String,
    downloadUrl: String,
    uploadDetails: Map[String, String]
  ) extends UpscanCallBack

  object UpscanSuccess {
    implicit val format: OFormat[UpscanSuccess] = Json.format[UpscanSuccess]
  }

  final case class UpscanFailure(
    reference: String,
    fileStatus: String,
    failureDetails: Map[String, String]
  ) extends UpscanCallBack

  object UpscanFailure {
    implicit val format: OFormat[UpscanFailure] = Json.format[UpscanFailure]
  }

  implicit class UpscanSuccessOps(private val u: UpscanSuccess) extends AnyVal {

    def fileName: String =
      u.uploadDetails.getOrElse(
        "fileName",
        sys.error(s"Could not find filename for reference ${u.reference}")
      )
  }

  implicit val format: OFormat[UpscanCallBack] = derived.oformat()

}
