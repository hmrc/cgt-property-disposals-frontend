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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.util

import play.api.libs.json.JsError

object JsErrorOps {

  implicit def j(jsError: JsError): JsErrorOps = new JsErrorOps(jsError)

}

class JsErrorOps(val error: JsError) extends AnyVal {

  /**
    * Create a legible string describing the error suitable for debugging purposes
    */
  def prettyPrint(): String =
    error.errors
      .map {
        case (jsPath, validationErrors) ⇒
          jsPath.toString + ": [" + validationErrors.map(_.message).mkString(",") + "]"
      }
      .mkString("; ")

}
