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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.util

import play.api.libs.json.{JsDefined, JsError, Reads}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.JsErrorOps._
import uk.gov.hmrc.http.HttpResponse

import scala.util.Try
import cats.syntax.either._

object HttpResponseOps {

  implicit class HttpResponseOps(private val response: HttpResponse) extends AnyVal {

    def parseJSON[A](path: Option[String] = None)(implicit reads: Reads[A]): Either[String, A] =
      for {
        json          <-
          Try(response.json).toEither.leftMap(error => s"Could not read http response as JSON: ${error.getMessage}")
        // response.json failed in this case - there was no JSON in the response
        jsLookupResult =
          path match {
            case None       => JsDefined(json)
            case Some(path) => json \ path
          }
        // use Option here to filter out null values
        result        <- jsLookupResult.toOption.toRight("No JSON found in body of http response")
        // there was JSON in the response but we couldn't read it
        deserialized  <- result
                           .validate[A]
                           .asEither
                           .leftMap(errors => s"Could not parse http response JSON: ${JsError(errors).prettyPrint()}")
      } yield deserialized

  }
}
