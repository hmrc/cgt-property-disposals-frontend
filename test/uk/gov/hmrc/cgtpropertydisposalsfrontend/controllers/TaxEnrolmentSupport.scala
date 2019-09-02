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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import cats.data.EitherT
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.TaxEnrolmentConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{EnrolmentRequest, Error}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.Future

trait TaxEnrolmentSupport {

  this: MockFactory =>

  val mockTaxEnrolmentConnector = mock[TaxEnrolmentConnector]

  def mockAllocateEnrolmentToGroup(cgtReference: String, enrolmentRequest: EnrolmentRequest)(
    result: EitherT[Future, Error, HttpResponse]
  ) =
    (mockTaxEnrolmentConnector
      .allocateEnrolmentToGroup(_: String, _: EnrolmentRequest)(_: HeaderCarrier))
      .expects(cgtReference, enrolmentRequest, *)
      .returning(result)

}
