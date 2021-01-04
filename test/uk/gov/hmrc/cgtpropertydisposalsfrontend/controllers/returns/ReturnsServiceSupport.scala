/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import play.api.mvc.Request
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisplayReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait ReturnsServiceSupport { this: MockFactory =>

  val mockReturnsService: ReturnsService = mock[ReturnsService]

  def mockStoreDraftReturn(
    fillingOutReturn: FillingOutReturn
  )(
    result: Either[Error, Unit]
  ) =
    (mockReturnsService
      .storeDraftReturn(
        _: FillingOutReturn
      )(
        _: HeaderCarrier,
        _: Request[_]
      ))
      .expects(fillingOutReturn, *, *)
      .returning(EitherT.fromEither[Future](result))

  def mockDisplayReturn(
    cgtReference: CgtReference,
    submissionId: String
  )(
    result: Either[Error, DisplayReturn]
  ) =
    (mockReturnsService
      .displayReturn(
        _: CgtReference,
        _: String
      )(
        _: HeaderCarrier
      ))
      .expects(cgtReference, submissionId, *)
      .returning(EitherT.fromEither[Future](result))

}
