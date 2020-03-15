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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import cats.data.EitherT
import cats.instances.future._
import play.api.mvc.Request
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.AgentReferenceNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalDraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait ReturnsServiceSupport { this: ControllerSpec =>

  val mockReturnsService: ReturnsService = mock[ReturnsService]

  def mockStoreDraftReturn(draftReturn: SingleDisposalDraftReturn, agentReferenceNumber: Option[AgentReferenceNumber])(
    result: Either[Error, Unit]
  ) =
    (mockReturnsService
      .storeDraftReturn(_: SingleDisposalDraftReturn, _: Option[AgentReferenceNumber])(_: HeaderCarrier, _: Request[_]))
      .expects(draftReturn, agentReferenceNumber, *, *)
      .returning(EitherT.fromEither[Future](result))

}
