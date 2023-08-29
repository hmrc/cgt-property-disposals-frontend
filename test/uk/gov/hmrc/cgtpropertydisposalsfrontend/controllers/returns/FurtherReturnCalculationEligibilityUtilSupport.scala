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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.handlers.CallHandler3
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{FurtherReturnCalculationEligibility, FurtherReturnCalculationEligibilityUtil}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait FurtherReturnCalculationEligibilityUtilSupport { this: ControllerSpec =>

  val mockFurtherReturnCalculationEligibilityUtil: FurtherReturnCalculationEligibilityUtil =
    mock[FurtherReturnCalculationEligibilityUtil]

  protected def mockFurtherReturnCalculationEligibilityCheck(
    fillingOutReturn: FillingOutReturn
  )(
    result: Either[Error, FurtherReturnCalculationEligibility]
  ): CallHandler3[FillingOutReturn, HeaderCarrier, RequestWithSessionData[_], EitherT[
    Future,
    Error,
    FurtherReturnCalculationEligibility
  ]] =
    (
      mockFurtherReturnCalculationEligibilityUtil
        .isEligibleForFurtherReturnOrAmendCalculation(
          _: FillingOutReturn
        )(
          _: HeaderCarrier,
          _: RequestWithSessionData[_]
        )
      )
      .expects(fillingOutReturn, *, *)
      .returning(EitherT.fromEither[Future](result))

}
