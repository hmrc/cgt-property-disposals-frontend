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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding.IvConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.MockMetrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.iv.IvErrorStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.IvServiceImpl.IvStatusResponse
import uk.gov.hmrc.http.HeaderCarrier

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class IvServiceImplSpec extends AnyWordSpec with Matchers with MockFactory {

  private val mockConnector = mock[IvConnector]

  val service = new IvServiceImpl(mockConnector, MockMetrics.metrics)

  private def mockIvGetFailedJourneyStatus(
    journeyId: UUID
  )(result: Either[Error, IvStatusResponse]) =
    (mockConnector
      .getFailedJourneyStatus(_: UUID)(_: HeaderCarrier))
      .expects(journeyId, *)
      .returning(EitherT.fromEither[Future](result))

  "IvServiceImpl" when {

    "getting a failed IV journey status" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val journeyId                  = UUID.randomUUID()

      "return an error" when {

        "there is an error calling the IV service" in {
          mockIvGetFailedJourneyStatus(journeyId)(Left(Error("some error")))

          val result = await(service.getFailedJourneyStatus(journeyId).value)

          result shouldBe Left(Error("some error"))
        }
      }

      "return the status" when {

        "the response comes back with a JSON body which can be parsed" in {
          List(
            "Incomplete"           -> Incomplete,
            "FailedMatching"       -> FailedMatching,
            "FailedIV"             -> FailedIV,
            "InsufficientEvidence" -> InsufficientEvidence,
            "LockedOut"            -> LockedOut,
            "UserAborted"          -> UserAborted,
            "Timeout"              -> Timeout,
            "TechnicalIssue"       -> TechnicalIssue,
            "PreconditionFailed"   -> PreconditionFailed,
            "???"                  -> Unknown("???")
          ).foreach { case (statusString, status) =>
            mockIvGetFailedJourneyStatus(journeyId)(Right(IvStatusResponse(statusString)))

            val result = await(service.getFailedJourneyStatus(journeyId).value)

            result shouldBe Right(status)
          }
        }
      }
    }
  }
}
