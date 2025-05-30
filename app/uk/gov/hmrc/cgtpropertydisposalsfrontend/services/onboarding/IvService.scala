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
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.{Json, Reads}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.onboarding.IvConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.Metrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.iv.IvErrorStatus
import uk.gov.hmrc.http.HeaderCarrier

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.chaining.scalaUtilChainingOps

@ImplementedBy(classOf[IvServiceImpl])
trait IvService {
  def getFailedJourneyStatus(journeyId: UUID)(implicit hc: HeaderCarrier): EitherT[Future, Error, IvErrorStatus]
}

@Singleton
class IvServiceImpl @Inject() (connector: IvConnector, metrics: Metrics)(implicit ec: ExecutionContext)
    extends IvService {

  override def getFailedJourneyStatus(
    journeyId: UUID
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, IvErrorStatus] = {
    val timer = metrics.ivGetFailedJourneyStatusTimer.time()
    connector
      .getFailedJourneyStatus(journeyId)
      .bimap(
        _.tap { _ =>
          timer.stop()
          metrics.ivGetFailedJourneyStatusErrorCounter.inc()
        },
        r => {
          timer.stop()
          IvErrorStatus.fromString(r.result)
        }
      )
  }

}

object IvServiceImpl {

  final case class IvStatusResponse(result: String)

  implicit val reads: Reads[IvStatusResponse] = Json.reads

}
