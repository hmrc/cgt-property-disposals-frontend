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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import cats.syntax.eq._
import cats.instances.int._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SubscriptionDetails, SubscriptionResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[SubscriptionServiceImpl])
trait SubscriptionService {

  def subscribe(subscriptionDetails: SubscriptionDetails)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, SubscriptionResponse]

}

@Singleton
class SubscriptionServiceImpl @Inject()(connector: CGTPropertyDisposalsConnector)(implicit ec: ExecutionContext)
    extends SubscriptionService {

  override def subscribe(
    subscriptionDetails: SubscriptionDetails
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, SubscriptionResponse] =
    connector.subscribe(subscriptionDetails).subflatMap { response =>
      if (response.status === 200)
        response.parseJSON[SubscriptionResponse]().leftMap(Error(_))
      else
        Left(Error(s"call to subscribe came back with status ${response.status}"))
    }

}
