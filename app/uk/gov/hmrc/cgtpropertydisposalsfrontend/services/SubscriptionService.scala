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
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, RegistrationDetails, SubscribedDetails, SubscriptionDetails, SubscriptionResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[SubscriptionServiceImpl])
trait SubscriptionService {

  def subscribe(subscriptionDetails: SubscriptionDetails)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, SubscriptionResponse]

  def registerWithoutIdAndSubscribe(registrationDetails: RegistrationDetails)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, SubscriptionResponse]

  def hasSubscription(
    )(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[CgtReference]]

  def getSubscribedDetails(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, SubscribedDetails]

  def updateSubscribedDetails(subscribedDetails: SubscribedDetails)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

}

@Singleton
class SubscriptionServiceImpl @Inject()(connector: CGTPropertyDisposalsConnector)(implicit ec: ExecutionContext)
    extends SubscriptionService {

  override def subscribe(
    subscriptionDetails: SubscriptionDetails
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, SubscriptionResponse] =
    connector
      .subscribe(subscriptionDetails)
      .subflatMap(handleSubscriptionResponse(_, "subscribe"))

  override def registerWithoutIdAndSubscribe(registrationDetails: RegistrationDetails)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, SubscriptionResponse] =
    connector
      .registerWithoutIdAndSubscribe(registrationDetails)
      .subflatMap(handleSubscriptionResponse(_, "register without id and subscribe"))

  private def handleSubscriptionResponse(
    response: HttpResponse,
    description: String
  ): Either[Error, SubscriptionResponse] =
    if (response.status === 200)
      response.parseJSON[SubscriptionResponse]().leftMap(Error(_))
    else
      Left(Error(s"call to $description came back with status ${response.status}"))

  override def hasSubscription(
    )(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[CgtReference]] =
    connector.getSubscriptionStatus().subflatMap { response =>
      if (response.status === 200)
        response.parseJSON[CgtReference]().leftMap(Error(_)).map { cgtReference =>
          Some(cgtReference)
        } else if (response.status === 204) Right(None)
      else
        Left(Error(s"call to get subscription status came back with status ${response.status}"))
    }

  override def getSubscribedDetails(cgtReference: CgtReference)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, SubscribedDetails] =
    connector.getSubscribedDetails(cgtReference).subflatMap { response =>
      if (response.status === 200)
        response.parseJSON[SubscribedDetails]().leftMap(Error(_))
      else
        Left(Error(s"Call to get subscribed details came back with status ${response.status}"))
    }

  override def updateSubscribedDetails(subscribedDetails: SubscribedDetails)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] =
    connector.updateSubscribedDetails(subscribedDetails).subflatMap { response =>
      if (response.status === 200)
        Right(())
      else
        Left(Error(s"Call to get subscribed details came back with status ${response.status}"))
    }

}
