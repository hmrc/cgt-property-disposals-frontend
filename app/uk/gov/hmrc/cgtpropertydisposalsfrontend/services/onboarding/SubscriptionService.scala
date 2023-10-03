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

import cats.data.{EitherT, RWS}
import cats.instances.future._
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.{CONFLICT, NO_CONTENT, OK}
import play.api.i18n.Lang
import play.api.libs.json.{Json, Reads}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.Metrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.AddressSource.{ManuallyEntered => ManuallyEnteredAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactNameSource.{ManuallyEntered => ManuallyEnteredContactName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionResponse.{AlreadySubscribed, SubscriptionSuccessful}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailSource.{ManuallyEntered => ManuallyEnteredEmail}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.RepresenteeCgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService.GetSubscriptionResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[SubscriptionServiceImpl])
trait SubscriptionService {

  def subscribe(subscriptionDetails: SubscriptionDetails, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, SubscriptionResponse]

  def registerWithoutId(registrationDetails: RegistrationDetails)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, RegisteredWithoutId]

  def hasFailedCgtEnrolment(
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[CgtReference]]

  def getSubscribedDetails(cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Option[SubscribedDetails]]

  def updateSubscribedDetails(subscribedUpdateDetails: SubscribedUpdateDetails)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

  def registerWithoutIdAndSubscribe(completeRepresenteeAnswers: CompleteRepresenteeAnswers, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, RepresenteeCgtReference]

}

@Singleton
class SubscriptionServiceImpl @Inject() (
  connector: CGTPropertyDisposalsConnector,
  metrics: Metrics
)(implicit
  ec: ExecutionContext
) extends SubscriptionService {

  override def subscribe(
    subscriptionDetails: SubscriptionDetails,
    lang: Lang
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, SubscriptionResponse] =
    connector.subscribe(subscriptionDetails, lang).subflatMap { response =>
      response.status match {
        case OK       => response.parseJSON[SubscriptionSuccessful]().leftMap(Error(_))
        case CONFLICT =>
          metrics.accessWithWrongGGAccountCounter.inc()
          Right(AlreadySubscribed)
        case _        => Left(Error(s"call to subscribe came back with status ${response.status}"))
      }
    }

  override def registerWithoutId(registrationDetails: RegistrationDetails)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, RegisteredWithoutId] =
    connector.registerWithoutId(registrationDetails).subflatMap { response =>
      response.status match {
        case OK     => response.parseJSON[RegisteredWithoutId]().leftMap(Error(_))
        case status => Left(Error(s"Call to register without id came back with status $status"))
      }
    }

  override def hasFailedCgtEnrolment(
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[CgtReference]] =
    connector.getSubscriptionStatus().subflatMap { response =>
      response.status match {
        case OK         =>
          response
            .parseJSON[CgtReference]()
            .leftMap(Error(_))
            .map(cgtReference => Some(cgtReference))
        case NO_CONTENT => Right(None)
        case status     => Left(Error(s"call to get subscription status came back with status $status"))
      }
    }

  override def getSubscribedDetails(cgtReference: CgtReference)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Option[SubscribedDetails]] =
    connector.getSubscribedDetails(cgtReference).subflatMap { response =>
      response.status match {
        case OK     => response.parseJSON[GetSubscriptionResponse]().bimap(Error(_), _.subscribedDetails)
        case status => Left(Error(s"Call to get subscribed details came back with status $status"))
      }
    }

  override def updateSubscribedDetails(
    subscribedUpdateDetails: SubscribedUpdateDetails
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] =
    connector.updateSubscribedDetails(subscribedUpdateDetails).map(_.status).subflatMap {
      case OK     => Right(())
      case status => Left(Error(s"Call to get subscribed details came back with status $status"))
    }

  override def registerWithoutIdAndSubscribe(
    representeeAnswers: CompleteRepresenteeAnswers,
    lang: Lang
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, RepresenteeCgtReference] = {
    val result = for {
      sapNumber            <- registerWithoutId(
                                RegistrationDetails(
                                  representeeAnswers.name,
                                  representeeAnswers.contactDetails.emailAddress,
                                  representeeAnswers.contactDetails.address,
                                  ManuallyEnteredEmail
                                )
                              )
      subscriptionResponse <- subscribe(
                                SubscriptionDetails(
                                  Right(representeeAnswers.name),
                                  representeeAnswers.contactDetails.emailAddress,
                                  representeeAnswers.contactDetails.address,
                                  representeeAnswers.contactDetails.contactName,
                                  sapNumber.sapNumber,
                                  ManuallyEnteredEmail,
                                  ManuallyEnteredAddress,
                                  ManuallyEnteredContactName
                                ),
                                lang
                              )
    } yield subscriptionResponse

    result.flatMap {
      case SubscriptionResponse.SubscriptionSuccessful(cgtReferenceNumber) =>
        EitherT.rightT(RepresenteeCgtReference(CgtReference(cgtReferenceNumber)))
      case SubscriptionResponse.AlreadySubscribed                          => EitherT.leftT(Error("User is already subscribed"))
    }
  }

}

object SubscriptionService {

  final case class GetSubscriptionResponse(
    subscribedDetails: Option[SubscribedDetails]
  )

  object GetSubscriptionResponse {

    implicit val reads: Reads[GetSubscriptionResponse] =
      Json.reads[GetSubscriptionResponse]

  }

}
