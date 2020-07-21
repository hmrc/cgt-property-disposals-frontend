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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding
import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.AddressSource.ManuallyEntered
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, SapNumber}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.ContactNameSource.DerivedFromBusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.EmailSource.BusinessPartnerRecord
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscriptionDetails, SubscriptionResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.RepresenteeCgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CGTRegistrationService @Inject() (
  val errorHandler: ErrorHandler,
  connector: CGTPropertyDisposalsConnector,
  subscriptionService: SubscriptionService
)(implicit ec: ExecutionContext)
    extends Logging {

  def extractRepresenteeCgtReference(
    representeeAnswers: CompleteRepresenteeAnswers
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, RepresenteeCgtReference] = {
    val a = for {
      sapNumber            <- connector.registerWithoutId(
                                RegistrationDetails(
                                  representeeAnswers.name,
                                  representeeAnswers.contactDetails.emailAddress,
                                  representeeAnswers.contactDetails.address,
                                  BusinessPartnerRecord
                                )
                              )
      subscriptionResponse <- subscriptionService.subscribe(
                                SubscriptionDetails(
                                  Right(representeeAnswers.name),
                                  representeeAnswers.contactDetails.emailAddress,
                                  representeeAnswers.contactDetails.address,
                                  representeeAnswers.contactDetails.contactName,
                                  SapNumber(sapNumber.body),
                                  BusinessPartnerRecord,
                                  ManuallyEntered,
                                  DerivedFromBusinessPartnerRecord
                                )
                              )
    } yield subscriptionResponse

    a.flatMap {
      case SubscriptionResponse.SubscriptionSuccessful(cgtReferenceNumber) =>
        EitherT.rightT(RepresenteeCgtReference(CgtReference(cgtReferenceNumber)))
      case SubscriptionResponse.AlreadySubscribed                          => EitherT.leftT(Error(""))
    }
  }
}
