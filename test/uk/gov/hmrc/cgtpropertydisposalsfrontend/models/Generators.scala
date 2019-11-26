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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators.gen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualMissingEmail, IndividualSupplyingInformation, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionResponse.SubscriptionSuccessful
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails.{IndividualNameMatchDetails, TrustNameMatchDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.{BusinessPartnerRecord, BusinessPartnerRecordRequest, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName, TrustName}

import scala.reflect.{ClassTag, classTag}

object Generators
    extends WithGenGenerator
    with SessionDataGen
    with BusinessPartnerRecordGen
    with IdGen
    with NameGen
    with JourneyStatusGen
    with AddressGen
    with NameMatchGen
    with OnboardingDetailsGen
    with EmailGen {

  def sample[A: ClassTag](implicit gen: Gen[A]): A =
    gen.sample.getOrElse(sys.error(s"Could not generate instance of ${classTag[A].runtimeClass.getSimpleName}"))

  implicit def arb[A](implicit g: Gen[A]): Arbitrary[A] = Arbitrary(g)


}

sealed trait WithGenGenerator {

  def gen[A](implicit arb: Arbitrary[A]): Gen[A] = arb.arbitrary

}

trait SessionDataGen { this: WithGenGenerator =>

  implicit val sessionDataGen: Gen[SessionData] = gen[SessionData]

}

trait BusinessPartnerRecordGen { this: WithGenGenerator =>

  implicit val bprArb: Gen[BusinessPartnerRecord] = gen[BusinessPartnerRecord]

  implicit val bprRequestArb: Gen[BusinessPartnerRecordRequest] = gen[BusinessPartnerRecordRequest]

}

trait IdGen { this: WithGenGenerator =>

  implicit val cgtReferenceArb: Gen[CgtReference] = gen[CgtReference]

  implicit val ggCredIdGen: Gen[GGCredId] = gen[GGCredId]

}

trait NameGen { this: WithGenGenerator =>

  implicit val contactNameGen: Gen[ContactName] = gen[ContactName]

  implicit val individualNameGen: Gen[IndividualName] = gen[IndividualName]

  implicit val trustNameGen: Gen[TrustName] = gen[TrustName]

}

trait JourneyStatusGen { this: WithGenGenerator =>

  implicit val journeyStatusGen: Gen[JourneyStatus] = gen[JourneyStatus]

  implicit val registrationReadyGen: Gen[RegistrationReady] = gen[RegistrationReady]

  implicit val subscriptionReadyGen: Gen[SubscriptionReady] = gen[SubscriptionReady]

  implicit val subscriptionSuccessfulGen: Gen[SubscriptionSuccessful] = gen[SubscriptionSuccessful]

  implicit val subscribedGen: Gen[Subscribed] = gen[Subscribed]

  implicit val individualSupplyingInformationGen: Gen[IndividualSupplyingInformation] =
    gen[IndividualSupplyingInformation]

  implicit val individualMissingEmailGen: Gen[IndividualMissingEmail] = gen[IndividualMissingEmail]

}

trait AddressGen { this: WithGenGenerator =>

  implicit val addressGen: Gen[Address] = gen[Address]

  implicit val ukAddressGen: Gen[UkAddress] = gen[UkAddress]

}

trait NameMatchGen { this: WithGenGenerator =>

  implicit val trustNameMatchDetailsGen: Gen[TrustNameMatchDetails] = gen[TrustNameMatchDetails]

  implicit val individualNameMatchDetailsGen: Gen[IndividualNameMatchDetails] = gen[IndividualNameMatchDetails]

  implicit val individualUnsuccessfulNameMatchAttemptsGen
    : Gen[UnsuccessfulNameMatchAttempts[IndividualNameMatchDetails]] =
    gen[UnsuccessfulNameMatchAttempts[IndividualNameMatchDetails]]

}

trait OnboardingDetailsGen { this: WithGenGenerator =>

  implicit val registrationDetailsArb: Gen[RegistrationDetails] = gen[RegistrationDetails]

  implicit val subscriptionDetailsArb: Gen[SubscriptionDetails] = gen[SubscriptionDetails]

  implicit val subscribedDetailsGen: Gen[SubscribedDetails] = gen[SubscribedDetails]

  implicit val subscribedUpdateDetailsGen: Gen[SubscribedUpdateDetails] = gen[SubscribedUpdateDetails]

}

trait EmailGen { this: WithGenGenerator =>

  implicit val emailGen: Gen[Email] = gen[Email]

  implicit val emailSourceGen: Gen[EmailSource] = gen[EmailSource]
}
