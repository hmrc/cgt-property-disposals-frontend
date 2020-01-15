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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.{IndividualMissingEmail, IndividualSupplyingInformation, RegistrationReady}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.agents.UnsuccessfulVerifierAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{AgentReferenceNumber, CgtReference, GGCredId, SAUTR, SapNumber}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionResponse.SubscriptionSuccessful
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails.{IndividualNameMatchDetails, TrustNameMatchDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecord, BusinessPartnerRecordRequest, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.{Email, EmailSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscribedDetails, SubscribedUpdateDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{IndividualTriageAnswers, IndividualUserType}

object Generators
    extends GenUtils
    with SessionDataGen
    with BusinessPartnerRecordGen
    with IdGen
    with NameGen
    with JourneyStatusGen
    with AddressGen
    with NameMatchGen
    with OnboardingDetailsGen
    with EmailGen
    with VerifierMatchGen
    with UserTypeGen
    with TriageQuestionsGen {

  def sample[A](implicit gen: Gen[A]): A =
    gen.sample.getOrElse(sys.error(s"Could not generate instance with $gen"))

  implicit def arb[A](implicit g: Gen[A]): Arbitrary[A] = Arbitrary(g)

}

sealed trait GenUtils {

  def gen[A](implicit arb: Arbitrary[A]): Gen[A] = arb.arbitrary

  // define our own Arbitrary instance for String to generate more legible strings
  implicit val stringArb: Arbitrary[String] = Arbitrary(Gen.alphaNumStr)

}

trait SessionDataGen { this: GenUtils =>

  implicit val sessionDataGen: Gen[SessionData] = gen[SessionData]

}

trait BusinessPartnerRecordGen { this: GenUtils =>

  implicit val bprArb: Gen[BusinessPartnerRecord] = gen[BusinessPartnerRecord]

  implicit val bprRequestArb: Gen[BusinessPartnerRecordRequest] = gen[BusinessPartnerRecordRequest]

}

trait IdGen { this: GenUtils =>

  implicit val cgtReferenceArb: Gen[CgtReference] = gen[CgtReference]

  implicit val ggCredIdGen: Gen[GGCredId] = gen[GGCredId]

  implicit val sautrGen: Gen[SAUTR] = gen[SAUTR]

  implicit val sapNumberGen: Gen[SapNumber] = gen[SapNumber]

  implicit val arnGen: Gen[AgentReferenceNumber] = gen[AgentReferenceNumber]

}

trait NameGen { this: GenUtils =>

  implicit val contactNameGen: Gen[ContactName] = gen[ContactName]

  implicit val individualNameGen: Gen[IndividualName] = gen[IndividualName]

  implicit val trustNameGen: Gen[TrustName] = gen[TrustName]

}

trait JourneyStatusGen extends JourneyStatusLowerPriorityGen { this: GenUtils =>

  implicit val journeyStatusGen: Gen[JourneyStatus] = gen[JourneyStatus]

  implicit val subscriptionReadyGen: Gen[SubscriptionReady] = gen[SubscriptionReady]

}

trait JourneyStatusLowerPriorityGen { this: GenUtils =>

  implicit val subscriptionSuccessfulGen: Gen[SubscriptionSuccessful] = gen[SubscriptionSuccessful]

  implicit val individualSupplyingInformationGen: Gen[IndividualSupplyingInformation] =
    gen[IndividualSupplyingInformation]

  implicit val subscribedGen: Gen[Subscribed] = gen[Subscribed]

  implicit val individualMissingEmailGen: Gen[IndividualMissingEmail] = gen[IndividualMissingEmail]

  implicit val registrationReadyGen: Gen[RegistrationReady] = gen[RegistrationReady]

}

trait AddressGen extends AddressLowerPriorityGen { this: GenUtils =>

  implicit val addressGen: Gen[Address] = gen[Address]

  implicit val nonUkAddressGen: Gen[NonUkAddress] = gen[NonUkAddress]

  implicit val postcodeGen: Gen[Postcode] = gen[Postcode]

}

trait AddressLowerPriorityGen { this: GenUtils =>

  implicit val ukAddressGen: Gen[UkAddress] = gen[UkAddress]

}

trait NameMatchGen { this: GenUtils =>

  implicit val trustNameMatchDetailsGen: Gen[TrustNameMatchDetails] = gen[TrustNameMatchDetails]

  implicit val individualNameMatchDetailsGen: Gen[IndividualNameMatchDetails] = gen[IndividualNameMatchDetails]

  implicit val individualUnsuccessfulNameMatchAttemptsGen
    : Gen[UnsuccessfulNameMatchAttempts[IndividualNameMatchDetails]] =
    gen[UnsuccessfulNameMatchAttempts[IndividualNameMatchDetails]]

}

trait OnboardingDetailsGen { this: GenUtils =>

  implicit val registrationDetailsArb: Gen[RegistrationDetails] = gen[RegistrationDetails]

  implicit val subscriptionDetailsArb: Gen[SubscriptionDetails] = gen[SubscriptionDetails]

  implicit val subscribedDetailsGen: Gen[SubscribedDetails] = gen[SubscribedDetails]

  implicit val subscribedUpdateDetailsGen: Gen[SubscribedUpdateDetails] = gen[SubscribedUpdateDetails]

}

trait EmailGen { this: GenUtils =>

  implicit val emailGen: Gen[Email] = gen[Email]

  implicit val emailSourceGen: Gen[EmailSource] = gen[EmailSource]
}

trait VerifierMatchGen { this: GenUtils =>

  implicit val unsuccessfulVerifierMatchAttemptsGen: Gen[UnsuccessfulVerifierAttempts] =
    gen[UnsuccessfulVerifierAttempts]

}

trait UserTypeGen { this: GenUtils =>

  implicit val userTypeGen: Gen[UserType] = gen[UserType]

}

trait TriageQuestionsGen { this: GenUtils =>

  implicit val individualTriageAnswersGen: Gen[IndividualTriageAnswers] = gen[IndividualTriageAnswers]

  implicit val individualUserTypeGen: Gen[IndividualUserType] = gen[IndividualUserType]

}
