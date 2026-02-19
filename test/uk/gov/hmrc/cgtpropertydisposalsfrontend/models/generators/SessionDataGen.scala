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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators

import org.scalacheck.Gen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.AddressLookupResult
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailToBeVerified
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.NeedMoreDetailsDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.NeedMoreDetailsDetails.AffinityGroup

object SessionDataGen extends GenUtils {

  private val emailToBeVerifiedGen: Gen[EmailToBeVerified] = for {
    email                      <- EmailGen.emailGen
    id                         <- Gen.uuid
    verified                   <- Generators.booleanGen
    hasResentVerificationEmail <- Generators.booleanGen
  } yield EmailToBeVerified(email, id, verified, hasResentVerificationEmail)

  private val addressLookupResultGen: Gen[AddressLookupResult] = for {
    postcode  <- AddressGen.postcodeGen
    filter    <- Gen.option(Generators.stringGen)
    addresses <- Generators.listOfMax(3, AddressGen.addressGen)
  } yield AddressLookupResult(postcode, filter, addresses)

  private val needMoreDetailsDetailsGen: Gen[NeedMoreDetailsDetails] = for {
    continueUrl   <- Generators.stringGen
    affinityGroup <- Gen.oneOf(AffinityGroup.Individual, AffinityGroup.Organisation)
  } yield NeedMoreDetailsDetails(continueUrl, affinityGroup)

  private val userTypeGen: Gen[UserType] =
    Gen.oneOf(
      UserType.Individual,
      UserType.Organisation,
      UserType.NonGovernmentGatewayUser,
      UserType.Agent
    )

  private val journeyTypeGen: Gen[JourneyType] =
    Gen.oneOf(OnBoarding, Returns, Amend)

  implicit val sessionDataGen: Gen[SessionData] =
    for {
      journeyStatus          <- Gen.option(JourneyStatusGen.journeyStatusGen)
      emailToBeVerified      <- Gen.option(emailToBeVerifiedGen)
      addressLookupResult    <- Gen.option(addressLookupResultGen)
      needMoreDetailsDetails <- Gen.option(needMoreDetailsDetailsGen)
      userType               <- Gen.option(userTypeGen)
      journeyType            <- Gen.option(journeyTypeGen)
    } yield SessionData(
      journeyStatus,
      emailToBeVerified,
      addressLookupResult,
      needMoreDetailsDetails,
      userType,
      journeyType
    )

}
