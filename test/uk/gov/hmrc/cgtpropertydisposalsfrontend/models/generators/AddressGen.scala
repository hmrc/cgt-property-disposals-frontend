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

import org.scalacheck.{Arbitrary, Gen}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Country, Postcode}

trait AddressHigherPriorityGen {
  implicit val postcodeGen: Gen[Postcode] = Generators.stringGen.map(Postcode(_))

  implicit val ukAddressGen: Gen[UkAddress] =
    for {
      line1    <- Generators.stringGen
      line2    <- Gen.option(Generators.stringGen)
      town     <- Gen.option(Generators.stringGen)
      county   <- Gen.option(Generators.stringGen)
      postcode <- postcodeGen
    } yield UkAddress(line1, line2, town, county, postcode)
}

object AddressGen extends AddressHigherPriorityGen {

  implicit val countryGen: Gen[Country] = Gen.oneOf(Country.countryCodes.map(Country(_)))

  implicit val nonUkAddressGen: Gen[NonUkAddress] = for {
    line1    <- Generators.stringGen
    line2    <- Gen.option(Generators.stringGen)
    line3    <- Gen.option(Generators.stringGen)
    line4    <- Gen.option(Generators.stringGen)
    postcode <- Gen.option(Generators.stringGen)
    country  <- countryGen
  } yield NonUkAddress(line1, line2, line3, line4, postcode, country)

  implicit val addressGen: Gen[Address] = Gen.oneOf(ukAddressGen, nonUkAddressGen)

  given addressArb: Arbitrary[Address]           = Arbitrary(AddressGen.addressGen)
  given postcodeArb: Arbitrary[Postcode]         = Arbitrary(AddressGen.postcodeGen)
  given countryArb: Arbitrary[Country]           = Arbitrary(AddressGen.countryGen)
  given ukAddressArb: Arbitrary[UkAddress]       = Arbitrary(AddressGen.ukAddressGen)
  given nonUkAddressArb: Arbitrary[NonUkAddress] = Arbitrary(AddressGen.nonUkAddressGen)
}
