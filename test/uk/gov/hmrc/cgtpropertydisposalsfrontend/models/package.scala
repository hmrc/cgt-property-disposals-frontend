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

package uk.gov.hmrc.cgtpropertydisposalsfrontend

import org.scalacheck.Gen
import uk.gov.hmrc.smartstub.AutoGen

import scala.reflect._

package object models {

  def sample[A: ClassTag](gen: Gen[A]): A = gen.sample.getOrElse(sys.error(s"Could not generate instance of ${classTag[A].runtimeClass.getSimpleName}"))

  val bprGen = AutoGen[BusinessPartnerRecord]

  val addressLookupResultGen = AutoGen[AddressLookupResult]

  val subscriptionDetailsGen = AutoGen[SubscriptionDetails]

  // autogen for Boolean seems to be broken so we have to do it manually here
  val sessionGen = {
    val emailToBeVerifiedGen = for {
      emailLocal <- Gen.alphaStr
      emailDomain <- Gen.alphaStr
      uuid <- Gen.uuid
      verified <- Gen.oneOf(true, false)
    } yield EmailToBeVerified(Email(s"$emailLocal@$emailDomain"), uuid, verified)

    for {
      url <- Gen.alphaStr
      bpr <- bprGen
      emailToBeVerified <- emailToBeVerifiedGen
      addressLookupResult <- addressLookupResultGen
      subscriptionDetails <- subscriptionDetailsGen
    } yield SessionData(Some(url), Some(bpr), Some(emailToBeVerified), Some(addressLookupResult), Some(subscriptionDetails))
  }

}
