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

import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}

import scala.reflect._

package object models {

  def sample[A: ClassTag](implicit gen: Arbitrary[A]): A =
    gen.arbitrary.sample.getOrElse(sys.error(s"Could not generate instance of ${classTag[A].runtimeClass.getSimpleName}"))


//  implicit val subscriptionDetailsArb: Arbitrary[SubscriptionDetails] = gen[SubscriptionDetails]
//
//  implicit val sessionDataArb: Arbitrary[SessionData] = gen[SessionData]
//
//  implicit val bprArb: Arbitrary[BusinessPartnerRecord] = gen[BusinessPartnerRecord]
//
//  implicit val nameArb: Arbitrary[Name] = gen[Name]
//
//  implicit val subscriptionResponseArb: Arbitrary[SubscriptionResponse] = gen[SubscriptionResponse]

}
