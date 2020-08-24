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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators

import org.scalacheck.{Arbitrary, Gen}

object Generators {

  implicit val booleanGen: Gen[Boolean] = Gen.oneOf(true, false)

  implicit val stringGen: Gen[String] = Gen.nonEmptyListOf(Gen.alphaUpperChar).map(_.mkString(""))

  implicit val longGen: Gen[Long] =
    Gen.choose(-5e13.toLong, 5e13.toLong)

  implicit def listGen[A](g: Gen[A]): Gen[List[A]] = Gen.listOf(g)

  def sample[A](implicit gen: Gen[A]): A =
    gen.sample.getOrElse(sys.error(s"Could not generate instance with $gen"))

  implicit def arb[A](implicit g: Gen[A]): Arbitrary[A] = Arbitrary(g)

}
