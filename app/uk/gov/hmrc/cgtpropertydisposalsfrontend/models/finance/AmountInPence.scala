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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance

import cats.Order
import cats.instances.long._
import cats.syntax.order._
import play.api.libs.functional.syntax._
import play.api.libs.json.Format

final case class AmountInPence(value: Long)

object AmountInPence {

  val zero: AmountInPence = AmountInPence(0L)

  def fromPounds(amount: BigDecimal): AmountInPence =
    AmountInPence((amount * BigDecimal("100")).toLong)

  implicit class AmountInPenceOps(private val a: AmountInPence) extends AnyVal {
    def inPounds(): BigDecimal = a.value / BigDecimal("100")

    def ++(other: AmountInPence): AmountInPence =
      AmountInPence(a.value + other.value)

    def --(other: AmountInPence): AmountInPence =
      AmountInPence(a.value - other.value)

    def withFloorZero: AmountInPence =
      if (a.value < 0L) AmountInPence.zero else a

    def withCeilingZero: AmountInPence =
      if (a.value > 0L) AmountInPence.zero else a

    def isNegative: Boolean = a < AmountInPence.zero

    def isPositive: Boolean = a > AmountInPence.zero

    def isZero: Boolean = a.value === 0L

    def abs(): AmountInPence = if (a.isNegative) AmountInPence(-a.value) else a

  }

  implicit class ListOfAmountInPenceOps(private val l: List[AmountInPence]) extends AnyVal {
    def total(): AmountInPence = AmountInPence(l.map(_.value).sum)
  }

  implicit val order: Order[AmountInPence] =
    Order.by[AmountInPence, Long](_.value)

  implicit val format: Format[AmountInPence] =
    implicitly[Format[Long]].inmap(AmountInPence(_), _.value)

}
