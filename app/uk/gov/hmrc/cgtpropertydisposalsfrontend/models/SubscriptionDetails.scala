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

import cats.syntax.applicative._
import cats.syntax.apply._
import cats.data.Validated._
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.EitherFormat.eitherFormat

final case class SubscriptionDetails(
                                      contactName: Either[TrustName,Name],
                                      emailAddress: String,
                                      address: Address,
                                      sapNumber: String
)

object SubscriptionDetails {

  implicit val format: Format[SubscriptionDetails] = Json.format

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def apply(bpr: BusinessPartnerRecord,
            name: Either[Option[TrustName],Name],
            maybeEmail: Option[Email]
           ): Either[NonEmptyList[MissingData], SubscriptionDetails] = {
    val emailValidation: ValidatedNel[MissingData, String] =
      bpr.emailAddress.orElse(maybeEmail.map(_.value))
        .fold[ValidatedNel[MissingData,String]](
          Invalid(NonEmptyList.one(MissingData.Email)))(a => Valid(a))

    val trustNameValidation: ValidatedNel[MissingData, Either[TrustName, Name]] =
      name match {
      case Left(None) => Invalid(NonEmptyList.one(MissingData.TrustName))
      case Left(Some(trustName)) => Valid(Left(trustName))
      case Right(name) => Valid(Right(name))
    }

    (emailValidation, trustNameValidation).mapN{ case (email, name) =>
      SubscriptionDetails(name, email, bpr.address, bpr.sapNumber)
    }.toEither
  }

  sealed trait MissingData extends Product with Serializable

  object MissingData {

    case object Email extends MissingData

    case object TrustName extends MissingData

  }

}
