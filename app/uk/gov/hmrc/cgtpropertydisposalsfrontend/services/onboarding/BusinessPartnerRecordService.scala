/*
 * Copyright 2022 HM Revenue & Customs
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
import cats.syntax.either._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.OK
import play.api.i18n.Lang
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.CGTPropertyDisposalsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecordRequest, BusinessPartnerRecordResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[BusinessPartnerRecordServiceImpl])
trait BusinessPartnerRecordService {

  def getBusinessPartnerRecord(request: BusinessPartnerRecordRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, BusinessPartnerRecordResponse]

}

@Singleton
class BusinessPartnerRecordServiceImpl @Inject() (
  connector: CGTPropertyDisposalsConnector
)(implicit
  ec: ExecutionContext
) extends BusinessPartnerRecordService {

  override def getBusinessPartnerRecord(request: BusinessPartnerRecordRequest, lang: Lang)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, BusinessPartnerRecordResponse] =
    connector
      .getBusinessPartnerRecord(request, lang)
      .subflatMap { response =>
        response.status match {
          case OK    =>
            response
              .parseJSON[BusinessPartnerRecordResponse]()
              .map(response =>
                response.copy(
                  businessPartnerRecord = response.businessPartnerRecord.map(bpr =>
                    bpr.copy(
                      address = bpr.address.map(sanitiseAddress),
                      name = bpr.name.bimap(sanitiseTrustName, sanitiseIndividualName),
                      emailAddress = bpr.emailAddress.filter(e => Email.emailRegex.test(e.value))
                    )
                  )
                )
              )
              .leftMap(Error.apply)
          case other =>
            Left(Error(s"Call to get BPR came back with status $other"))
        }
      }

  private def sanitiseTrustName(t: TrustName): TrustName =
    TrustName(t.value.filter(TrustName.allowedCharacters.contains(_)))

  private def sanitiseIndividualName(i: IndividualName): IndividualName =
    IndividualName(
      i.firstName.filter(IndividualName.allowedCharacters.contains(_)),
      i.lastName.filter(IndividualName.allowedCharacters.contains(_))
    )

  private def sanitiseAddress(a: Address): Address = {
    def sanitiseAddressLine(l: String): String = l.filter(Address.addressLineAllowedCharacters.contains(_))

    a match {
      case u: UkAddress =>
        UkAddress(
          sanitiseAddressLine(u.line1),
          u.line2.map(sanitiseAddressLine),
          u.town.map(sanitiseAddressLine),
          u.county.map(sanitiseAddressLine),
          u.postcode
        )

      case n: NonUkAddress =>
        NonUkAddress(
          sanitiseAddressLine(n.line1),
          n.line2.map(sanitiseAddressLine),
          n.line3.map(sanitiseAddressLine),
          n.line4.map(sanitiseAddressLine),
          n.postcode,
          n.country
        )
    }
  }

}
