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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors

import java.time.LocalDate

import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.Json
import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Address, BusinessPartnerRecord, DateOfBirth, NINO}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[BusinessPartnerRecordConnectorImpl])
trait BusinessPartnerRecordConnector {

  def getBusinessPartnerRecord(nino: NINO)(implicit hc: HeaderCarrier): Future[HttpResponse]

}

@Singleton
class BusinessPartnerRecordConnectorImpl @Inject() (
    http: HttpClient,
    servicesConfig: ServicesConfig
)(implicit ec: ExecutionContext) extends BusinessPartnerRecordConnector {

  val baseUrl: String = servicesConfig.baseUrl("business-partner-record")

  def url(nino: NINO): String = s"$baseUrl/${nino.value}/business-partner-record"

  def getBusinessPartnerRecord(nino: NINO)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    val response = BusinessPartnerRecord("Tinky", "Winky", DateOfBirth(LocalDate.of(1900, 1, 1)), Some("tubbies4eva@email.com"),
                                                           Address.UkAddress("Over the hill", Some("and faraway"), None, None, "PO1 L414"))
    Future.successful(HttpResponse(200, Some(Json.toJson(response))))
    // http.get(url(nino))
  }

}
