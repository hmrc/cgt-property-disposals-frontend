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

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{EnrolmentRequest, Error}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.http.HttpClient

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[TaxEnrolmentConnectorImpl])
trait TaxEnrolmentConnector {
  def allocateEnrolmentToGroup(cgtReference: String, enrolmentRequest: EnrolmentRequest)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]
}

@Singleton
class TaxEnrolmentConnectorImpl @Inject()(http: HttpClient, servicesConfig: ServicesConfig)(
  implicit ec: ExecutionContext
) extends TaxEnrolmentConnector {

  val serviceUrl: String = servicesConfig.baseUrl("tax-enrolments")

  override def allocateEnrolmentToGroup(cgtReference: String, enrolmentRequest: EnrolmentRequest)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = {

    val serviceName: String  = "HMRC-CGT-PD"
    val enrolmentUrl: String = s"$serviceUrl/tax-enrolments/enrolments/$serviceName~$cgtReference"
    EitherT[Future, Error, HttpResponse](
      http
        .PUT[EnrolmentRequest, HttpResponse](enrolmentUrl, enrolmentRequest)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
  }
}
