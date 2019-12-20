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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import com.google.inject.{ImplementedBy, Singleton}
import javax.inject.Inject
import play.api.libs.json.{Json, OFormat, Reads}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.UpscanConnector

final case class Reference(value: String) extends AnyVal

object Reference {
  implicit val format: OFormat[Reference] = Json.format[Reference]
}

final case class UpscanRequest(href: String, fields: Map[String, String])
object UpscanRequest {
  implicit val format: OFormat[UpscanRequest] = Json.format[UpscanRequest]
}

final case class UpscanDescriptor(reference: Reference, upscanRequest: UpscanRequest)

object UpscanDescriptor {
  implicit val upscanDescriptorFormat: OFormat[UpscanDescriptor] = Json.format[UpscanDescriptor]
}

@ImplementedBy(classOf[UpscanServiceImpl])
trait UpscanService {
//  def initiate()(
//    implicit hc: HeaderCarrier
//  ): EitherT[Future, Error, UpscanDescriptor]
}

@Singleton
class UpscanServiceImpl @Inject()(upscanConnector: UpscanConnector) extends UpscanService {
  //override def initiate()(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanDescriptor] = {
  // for this upscan initiate call we need to store the response and then return the descriptor to the controller
//    for {
//      response   <- upscanConnector.initiate()
//      descriptor <- EitherT.pure(response.json.validate[UpscanDescriptor])
//      _          <- storeUpscanDescriptor(descriptor)
//    } yield (descriptor)
//
//    EitherT.leftT[Future, UpscanResponse](Error(""))
//  }
}
