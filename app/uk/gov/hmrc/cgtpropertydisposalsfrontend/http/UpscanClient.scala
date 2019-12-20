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

///*
// * Copyright 2019 HM Revenue & Customs
// *
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *     http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
//package uk.gov.hmrc.cgtpropertydisposalsfrontend.http
//
//import cats.data.EitherT
//import com.google.inject.{ImplementedBy, Inject, Singleton}
//import configs.Configs
//import configs.syntax._
//import play.api.Configuration
//import play.api.libs.json.{JsError, Json}
//import play.api.mvc.Call
//import uk.gov.hmrc.cgtpropertydisposalsfrontend.http.HttpClient._
//import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
//import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
//import uk.gov.hmrc.http.HeaderCarrier
//import uk.gov.hmrc.play.audit.http.connector.AuditConnector
//import uk.gov.hmrc.play.bootstrap.http.HttpClient
//
//import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.Future
//
//final case class UploadRequest(href: String, fields: Map[String, String])
//final case class UploadDescriptor(reference: String, uploadRequest: UploadRequest)
//object UploadDescriptor {
//  implicit val format = Json.format[UploadDescriptor]
//}
//
//@ImplementedBy(classOf[UpscanClientImpl])
//trait UpscanClient {
//
//  def initiate(callBack: Call, minFileSize: Long, maxFileSize: Long)(
//    implicit hc: HeaderCarrier
//  ): EitherT[Future, Error, UploadDescriptor]
//
////  def upload(href: String, form: MultipartFormData[Source[ByteString, _]])(
////    implicit hc: HeaderCarrier
////  ): EitherT[Future, Error, Unit]
//
//}
//
//@Singleton
//class UpscanClientImpl @Inject()(auditConnector: AuditConnector, config: Configuration, http: HttpClient)
//    extends UpscanClient
//    with Logging {
//
//  private def getUpscanInitiateConfig[A: Configs](key: String): A =
//    config.underlying
//      .get[A](s"microservice.services.upscan-initiate.$key")
//      .value
//
//  val url: String = {
//    val protocol = getUpscanInitiateConfig[String]("protocol")
//    val host     = getUpscanInitiateConfig[String]("host")
//    val port     = getUpscanInitiateConfig[String]("port")
//    s"$protocol://$host:$port/upscan/initiate"
//  }
//
//  override def initiate(callBack: Call, minFileSize: Long, maxFileSize: Long)(
//    implicit hc: HeaderCarrier
//  ): EitherT[Future, Error, UploadDescriptor] = {
//
//    val payload = Json.obj(
//      "callbackUrl"     -> callBack.url,
//      "minimumFileSize" -> minFileSize,
//      "maximumFileSize" -> maxFileSize
//    )
//
//    EitherT[Future, Error, UploadDescriptor](
//      http
//        .post(url, payload)
//        .map(response => Right(response.json.validate[UploadDescriptor]))
//    )
//
//  }
//
////  override def upload(href: String, form: MultipartFormData[Source[ByteString, _]])(
////    implicit hc: HeaderCarrier
////  ): EitherT[Future, Error, Unit] = {
////    val parts: Source[MultipartFormData.Part[Source[ByteString, _]], _] = Source.apply(form.dataParts.flatMap {
////      case (key, values) =>
////        values.map(value => MultipartFormData.DataPart(key, value): MultipartFormData.Part[Source[ByteString, _]])
////    } ++ form.files)
////    EitherT.pure(())
////  }
//}
//
//class UpscanException(message: String) extends Exception(message)
//
//object UpscanException {
//  def byJsError(errors: JsError) =
//    new UpscanException("Unable to parse response - %s".format(JsError.toJson(errors).toString()))
//}
