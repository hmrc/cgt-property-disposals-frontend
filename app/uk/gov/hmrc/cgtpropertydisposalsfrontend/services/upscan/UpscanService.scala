/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.upscan

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Singleton}
import javax.inject.Inject
import play.api.http.Status.OK
import play.api.mvc.Call
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.upscan.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UploadReference, UpscanUpload, UpscanUploadMeta}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TimeUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[UpscanServiceImpl])
trait UpscanService {

  def initiate(
    errorRedirect: Call,
    successRedirect: UploadReference => Call
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, UpscanUpload]

  def getUpscanUpload(
    uploadReference: UploadReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanUpload]

}

@Singleton
class UpscanServiceImpl @Inject() (
  upscanConnector: UpscanConnector
)(implicit ec: ExecutionContext)
    extends UpscanService
    with Logging {

  override def initiate(
    errorRedirect: Call,
    successRedirect: UploadReference => Call
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanUpload] =
    for {
      uploadReference  <- EitherT.pure(UploadReference(UUID.randomUUID().toString))
      httpResponse     <- upscanConnector
                            .initiate(
                              errorRedirect,
                              successRedirect(uploadReference),
                              uploadReference
                            )
      upscanUploadMeta <- EitherT.fromOption(
                            httpResponse.json.validate[UpscanUploadMeta].asOpt,
                            Error("could not parse upscan initiate response")
                          )
      upscanUpload     <- EitherT.pure(
                            UpscanUpload(uploadReference, upscanUploadMeta, TimeUtils.now(), None)
                          )
      _                <- upscanConnector.saveUpscanUpload(upscanUpload)
    } yield upscanUpload

  override def getUpscanUpload(
    uploadReference: UploadReference
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanUpload] =
    upscanConnector.getUpscanUpload(uploadReference).subflatMap { response =>
      if (response.status === OK)
        response
          .parseJSON[UpscanUpload]()
          .leftMap(Error(_))
      else
        Left(Error(s"call to get upscan upload failed ${response.status}"))
    }

}
