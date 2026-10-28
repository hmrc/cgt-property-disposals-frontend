/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.data.EitherT
import cats.instances.future.*
import cats.instances.int.*
import cats.syntax.either.*
import cats.syntax.eq.*
import com.google.inject.{ImplementedBy, Singleton}
import play.api.http.Status.OK
import play.api.mvc.Call
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.upscan.UpscanConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.upscan.{UploadReference, UpscanUpload, UpscanUploadMeta}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData, TimeUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.HttpResponseOps.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[UpscanServiceImpl])
trait UpscanService {

  def initiate(
    errorRedirect: Call,
    successRedirect: UploadReference => Call,
    sessionData: Option[SessionData]
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, UpscanUpload]

  def getUpscanUpload(
    uploadReference: UploadReference,
    sessionData: Option[SessionData]
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
    successRedirect: UploadReference => Call,
    sessionData: Option[SessionData]
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanUpload] =
    for {
      ggCredId         <- getGGCredID(sessionData)
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
      _                <- upscanConnector.saveUpscanUpload(upscanUpload, ggCredId)
    } yield upscanUpload

  override def getUpscanUpload(
    uploadReference: UploadReference,
    sessionData: Option[SessionData]
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, UpscanUpload] =
    for {
      ggCredId <- getGGCredID(sessionData)

      upscanUpload <- upscanConnector.getUpscanUpload(uploadReference, ggCredId).subflatMap { response =>
                        if (response.status === OK) {
                          response
                            .parseJSON[UpscanUpload]()
                            .leftMap(Error(_))
                        } else {
                          Left(Error(s"call to get upscan upload failed ${response.status}"))
                        }
                      }
    } yield upscanUpload

  private def getGGCredID(sessionData: Option[SessionData]): EitherT[Future, Error, GGCredId] =
    EitherT.fromEither[Future] {
      sessionData.flatMap(_.journeyStatus) match {
        case Some(f: FillingOutReturn) => Right(f.ggCredId)
        case _                         => Left(Error("Failed to get GGCredId from the session"))
      }
    }
}
