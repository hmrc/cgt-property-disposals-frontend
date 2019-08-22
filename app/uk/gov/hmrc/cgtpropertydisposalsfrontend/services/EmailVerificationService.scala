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

import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.{CONFLICT, CREATED}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.EmailVerificationConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Email, Error}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.EmailVerificationService.EmailVerificationResponse.{EmailAlreadyVerified, EmailVerificationRequested}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[EmailVerificationServiceImpl])
trait EmailVerificationService {

  def verifyEmail(email: Email, id: UUID, name: String)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, EmailVerificationResponse]

}

object EmailVerificationService {

  sealed trait EmailVerificationResponse

  object EmailVerificationResponse {

    case object EmailVerificationRequested extends EmailVerificationResponse

    case object EmailAlreadyVerified extends EmailVerificationResponse

  }

}

@Singleton
class EmailVerificationServiceImpl @Inject()(connector: EmailVerificationConnector)(implicit ec: ExecutionContext)
    extends EmailVerificationService {

  def verifyEmail(email: Email, id: UUID, name: String)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, EmailVerificationResponse] =
    connector.verifyEmail(email, id, name).subflatMap { response =>
      response.status match {
        case CREATED =>
          Right(EmailVerificationRequested)

        case CONFLICT =>
          Right(EmailAlreadyVerified)

        case other =>
          Left(Error(s"Call to verify email came back with status $other"))
      }
    }

}
