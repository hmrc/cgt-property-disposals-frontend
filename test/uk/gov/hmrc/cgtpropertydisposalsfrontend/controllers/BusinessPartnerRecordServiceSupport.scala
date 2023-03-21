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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import cats.data.EitherT
import play.api.i18n.Lang
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecordRequest, BusinessPartnerRecordResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.BusinessPartnerRecordService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait BusinessPartnerRecordServiceSupport {
  this: ControllerSpec with SessionSupport =>

  val mockBusinessPartnerRecordService = mock[BusinessPartnerRecordService]
  def mockGetBusinessPartnerRecord(
    businessPartnerRecordRequest: BusinessPartnerRecordRequest,
    expectedBusinessPartnerRecordResponse: Either[
      Error,
      BusinessPartnerRecordResponse
    ],
    lang: Lang
  ): Unit                              =
    (mockBusinessPartnerRecordService
      .getBusinessPartnerRecord(_: BusinessPartnerRecordRequest, _: Lang)(
        _: HeaderCarrier
      ))
      .expects(businessPartnerRecordRequest, lang, *)
      .returning(
        EitherT[Future, Error, BusinessPartnerRecordResponse](
          Future.successful(
            expectedBusinessPartnerRecordResponse
          )
        )
      )

}
