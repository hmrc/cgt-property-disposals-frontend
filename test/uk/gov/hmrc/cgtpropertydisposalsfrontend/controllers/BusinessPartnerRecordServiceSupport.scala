/*
 * Copyright 2020 HM Revenue & Customs
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.SapNumber
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecord, BusinessPartnerRecordRequest, BusinessPartnerRecordResponse}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.BusinessPartnerRecordService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error

import scala.concurrent.{ExecutionContext, Future}

trait BusinessPartnerRecordServiceSupport {
  this: ControllerSpec with SessionSupport =>

  val mockBusinessPartnerRecordService = mock[BusinessPartnerRecordService]
  def mockGetBusinessPartnerRecord(): Unit =
    (mockBusinessPartnerRecordService
      .getBusinessPartnerRecord(_: BusinessPartnerRecordRequest)(_: HeaderCarrier))
      .expects(*, *)
      .returning(
        EitherT[Future, Error, BusinessPartnerRecordResponse](
          Future.successful(
            Right(
              BusinessPartnerRecordResponse(
                Some(
                  BusinessPartnerRecord(
                    None,
                    UkAddress("Some St.", None, None, None, Postcode("EC1 AB2")),
                    SapNumber("123"),
                    Right(IndividualName("First", "Last"))
                  )
                ),
                None
              )
            )
          )
        )
      )

}
