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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import scala.concurrent.Future

trait SubscriptionServiceSupport {
  this: ControllerSpec with SessionSupport =>

  val mockSubscriptionService = mock[SubscriptionService]
  def mockGetSubscriptionDetails(): Unit =
    (mockSubscriptionService
      .getSubscribedDetails(_: CgtReference)(_: HeaderCarrier))
      .expects(*, *)
      .returning(
        EitherT[Future, Error, SubscribedDetails](
          Future.successful(
            Right(
              SubscribedDetails(
                Right(IndividualName("First", "Last")),
                Email("email"),
                UkAddress("Some St.", None, None, None, Postcode("EC1 AB2")),
                ContactName(""),
                CgtReference("cgtReference"),
                None,
                false
              )
            )
          )
        )
      )
}
