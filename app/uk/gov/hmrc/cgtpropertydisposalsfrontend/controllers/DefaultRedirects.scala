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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import play.api.mvc.Result
import play.api.mvc.Results.SeeOther
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.SubscriptionStatus._

trait DefaultRedirects {

  def defaultRedirect(subscriptionStatus: Option[SubscriptionStatus]): Result = {
    val redirectTo =
      subscriptionStatus match {
        case Some(_: SubscriptionMissingData)  => routes.StartController.start()
        case Some(_: SubscriptionReady)        => routes.SubscriptionController.checkYourDetails()
        case Some(_: SubscriptionComplete)     => routes.SubscriptionController.subscribed()
        case Some(_: IndividualInsufficientConfidenceLevel) => routes.InsufficientConfidenceLevelController.doYouHaveNINO()
        case Some(OrganisationUnregisteredTrust)  => routes.RegisterTrustController.registerYourTrust()
        case None                              => routes.StartController.start()
      }

    SeeOther(redirectTo.url)
  }

}
