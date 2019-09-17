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

import play.api.mvc.{Call, Result}
import play.api.mvc.Results.SeeOther
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{RegistrationStatus, SubscriptionStatus}

trait DefaultRedirects {

  def defaultRedirect(subscriptionStatus: Option[JourneyStatus]): Result = {
    val redirectTo =
      subscriptionStatus match {
        case Some(s: SubscriptionStatus)  => subscriptionDefaults(s)
        case Some(r: RegistrationStatus) => registrationDefaults(r)
        case None                              => routes.StartController.start()
      }

    SeeOther(redirectTo.url)
  }

  private def subscriptionDefaults(subscriptionStatus: SubscriptionStatus): Call = {
    import SubscriptionStatus._
    subscriptionStatus match {
    case _: SubscriptionMissingData  => routes.StartController.start()
    case _: SubscriptionReady        => routes.SubscriptionController.checkYourDetails()
    case _: SubscriptionComplete     => routes.SubscriptionController.subscribed()
    case _: IndividualWithInsufficientConfidenceLevel => routes.InsufficientConfidenceLevelController.doYouHaveNINO()
    case UnregisteredTrust  => routes.RegisterTrustController.registerYourTrust()
    }
  }

  private def registrationDefaults(registrationStatus: RegistrationStatus): Call = {
    registrationStatus match {
      case _ => routes.RegistrationController.startRegistration()
    }
  }


}
