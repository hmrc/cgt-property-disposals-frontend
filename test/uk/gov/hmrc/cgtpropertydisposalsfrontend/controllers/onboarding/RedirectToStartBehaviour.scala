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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding

import org.scalacheck.Arbitrary
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck
import play.api.mvc.Result
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.arb
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen.journeyStatusGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{JourneyStatus, SessionData}

import java.util.UUID
import scala.concurrent.Future

trait RedirectToStartBehaviour {
  this: ControllerSpec & AuthSupport & SessionSupport & SampledScalaCheck =>

  def redirectToStartWhenInvalidJourney(
    performAction: () => Future[Result],
    isValidJourneyStatus: JourneyStatus => Boolean
  ): Unit =
    "redirect to the start endpoint" when {

      "there is no journey status in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(SessionData.empty)
        }

        checkIsRedirect(
          performAction(),
          controllers.routes.StartController.start()
        )
      }
      "the journey status in session is not valid" in {
        implicit val journeyStatusArb: Arbitrary[JourneyStatus] =
          arb(using journeyStatusGen)

        forAll { (j: JourneyStatus) =>
          val fixture = j match {
            case f: FillingOutReturn =>
              f.copy(
                draftReturn = DraftSingleDisposalReturn
                  .newDraftReturn(UUID.randomUUID(), IncompleteSingleDisposalTriageAnswers.empty, None)
              )
            case x: JourneyStatus    => x
          }

          whenever(!isValidJourneyStatus(fixture)) {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(SessionData.empty.copy(journeyStatus = Some(fixture)))
            }

            checkIsRedirect(
              performAction(),
              controllers.routes.StartController.start()
            )
          }
        }
      }

    }

}
