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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import org.scalatest.{Matchers, WordSpec}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.IncompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DraftMultipleDisposalsReturn, DraftReturn, DraftSingleDisposalReturn, DraftSingleIndirectDisposalReturn, ReturnSummary}

class JourneyStatusSpec extends WordSpec with Matchers {

  "FillingOutReturn" must {

    "have a method" which {

      "tries to say when a draft is return is a further return" when {

        def fillingOutReturn(
          name: Either[TrustName, IndividualName],
          previousSentReturns: Option[PreviousReturnData],
          draftReturn: DraftReturn
        ): FillingOutReturn =
          FillingOutReturn(
            sample[SubscribedDetails].copy(name = name),
            sample[GGCredId],
            None,
            draftReturn,
            previousSentReturns,
            None
          )

        "the user is a trust and" when {

          "the user has no previously sent returns" in {
            fillingOutReturn(
              Left(sample[TrustName]),
              None,
              sample[DraftReturn]
            ).isFurtherOrAmendReturn shouldBe Some(false)

            fillingOutReturn(
              Left(sample[TrustName]),
              Some(PreviousReturnData(List.empty, None)),
              sample[DraftReturn]
            ).isFurtherOrAmendReturn shouldBe Some(false)
          }

          "the user has previously sent returns" in {
            fillingOutReturn(
              Left(sample[TrustName]),
              Some(PreviousReturnData(List(sample[ReturnSummary]), None)),
              sample[DraftReturn]
            ).isFurtherOrAmendReturn shouldBe Some(true)
          }

        }

        "the user is not a trust and" when {

          "the user has not said who they are completing the return for" in {
            fillingOutReturn(
              Right(sample[IndividualName]),
              Some(PreviousReturnData(List(sample[ReturnSummary]), None)),
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = IncompleteSingleDisposalTriageAnswers.empty
              )
            ).isFurtherOrAmendReturn shouldBe None
          }

          "the user is completing the return for themselves and" when {

            val draftReturn =
              sample[DraftMultipleDisposalsReturn].copy(
                triageAnswers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                  individualUserType = Some(Self)
                )
              )

            "the user has no previously sent returns" in {
              fillingOutReturn(
                Right(sample[IndividualName]),
                None,
                draftReturn
              ).isFurtherOrAmendReturn shouldBe Some(false)

              fillingOutReturn(
                Right(sample[IndividualName]),
                Some(PreviousReturnData(List.empty, None)),
                draftReturn
              ).isFurtherOrAmendReturn shouldBe Some(false)
            }

            "the user has previously sent returns" in {
              fillingOutReturn(
                Right(sample[IndividualName]),
                Some(PreviousReturnData(List(sample[ReturnSummary]), None)),
                draftReturn
              ).isFurtherOrAmendReturn shouldBe Some(true)
            }

          }

          "the user is completing the return for someone else and" when {

            val draftReturn =
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
                  individualUserType = Some(PersonalRepresentative)
                )
              )

            "the user has not said if this is the first return for the person yet" in {
              fillingOutReturn(
                Right(sample[IndividualName]),
                Some(PreviousReturnData(List(sample[ReturnSummary]), None)),
                draftReturn.copy(
                  representeeAnswers = None
                )
              ).isFurtherOrAmendReturn shouldBe None

              fillingOutReturn(
                Right(sample[IndividualName]),
                Some(PreviousReturnData(List(sample[ReturnSummary]), None)),
                draftReturn.copy(
                  representeeAnswers = Some(
                    sample[IncompleteRepresenteeAnswers].copy(
                      isFirstReturn = None
                    )
                  )
                )
              ).isFurtherOrAmendReturn shouldBe None
            }

            "the user has said this is the first return for the person" in {
              fillingOutReturn(
                Right(sample[IndividualName]),
                Some(PreviousReturnData(List(sample[ReturnSummary]), None)),
                draftReturn.copy(
                  representeeAnswers = Some(
                    sample[IncompleteRepresenteeAnswers].copy(
                      isFirstReturn = Some(true)
                    )
                  )
                )
              ).isFurtherOrAmendReturn shouldBe Some(false)
            }

            "the user has said this is not the first return for the person" in {
              fillingOutReturn(
                Right(sample[IndividualName]),
                Some(PreviousReturnData(List(sample[ReturnSummary]), None)),
                draftReturn.copy(
                  representeeAnswers = Some(
                    sample[CompleteRepresenteeAnswers].copy(
                      isFirstReturn = false
                    )
                  )
                )
              ).isFurtherOrAmendReturn shouldBe Some(true)
            }

          }

        }

      }

    }

  }

}
