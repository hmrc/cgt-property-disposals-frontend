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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TaxYearGen.taxYearGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.GGCredId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsTriageAnswers, IncompleteMultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._

import java.time.LocalDate

class JourneyStatusSpec extends AnyWordSpec with Matchers {

  val date    = LocalDate.of(2020, 4, 6)
  val taxYear = sample[TaxYear].copy(startDateInclusive = date)

  "FillingOutReturn" must {

    "have a method" which {

      "tries to say when a draft is return is a further return" when {

        def getdraftReturn(): DraftReturn = {

          val sampleDisosalDate        = DisposalDate(date, taxYear)
          val draftReturn: DraftReturn = sample[DraftReturn].fold(
            _.copy(triageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers].copy(
                taxYear = taxYear
              )
            ),
            _.copy(triageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                disposalDate = sampleDisosalDate
              )
            ),
            _.copy(triageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                disposalDate = sampleDisosalDate
              )
            ),
            _.copy(triageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers].copy(
                taxYear = taxYear
              )
            ),
            _.copy(triageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                disposalDate = sampleDisosalDate
              )
            )
          )

          draftReturn
        }

        def getPrviousReturnDate(): PreviousReturnData = {
          val taxYearStartYear: String = getdraftReturn()
            .fold(
              _.triageAnswers.fold(
                _.taxYear.map(_.startDateInclusive.getYear),
                c => Some(c.taxYear.startDateInclusive.getYear)
              ),
              _.triageAnswers.fold(
                _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
                c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
              ),
              _.triageAnswers.fold(
                _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
                c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
              ),
              _.triageAnswers.fold(
                _.taxYear.map(_.startDateInclusive.getYear),
                c => Some(c.taxYear.startDateInclusive.getYear)
              ),
              _.triageAnswers.fold(
                _.disposalDate.map(_.taxYear.startDateInclusive.getYear),
                c => Some(c.disposalDate.taxYear.startDateInclusive.getYear)
              )
            )
            .getOrElse(2020)
            .toString

          PreviousReturnData(
            List(sample[ReturnSummary].copy(taxYear = taxYearStartYear)),
            None,
            None,
            None
          )
        }

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
              getdraftReturn()
            ).isFurtherOrAmendReturn shouldBe Some(false)

            fillingOutReturn(
              Left(sample[TrustName]),
              Some(PreviousReturnData(List.empty, None, None, None)),
              getdraftReturn()
            ).isFurtherOrAmendReturn shouldBe Some(false)
          }

          "the user has previously sent returns" in {
            fillingOutReturn(
              Left(sample[TrustName]),
              Some(getPrviousReturnDate()),
              getdraftReturn()
            ).isFurtherOrAmendReturn shouldBe Some(true)
          }

        }

        "the user is not a trust and" when {

          "the user has not said who they are completing the return for" in {
            fillingOutReturn(
              Right(sample[IndividualName]),
              Some(PreviousReturnData(List(sample[ReturnSummary]), None, None, None)),
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = IncompleteSingleDisposalTriageAnswers.empty
              )
            ).isFurtherOrAmendReturn shouldBe None
          }

          "the user is completing the return for themselves and" when {

            val draftReturn =
              sample[DraftMultipleDisposalsReturn].copy(
                triageAnswers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                  individualUserType = Some(Self),
                  taxYear = Some(taxYear)
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
                Some(PreviousReturnData(List.empty, None, None, None)),
                draftReturn
              ).isFurtherOrAmendReturn shouldBe Some(false)
            }

            "the user has previously sent returns" in {
              fillingOutReturn(
                Right(sample[IndividualName]),
                Some(getPrviousReturnDate()),
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
                Some(PreviousReturnData(List(sample[ReturnSummary]), None, None, None)),
                draftReturn.copy(
                  representeeAnswers = None
                )
              ).isFurtherOrAmendReturn shouldBe None

              fillingOutReturn(
                Right(sample[IndividualName]),
                Some(PreviousReturnData(List(sample[ReturnSummary]), None, None, None)),
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
                Some(PreviousReturnData(List(sample[ReturnSummary]), None, None, None)),
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
                Some(PreviousReturnData(List(sample[ReturnSummary]), None, None, None)),
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

      "says whether or not the return is an amend return" in {
        sample[FillingOutReturn].copy(amendReturnData = Some(sample[AmendReturnData])).isAmendReturn shouldBe true
        sample[FillingOutReturn].copy(amendReturnData = None).isAmendReturn                          shouldBe false
      }

      "sets the shouldDisplayGainOrLossAfterReliefs field correctly" in {
        val fillingOutReturn = sample[FillingOutReturn].copy(amendReturnData = None)
        val amendReturnData  = sample[AmendReturnData]

        def fillingOutReturnWithDisplayGainOrLossAfterReliefsForAmends(shouldDisplayGainOrLossAfterReliefs: Boolean) =
          fillingOutReturn.copy(amendReturnData =
            Some(amendReturnData.copy(shouldDisplayGainOrLossAfterReliefs = shouldDisplayGainOrLossAfterReliefs))
          )

        fillingOutReturnWithDisplayGainOrLossAfterReliefsForAmends(
          true
        ).withForceDisplayGainOrLossAfterReliefsForAmends shouldBe
          fillingOutReturnWithDisplayGainOrLossAfterReliefsForAmends(
            true
          ).withForceDisplayGainOrLossAfterReliefsForAmends

        fillingOutReturnWithDisplayGainOrLossAfterReliefsForAmends(
          false
        ).withForceDisplayGainOrLossAfterReliefsForAmends shouldBe
          fillingOutReturnWithDisplayGainOrLossAfterReliefsForAmends(
            true
          ).withForceDisplayGainOrLossAfterReliefsForAmends

        fillingOutReturn.withForceDisplayGainOrLossAfterReliefsForAmends shouldBe fillingOutReturn
      }

    }

  }

}
