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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns

import cats.data.EitherT
import cats.instances.future._
import cats.instances.list._
import cats.syntax.eq._
import cats.syntax.traverse._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{NonResidential, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.Self
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.{ExecutionContext, Future}
@ImplementedBy(classOf[GlarCalculatorEligibilityUtilImpl])
trait GlarCalculatorEligibilityUtil {

  def isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn: FillingOutReturn)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Boolean]

}

@Singleton
class GlarCalculatorEligibilityUtilImpl @Inject() (
  returnsService: ReturnsService,
  servicesConfig: ServicesConfig
)(implicit ec: ExecutionContext)
    extends GlarCalculatorEligibilityUtil {

  val amendAndFurtherReturnCalculationsEnabled = servicesConfig.getBoolean("glar-calculator.enabled")

  def isEligibleForFurtherReturnOrAmendCalculation(
    fillingOutReturn: FillingOutReturn
  )(implicit headerCarrier: HeaderCarrier): EitherT[Future, Error, Boolean] = {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def isEligible(
      fillingOutReturn: FillingOutReturn,
      completeSingleDisposalTriageAnswers: CompleteSingleDisposalTriageAnswers,
      reliefDetailsAnswers: CompleteReliefDetailsAnswers
    ): EitherT[Future, Error, Boolean] = {
      val eligibleAssetType =
        completeSingleDisposalTriageAnswers.assetType === Residential || completeSingleDisposalTriageAnswers.assetType === NonResidential

      val noOtherReliefs = reliefDetailsAnswers.otherReliefs.isEmpty

      val underPreviousReturnLimit = fillingOutReturn.previousSentReturns.exists(_.summaries.length <= 10)

      val isSelf = completeSingleDisposalTriageAnswers.individualUserType.contains(Self)

      val currentReturnIsEligible = eligibleAssetType && noOtherReliefs && underPreviousReturnLimit && isSelf

      if (currentReturnIsEligible) {
        val submissionIdsOfPreviousReturns = fillingOutReturn.previousSentReturns
          .map(e => e.summaries)
          .getOrElse(List.empty)
          .map(e => e.submissionId)

        val results = submissionIdsOfPreviousReturns.map {
          returnsService
            .displayReturn(fillingOutReturn.subscribedDetails.cgtReference, _)
            .map(_.completeReturn match {
              case c: CompleteSingleDisposalReturn =>
                val assetTypeCheck   = c.triageAnswers.assetType === completeSingleDisposalTriageAnswers.assetType
                val otherReliefCheck = c.reliefDetails.otherReliefs.isEmpty
                assetTypeCheck && otherReliefCheck
              case _                               => false
            })
        }

        results.sequence.map { r =>
          r.forall(e => e)
        }

      } else EitherT.right(Future(false))
    }

    fillingOutReturn match {
      case FillingOutReturn(
            _,
            _,
            _,
            DraftSingleDisposalReturn(
              _,
              triageAnswers: CompleteSingleDisposalTriageAnswers,
              _,
              _,
              _,
              Some(reliefDetailsAnswers: CompleteReliefDetailsAnswers),
              _,
              _,
              _,
              _,
              _,
              _,
              _
            ),
            _,
            _
          ) if amendAndFurtherReturnCalculationsEnabled =>
        val bb = isEligible(fillingOutReturn, triageAnswers, reliefDetailsAnswers)
        bb

      case _ => EitherT.right(Future(false))
    }

  }

}
