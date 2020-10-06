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
import play.api.Configuration
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{NonResidential, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.Self
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CalculatedGlarBreakdown, DraftSingleDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FurtherReturnEligibility.{Eligible, Ineligible}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[FurtherReturnEligibilityUtilImpl])
trait FurtherReturnEligibilityUtil {

  def isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn: FillingOutReturn)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, FurtherReturnEligibility]

}

sealed trait FurtherReturnEligibility

object FurtherReturnEligibility {
  final case class Eligible(calculation: CalculatedGlarBreakdown) extends FurtherReturnEligibility
  final case class Ineligible(previousReturnsImplyEligibility: Option[Boolean]) extends FurtherReturnEligibility
}

@Singleton
class FurtherReturnEligibilityUtilImpl @Inject() (
  returnsService: ReturnsService,
  configuration: Configuration
)(implicit ec: ExecutionContext)
    extends FurtherReturnEligibilityUtil
    with SessionUpdates {

  private val amendAndFurtherReturnCalculationsEnabled: Boolean =
    configuration.underlying.getBoolean("amend-and-further-returns-calculator.enabled")

  private val maxPreviousReturns: Int =
    configuration.underlying.getInt("amend-and-further-returns-calculator.max-previous-returns")

  def isEligibleForFurtherReturnOrAmendCalculation(
    fillingOutReturn: FillingOutReturn
  )(implicit
    headerCarrier: HeaderCarrier
  ): EitherT[Future, Error, FurtherReturnEligibility] =
    if (!amendAndFurtherReturnCalculationsEnabled)
      EitherT.pure(
        Ineligible(fillingOutReturn.previousSentReturns.flatMap(_.previousReturnsImplyEligibilityForCalculation))
      )
    else
      eligibleGlarCalculation(fillingOutReturn) match {
        case Left(e) => EitherT.leftT(e)

        case Right(None) =>
          EitherT.pure(
            Ineligible(fillingOutReturn.previousSentReturns.flatMap(_.previousReturnsImplyEligibilityForCalculation))
          )

        case Right(Some(glarBreakdown)) =>
          glarBreakdown.previousReturnData.previousReturnsImplyEligibilityForCalculation match {
            case Some(true) =>
              EitherT.pure(Eligible(glarBreakdown))

            case Some(false) =>
              EitherT.pure(Ineligible(Some(false)))

            case None =>
              val submissionIdsOfPreviousReturns = glarBreakdown.previousReturnData.summaries.map(_.submissionId)

              val results: List[EitherT[Future, Error, Boolean]] =
                submissionIdsOfPreviousReturns.map {
                  returnsService
                    .displayReturn(fillingOutReturn.subscribedDetails.cgtReference, _)
                    .map(_.completeReturn match {
                      case c: CompleteSingleDisposalReturn =>
                        val assetTypeCheck   = c.triageAnswers.assetType === glarBreakdown.assetType.merge
                        val otherReliefCheck = c.reliefDetails.otherReliefs.isEmpty
                        assetTypeCheck && otherReliefCheck
                      case _                               => false
                    })
                }

              results.sequence[EitherT[Future, Error, *], Boolean].map { e =>
                if (e.forall(identity))
                  Eligible(glarBreakdown)
                else
                  Ineligible(Some(false))
              }

          }

      }

  private def eligibleGlarCalculation(
    fillingOutReturn: FillingOutReturn
  ): Either[Error, Option[CalculatedGlarBreakdown]] =
    fillingOutReturn.draftReturn match {
      case DraftSingleDisposalReturn(
            _,
            triageAnswers: CompleteSingleDisposalTriageAnswers,
            _,
            Some(disposalDetailsAnswers: CompleteDisposalDetailsAnswers),
            Some(acquisitionDetailsAnswers: CompleteAcquisitionDetailsAnswers),
            Some(reliefDetailsAnswers: CompleteReliefDetailsAnswers),
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        if (!triageAnswers.individualUserType.contains(Self))
          Right(None)
        else
          fillingOutReturn.previousSentReturns match {
            case None =>
              Left(Error("Could not find previous return data for individual further or amend return"))

            case Some(previousReturnData) =>
              val eligibleAssetType =
                triageAnswers.assetType match {
                  case NonResidential => Some(Left(NonResidential))
                  case Residential    => Some(Right(Residential))
                  case _              => None
                }

              val result = eligibleAssetType match {
                case None            => None
                case Some(assetType) =>
                  val noOtherReliefs           = reliefDetailsAnswers.otherReliefs.isEmpty
                  val underPreviousReturnLimit = previousReturnData.summaries.length <= maxPreviousReturns
                  val currentReturnIsEligible  = noOtherReliefs && underPreviousReturnLimit

                  if (currentReturnIsEligible)
                    Some(
                      CalculatedGlarBreakdown(
                        disposalFees = disposalDetailsAnswers.disposalFees,
                        disposalPrice = disposalDetailsAnswers.disposalPrice,
                        acquisitionPrice = acquisitionDetailsAnswers.acquisitionPrice,
                        acquisitionCosts = acquisitionDetailsAnswers.acquisitionFees,
                        improvementCosts = acquisitionDetailsAnswers.improvementCosts,
                        privateResidentReliefs = reliefDetailsAnswers.privateResidentsRelief,
                        lettingRelief = reliefDetailsAnswers.lettingsRelief,
                        assetType = assetType,
                        previousReturnData = previousReturnData
                      )
                    )
                  else None
              }

              Right(result)
          }

      case _: DraftSingleDisposalReturn =>
        Left(Error("Insufficient data in DraftSingleDisposalReturn to determine eligibility for calculation"))

      case _ => Right(None)
    }

}
