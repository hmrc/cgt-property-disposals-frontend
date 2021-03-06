/*
 * Copyright 2021 HM Revenue & Customs
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
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.traverse._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, PreviousReturnData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AcquisitionDetailsAnswers.CompleteAcquisitionDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType.{NonResidential, Residential}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.CompleteDisposalDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.Self
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.OtherReliefsOption.NoOtherReliefs
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ReliefDetailsAnswers.CompleteReliefDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.CompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{CalculatedGlarBreakdown, DraftSingleDisposalReturn, FurtherReturnCalculationData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FurtherReturnCalculationEligibility.{Eligible, Ineligible}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.FurtherReturnCalculationEligibilityUtilImpl.EligibleData
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.ListUtils._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[FurtherReturnCalculationEligibilityUtilImpl])
trait FurtherReturnCalculationEligibilityUtil {

  def isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn: FillingOutReturn)(implicit
    hc: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): EitherT[Future, Error, FurtherReturnCalculationEligibility]

}

sealed trait FurtherReturnCalculationEligibility

object FurtherReturnCalculationEligibility {

  final case class Eligible(
    calculation: CalculatedGlarBreakdown,
    previousReturnCalculationData: List[FurtherReturnCalculationData],
    currentReturnAddress: UkAddress
  ) extends FurtherReturnCalculationEligibility

  final case class Ineligible(previousReturnsImplyEligibility: Option[Boolean])
      extends FurtherReturnCalculationEligibility

  implicit class FurtherReturnEligibilityOps(private val f: FurtherReturnCalculationEligibility) extends AnyVal {
    def isEligible: Boolean = f match {
      case _: Eligible   => true
      case _: Ineligible => false
    }

  }

}

@Singleton
class FurtherReturnCalculationEligibilityUtilImpl @Inject() (
  returnsService: ReturnsService,
  configuration: Configuration,
  sessionStore: SessionStore
)(implicit ec: ExecutionContext)
    extends FurtherReturnCalculationEligibilityUtil
    with SessionUpdates {

  private val maxPreviousReturns: Int =
    configuration.underlying.getInt("amend-and-further-returns-calculator.max-previous-returns")

  def isEligibleForFurtherReturnOrAmendCalculation(
    fillingOutReturn: FillingOutReturn
  )(implicit
    headerCarrier: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): EitherT[Future, Error, FurtherReturnCalculationEligibility] =
    for {
      eligibility   <- determineEligibility(fillingOutReturn)
      updatedJourney =
        fillingOutReturn.copy(
          previousSentReturns = fillingOutReturn.previousSentReturns.map { p =>
            eligibility match {
              case e: FurtherReturnCalculationEligibility.Eligible =>
                p.copy(
                  previousReturnsImplyEligibilityForCalculation = Some(true),
                  calculationData = Some(e.previousReturnCalculationData)
                )

              case FurtherReturnCalculationEligibility.Ineligible(previousReturnsImplyEligibility) =>
                p.copy(
                  previousReturnsImplyEligibilityForCalculation = previousReturnsImplyEligibility,
                  calculationData = None
                )
            }
          }
        )
      _             <- if (updatedJourney === fillingOutReturn) EitherT.pure[Future, Error](())
                       else EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedJourney))))
    } yield eligibility

  private def determineEligibility(
    fillingOutReturn: FillingOutReturn
  )(implicit
    headerCarrier: HeaderCarrier
  ): EitherT[Future, Error, FurtherReturnCalculationEligibility] =
    eligibleGlarCalculation(fillingOutReturn) match {
      case Left(e) => EitherT.leftT(e)

      case Right(None) =>
        EitherT.pure(
          Ineligible(fillingOutReturn.previousSentReturns.flatMap(_.previousReturnsImplyEligibilityForCalculation))
        )

      case Right(Some(EligibleData(glarBreakdown, assetType, previousReturnData, address))) =>
        (previousReturnData.previousReturnsImplyEligibilityForCalculation, previousReturnData.calculationData) match {
          case (Some(true), Some(previousReturnCalculationData)) =>
            EitherT.pure(Eligible(glarBreakdown, previousReturnCalculationData, address))

          case (Some(false), _) =>
            EitherT.pure(Ineligible(Some(false)))

          case _ =>
            val submissionIdsOfPreviousReturns = previousReturnData.summaries
              .map(_.submissionId)
              .filterNot(id => fillingOutReturn.amendReturnData.exists(_.originalReturn.summary.submissionId === id))

            val results: List[EitherT[Future, Error, Option[FurtherReturnCalculationData]]] =
              submissionIdsOfPreviousReturns.map {
                returnsService
                  .displayReturn(fillingOutReturn.subscribedDetails.cgtReference, _)
                  .map(_.completeReturn match {
                    case c: CompleteSingleDisposalReturn =>
                      val assetTypeCheck   = c.triageAnswers.assetType === assetType.merge
                      val otherReliefCheck = c.reliefDetails.otherReliefs.contains(NoOtherReliefs)

                      if (assetTypeCheck && otherReliefCheck) {
                        val gainOrLossAfterReliefs = c.gainOrLossAfterReliefs.getOrElse(
                          calculatedGlarBreakdown(
                            c.disposalDetails,
                            c.acquisitionDetails,
                            c.reliefDetails
                          ).gainOrLossAfterReliefs
                        )
                        Some(FurtherReturnCalculationData(c.propertyAddress, gainOrLossAfterReliefs))
                      } else None
                    case _                               => None
                  })
              }

            results.sequence[EitherT[Future, Error, *], Option[FurtherReturnCalculationData]].map { results =>
              val (ineligibleResults, eligibleResults) = results.partitionWith(Either.fromOption(_, ()))

              if (ineligibleResults.isEmpty)
                Eligible(glarBreakdown, eligibleResults, address)
              else
                Ineligible(Some(false))

            }

        }

    }

  private def eligibleGlarCalculation(
    fillingOutReturn: FillingOutReturn
  ): Either[Error, Option[EligibleData]] =
    fillingOutReturn.draftReturn match {
      case DraftSingleDisposalReturn(
            _,
            triageAnswers: CompleteSingleDisposalTriageAnswers,
            Some(address),
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
        if (!fillingOutReturn.subscribedDetails.isATrust && !triageAnswers.individualUserType.contains(Self))
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
                  val noOtherReliefs           = reliefDetailsAnswers.otherReliefs.contains(NoOtherReliefs)
                  val underPreviousReturnLimit = previousReturnData.summaries.length <= maxPreviousReturns
                  val currentReturnIsEligible  = noOtherReliefs && underPreviousReturnLimit

                  if (currentReturnIsEligible && noOtherReliefs)
                    Some(
                      EligibleData(
                        calculatedGlarBreakdown(
                          disposalDetailsAnswers,
                          acquisitionDetailsAnswers,
                          reliefDetailsAnswers
                        ),
                        assetType,
                        previousReturnData,
                        address
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

  private def calculatedGlarBreakdown(
    disposalDetailsAnswers: CompleteDisposalDetailsAnswers,
    acquisitionDetailsAnswers: CompleteAcquisitionDetailsAnswers,
    reliefDetailsAnswers: CompleteReliefDetailsAnswers
  ): CalculatedGlarBreakdown =
    CalculatedGlarBreakdown(
      disposalFees = disposalDetailsAnswers.disposalFees,
      disposalPrice = disposalDetailsAnswers.disposalPrice,
      acquisitionPrice = acquisitionDetailsAnswers.acquisitionPrice,
      acquisitionCosts = acquisitionDetailsAnswers.acquisitionFees,
      improvementCosts = acquisitionDetailsAnswers.improvementCosts,
      privateResidentReliefs = reliefDetailsAnswers.privateResidentsRelief,
      lettingRelief = reliefDetailsAnswers.lettingsRelief
    )

}

object FurtherReturnCalculationEligibilityUtilImpl {

  private final case class EligibleData(
    glarBreakdown: CalculatedGlarBreakdown,
    assetType: Either[NonResidential.type, Residential.type],
    previousSentReturns: PreviousReturnData,
    currentReturnAddress: UkAddress
  )

}
