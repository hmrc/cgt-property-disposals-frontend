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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators

import org.scalacheck.Gen
import io.github.martinhh.derived.scalacheck.given
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsTriageAnswers, IncompleteMultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.*

object TriageQuestionsGen extends HigherPriorityTriageQuestionsGen with GenUtils

trait HigherPriorityTriageQuestionsGen extends LowerPriorityTriageQuestionsGen { this: GenUtils =>

  implicit val completeSingleDisposalTriageAnswersGen: Gen[CompleteSingleDisposalTriageAnswers] =
    for {
      individualUserType        <- Gen.option(individualUserTypeGen)
      disposalMethod            <- DisposalMethodGen.disposalMethodGen
      countryOfResidence        <- AddressGen.countryGen
      assetType                 <- assetTypeGen
      disposalDate              <- disposalDateGen
      alreadySentSelfAssessment <- Gen.option(Generators.booleanGen)
      completionDate            <- completionDateGen
    } yield CompleteSingleDisposalTriageAnswers(
      individualUserType,
      disposalMethod,
      countryOfResidence,
      assetType,
      disposalDate,
      alreadySentSelfAssessment,
      completionDate
    )

  implicit val individualTriageAnswersGen: Gen[SingleDisposalTriageAnswers] = Gen.oneOf(
    completeSingleDisposalTriageAnswersGen,
    incompleteSingleDisposalTriageAnswersGen
  )

  val singleDisposalTraiageAnswersGen: Gen[SingleDisposalTriageAnswers] =
    Gen.oneOf(completeSingleDisposalTriageAnswersGen, incompleteSingleDisposalTriageAnswersGen)

  given taxYearExchangedGen: Gen[TaxYearExchanged] = gen[TaxYearExchanged]

  implicit val completeMultipleDisposalsTriageAnswersGen: Gen[CompleteMultipleDisposalsTriageAnswers] =
    for {
      individualUserType        <- Gen.option(individualUserTypeGen)
      numberOfProperties        <- Gen.size
      countryOfResidence        <- AddressGen.countryGen
      assetTypes                <- Generators.listOfMax(3, assetTypeGen)
      taxYearExchanged          <- Gen.option(taxYearExchangedGen)
      taxYear                   <- TaxYearGen.taxYearGen
      alreadySentSelfAssessment <- Gen.option(Generators.booleanGen)
      completionDate            <- completionDateGen
    } yield CompleteMultipleDisposalsTriageAnswers(
      individualUserType,
      numberOfProperties,
      countryOfResidence,
      assetTypes,
      taxYearExchanged,
      taxYear,
      alreadySentSelfAssessment,
      completionDate
    )

  implicit val incompleteMultipleDisposalsTriageAnswersGen: Gen[IncompleteMultipleDisposalsTriageAnswers] =
    for {
      individualUserType           <- Gen.option(individualUserTypeGen)
      numberOfProperties           <- Gen.option(Gen.size)
      wasAUKResident               <- Gen.option(Generators.booleanGen)
      countryOfResidence           <- Gen.option(AddressGen.countryGen)
      wereAllPropertiesResidential <- Gen.option(Generators.booleanGen)
      assetTypes                   <- Gen.option(Generators.listOfMax(3, assetTypeGen))
      taxYearExchanged             <- Gen.option(taxYearExchangedGen)
      taxYear                      <- Gen.option(TaxYearGen.taxYearGen)
      alreadySentSelfAssessment    <- Gen.option(Generators.booleanGen)
      completionDate               <- Gen.option(completionDateGen)
    } yield IncompleteMultipleDisposalsTriageAnswers(
      individualUserType,
      numberOfProperties,
      wasAUKResident,
      countryOfResidence,
      wereAllPropertiesResidential,
      assetTypes,
      taxYearExchanged,
      taxYear,
      alreadySentSelfAssessment,
      completionDate
    )

  val multipleDisposalsTriageAnswersGen: Gen[MultipleDisposalsTriageAnswers] =
    Gen.oneOf(completeMultipleDisposalsTriageAnswersGen, incompleteMultipleDisposalsTriageAnswersGen)

  given numberOfPropertiesGen: Gen[NumberOfProperties] =
    Gen.oneOf(NumberOfProperties.One, NumberOfProperties.MoreThanOne)
}

trait LowerPriorityTriageQuestionsGen { this: GenUtils =>

  implicit val individualUserTypeGen: Gen[IndividualUserType] =
    Gen.oneOf(
      IndividualUserType.Self,
      IndividualUserType.Capacitor,
      IndividualUserType.PersonalRepresentative,
      IndividualUserType.PersonalRepresentativeInPeriodOfAdmin
    )

  implicit val assetTypeGen: Gen[AssetType] = Gen.oneOf(
    AssetType.Residential,
    AssetType.NonResidential,
    AssetType.IndirectDisposal,
    AssetType.MixedUse
  )

  implicit val disposalDateGen: Gen[DisposalDate] = for {
    value   <- Generators.dateGen
    taxYear <- TaxYearGen.taxYearGen
  } yield DisposalDate(value, taxYear)

  implicit val completionDateGen: Gen[CompletionDate] = Generators.dateGen.map(CompletionDate(_))

  implicit val incompleteSingleDisposalTriageAnswersGen: Gen[IncompleteSingleDisposalTriageAnswers] =
    for {
      individualUserType         <- Gen.option(individualUserTypeGen)
      hasConfirmedSingleDisposal <- Generators.booleanGen
      disposalMethod             <- Gen.option(DisposalMethodGen.disposalMethodGen)
      wasAUKResident             <- Gen.option(Generators.booleanGen)
      countryOfResidence         <- Gen.option(AddressGen.countryGen)
      assetType                  <- Gen.option(assetTypeGen)
      disposalDate               <- Gen.option(disposalDateGen)
      alreadySentSelfAssessment  <- Gen.option(Generators.booleanGen)
      completionDate             <- Gen.option(completionDateGen)
      tooEarlyDisposalDate       <- Gen.option(Generators.dateGen)
    } yield IncompleteSingleDisposalTriageAnswers(
      individualUserType,
      hasConfirmedSingleDisposal,
      disposalMethod,
      wasAUKResident,
      countryOfResidence,
      assetType,
      disposalDate,
      alreadySentSelfAssessment,
      completionDate,
      tooEarlyDisposalDate
    )

}
