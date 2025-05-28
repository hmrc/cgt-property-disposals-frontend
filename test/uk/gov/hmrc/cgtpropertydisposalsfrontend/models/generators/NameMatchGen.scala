/*
 * Copyright 2025 HM Revenue & Customs
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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails.{IndividualRepresenteeNameMatchDetails, IndividualSautrNameMatchDetails, TrustNameMatchDetails}

object NameMatchGen extends GenUtils:

  given trustNameMatchDetailsGen: Gen[TrustNameMatchDetails] =
    gen[TrustNameMatchDetails]

  given individualSautrNameMatchDetailsGen: Gen[IndividualSautrNameMatchDetails] =
    gen[IndividualSautrNameMatchDetails]

  given individualUnsuccessfulNameMatchAttemptsGen
    : Gen[UnsuccessfulNameMatchAttempts[IndividualSautrNameMatchDetails]] =
    gen[UnsuccessfulNameMatchAttempts[IndividualSautrNameMatchDetails]]

  given individualRepresenteeNameMatchDetailsGen: Gen[IndividualRepresenteeNameMatchDetails] =
    gen[IndividualRepresenteeNameMatchDetails]
