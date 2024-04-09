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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics

import com.codahale.metrics.{Counter, Timer}
import com.google.inject.{Inject, Singleton}

@Singleton
class Metrics @Inject() (metrics: com.codahale.metrics.MetricRegistry) {

  protected def timer(name: String): Timer =
    metrics.timer(s"frontend.$name")

  protected def counter(name: String): Counter =
    metrics.counter(s"frontend.$name")

  val postcodeLookupTimer: Timer = timer("postcode-lookup.time")

  val postcodeLookupErrorCounter: Counter = counter(
    "postcode-lookup.errors.count"
  )

  val emailVerificationTimer: Timer = timer("email-verification.time")

  val emailVerificationErrorCounter: Counter = counter(
    "email-verification.errors.count"
  )

  val ivIncompleteCounter: Counter = counter(
    "iv.journey-status.incomplete.count"
  )

  val ivFailedMatchingCounter: Counter = counter(
    "iv.journey-status.failed-matching.count"
  )

  val ivFailedIVCounter: Counter = counter("iv.journey-status.failed-iv.count")

  val ivInsufficientEvidenceCounter: Counter = counter(
    "iv.journey-status.insufficient-evidence.count"
  )

  val ivUserAbortedCounter: Counter = counter(
    "iv.journey-status.user-aborted.count"
  )

  val ivLockedOutCounter: Counter = counter(
    "iv.journey-status.locked-out.count"
  )

  val ivPreconditionFailedCounter: Counter = counter(
    "iv.journey-status.precondition-failed.count"
  )

  val ivTechnicalIssueCounter: Counter = counter(
    "iv.journey-status.technical-issue.count"
  )

  val ivTimeoutCounter: Counter = counter("iv.journey-status.timeout.count")

  val ivUnknownErrorCounter: Counter = counter(
    "iv.journey-status.unknown-error.count"
  )

  val ivGetFailedJourneyStatusTimer: Timer = timer(
    "iv.get-failed-journey-status.time"
  )

  val ivGetFailedJourneyStatusErrorCounter: Counter = counter(
    "iv.get-failed-journey-status.errors.count"
  )

  val accessWithWrongGGAccountCounter: Counter = counter(
    "access-with-wrong-gg-account.count"
  )

  val individualWantingToRegisterTrustCounter: Counter = counter(
    "individual-wanting-to-register-trust.count"
  )

  val unregisteredTrustCounter: Counter = counter("unregistered-trust.count")

  val nonTrustOrganisationCounter: Counter = counter(
    "non-trust-organisation.count"
  )

}
