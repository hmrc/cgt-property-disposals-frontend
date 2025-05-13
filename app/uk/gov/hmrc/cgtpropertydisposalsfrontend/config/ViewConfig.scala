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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.config

import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.{Inject, Singleton}

@Singleton
class ViewConfig @Inject() (servicesConfig: ServicesConfig) extends Logging {

  private def getString(key: String): String = servicesConfig.getString(key)

  private val basGatewayFrontendUrl: String = getString("bas-gateway-frontend.url")
  private val signOutUri: String            = getString("sign-out.uri")
  private val contactFormServiceIdentifier  = "CGTPD"

  val govUkUrl: String                                = getString("external-url.gov-uk")
  val signOutUrl: String                              = s"$basGatewayFrontendUrl$signOutUri"
  val trustRegistrationUrl: String                    = getString("external-url.trust-registration")
  val callChargesUrl: String                          = getString("external-url.gov-call-charges")
  val additionalNeedsUrl: String                      = getString("external-url.additional-needs")
  val taxSellHomeUrl: String                          = getString("external-url.tax-sell-home")
  val cgtUrl: String                                  = getString("external-url.capital-gains-tax")
  val cgtLegacyUrl: String                            = getString("external-url.capital-gains-tax-legacy")
  val cgtModernUrl: String                            = getString("external-url.capital-gains-tax-modern")
  val legacyCgtNonResidentUrl: String                 = getString("external-url.legacy-cgt-non-resident")
  val agentsStartPageUrl: String                      = getString("external-url.agents-start-page")
  val createAgentsAccountUrl: String                  = getString("external-url.create-agents-account")
  val lostUtrUrl: String                              = getString("external-url.lost-utr")
  val trusteeResponsibilitiesUrl: String              = getString("external-url.trustee-responsibilities")
  val fileCorporationTaxUrl: String                   = getString("external-url.file-corporation-tax")
  val trustHelpUrl: String                            = getString("external-url.trust-help")
  val tellHmrcChangeDetails: String                   = getString("external-url.tell-hmrc-change-details")
  val workOurYouResidenceStatusUrl: String            = getString("external-url.work-out-your-residence-status")
  val tranferringOwnershipHelp: String                = getString("external-url.transferring-ownership-help")
  val cgtLossesUrl: String                            = getString("external-url.cgt-losses")
  val cgtLossesUrlNonUk: String                       = getString("external-url.cgt-losses-non-uk")
  val annualExemptAmountUrl: String                   = getString("external-url.annual-exempt-amount")
  val taxFreeAllowanceUrl: String                     = getString("external-url.tax-free-allowance")
  val personalAllowanceUrl: String                    = getString("external-url.personal-allowance")
  val selfAssessmentUrl: String                       = getString("external-url.sign-in-to-self-assessment")
  val nrcgtReturn: String                             = getString("external-url.nrcgtReturn")
  val reliefsInfo: String                             = getString("external-url.reliefs-info")
  val lettingReliefInfo: String                       = getString("external-url.letting-relief-info")
  val marketValue: String                             = getString("external-url.market-value")
  val trustsTaxFreeAllowance: String                  = getString("external-url.trusts-tax-free-allowance")
  val trustsForVulnerable: String                     = getString("external-url.trusts-for-vulnerable")
  val trustsAllowableCostsUrl: String                 = getString("external-url.trusts-allowable-costs")
  val nonResidentsRebasingUrl: String                 = getString("external-url.non-resident-rebasing")
  val govUkAccessibilityStatementUrl: String          = getString("external-url.gov-uk-accessibility-statement")
  val abilityNetUrl: String                           = getString("external-url.ability-net")
  val webContentAccessibilityGuidelinesV21Url: String = getString(
    "external-url.web-content-accessibility-guidelines-v2-1"
  )
  val equalityAndAdvisoryServiceUrl: String           = getString("external-url.equality-advisory-and-support-service")
  val equalityCommissionForNorthenIrelandUrl: String  = getString("external-url.equality-commission-for-northen-ireland")
  val digitalAccessibilityCentretUrl: String          = getString("external-url.digital-accessibility-centre")
  val reportAccessibilityProblemUrl: String           =
    s"${getString("external-url.report-accessibility-problem")}?service=cgtpd"
  val contactHmrc: String                             = getString("external-url.contact-hmrc")

  val personalRepresentativeUrl: String = getString("external-url.personal-representative")

  val calculateCgt: String   = getString("external-url.calculate-cgt")
  val calculateCgtNr: String = getString("external-url.calculate-cgt-non-resident")

  val payYourTax: String                   = getString("external-url.pay-your-tax")
  val payTheirTaxBill: String              = getString("external-url.pay-their-tax-bill")
  val powerOfAttorney: String              = getString("external-url.power-of-attorney")
  val cgtRatesUrl: String                  = getString("external-url.cgt-rates")
  val trustsAndCgtUrl: String              = getString("external-url.trusts-and-cgt")
  val trustsAndCgtWorkoutUrl: String       = getString("external-url.trusts-and-cgt-workout")
  val agentAskClientToAuthoriseUrl: String = getString("external-url.agent-ask-client-to-authorise")

  private val feedbackFeUrl: String   = getString("microservice.services.feedback-frontend.url")
  val onboardingExitSurveyUrl: String = s"$feedbackFeUrl/CGTPD-REG"
  val returnsExitSurveyUrl: String    = s"$feedbackFeUrl/CGTPD-RET"
  val amendsExitSurveyUrl: String     = s"$feedbackFeUrl/CGTPD-AMEND"
  val generalExistSurveyUrl: String   = s"$feedbackFeUrl/$contactFormServiceIdentifier"

  val ggCreateAccountUrl: String =
    "/bas-gateway?" +
      "accountType=individual&" +
      "continueUrl=%2Fcapital-gains-tax-uk-property%2Fstart&" +
      "origin=capital-gains-tax-uk-property-frontend"

  val ggTimedOutUrl: String = "/capital-gains-tax-uk-property" + routes.StartController
    .timedOut()
    .url

  val userResearchBannerEnabled: Boolean = servicesConfig.getBoolean("user-research-banner.enabled")
  val userResearchUrl: String            = getString("user-research-banner.url")

  val futureDatesEnabled: Boolean = servicesConfig.getBoolean("futureDates.enabled")

  val maxYearForDisposalsAndCompletion: Int = servicesConfig.getInt("futureDates.maxTaxYearAllowed")

  val selfAssessmentTaxReturnsCorrections: String = getString("external-url.sa-tax-returns-corrections")

  val draftReturnNewDueDateStartYear: Int  = servicesConfig.getInt("draft-return.new-due-date.start-year")
  val draftReturnNewDueDateStartMonth: Int = servicesConfig.getInt("draft-return.new-due-date.start-month")
  val draftReturnNewDueDateStartDay: Int   = servicesConfig.getInt("draft-return.new-due-date.start-day")

  val selfUrl: String = servicesConfig.getString("self.url")
}
