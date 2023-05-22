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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.views

import uk.gov.hmrc.govukfrontend.views.html.components._

import javax.inject.Inject

class ViewHelpers @Inject() (
  val govukButton: GovukButton,
  val govukErrorSummary: GovukErrorSummary,
  val govukRadios: GovukRadios,
  val govukDateInput: GovukDateInput,
  val govukInput: GovukInput,
  val govukFileUpload: GovukFileUpload,
  val govUkDetails: GovukDetails,
  val govukWarningText: GovukWarningText,
  val govukPanel: GovukPanel,
  val form: FormWithCSRF,
  val govukErrorMessage: GovukErrorMessage,
  val govukBackLink: GovukBackLink,
  val govukSummaryList: GovukSummaryList,
  val govukDetails: GovukDetails,
  val govukInsetText: GovukInsetText,
  val govukSelect: GovukSelect,
  val govukBreadcrumbs: GovukBreadcrumbs,
  val govukCheckboxes: GovukCheckboxes
)
