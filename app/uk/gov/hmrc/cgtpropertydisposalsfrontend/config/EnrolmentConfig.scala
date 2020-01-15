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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.config

object EnrolmentConfig {

  object Cgt {

    val key = "HMRC-CGT-PD"

    val cgtReferenceIdentifier = "CGTPDRef"

    val delegateAuthRule = "cgt-auth"
  }

  object Trusts {

    val key = "HMRC-TERS-ORG"

    val sautrIdentifier = "SAUTR"

  }

  object Agents {

    val key = "HMRC-AS-AGENT"

    val agentReferenceNumberIdentifier = "AGENTREFERENCENUMBER"

  }

}
