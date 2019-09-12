/*
 * Copyright 2019 HM Revenue & Customs
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

sealed trait UserType

object UserType {

  final case class Individual(
                               id: Either[SAUTR,NINO],
                               name: Name,
                               dateOfBirth: Option[DateOfBirth],
                               email: Option[Email]
                             ) extends UserType

  final case class Trust(sautr: SAUTR, email: Option[Email]) extends UserType

  final case object OrganisationUnregisteredTrust extends UserType

  final case class IndividualWithInsufficientConfidenceLevel(
                                                              nino: Option[NINO],
                                                              sautr: Option[SAUTR],
                                                              name: Name,
                                                              email: Option[Email]
                                                            ) extends UserType

}
