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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.views.returns

import cats.Eq
import cats.syntax.eq._

sealed trait TaskListStatus extends Product with Serializable

object TaskListStatus {

  case object Complete extends TaskListStatus

  case object InProgress extends TaskListStatus

  case object CannotStart extends TaskListStatus

  case object ToDo extends TaskListStatus

  implicit val eq: Eq[TaskListStatus] = Eq.fromUniversalEquals[TaskListStatus]

  implicit class TaskListStatusOps(private val a: TaskListStatus) extends AnyVal {
    def isComplete(): Boolean = a === Complete
  }

}
