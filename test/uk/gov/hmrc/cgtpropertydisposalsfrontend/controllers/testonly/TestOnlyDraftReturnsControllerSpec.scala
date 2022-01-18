/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.testonly

import cats.data.EitherT
import cats.instances.future._
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.ControllerSpec
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.testonly.{TestOnlyDraftReturnsConnector, TestOnlyDraftReturnsController}
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TestOnlyDraftReturnsControllerSpec extends ControllerSpec {

  val mockTestOnlyDraftReturnsConnector: TestOnlyDraftReturnsConnector = mock[TestOnlyDraftReturnsConnector]

  def mockTestOnlyDraftReturnsConnector(cgtReference: String)(result: Either[Error, HttpResponse]): Unit =
    (mockTestOnlyDraftReturnsConnector
      .deleteDraftReturn(_: String))
      .expects(cgtReference)
      .returning(EitherT.fromEither[Future](result))

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[TestOnlyDraftReturnsConnector].toInstance(mockTestOnlyDraftReturnsConnector)
    )

  lazy val controller: TestOnlyDraftReturnsController = instanceOf[TestOnlyDraftReturnsController]

  private val emptyJsonBody = "{}"

  def performAction(cgtReference: String): Future[Result] =
    controller.deleteDraftReturn(cgtReference).apply(FakeRequest())

  "TestOnlyDraftReturnsController" must {
    "handling requests to delete a draft return" in {
      val cgtReference: String = "sampleCgtRef"

      mockTestOnlyDraftReturnsConnector(cgtReference)(Right(HttpResponse(OK, emptyJsonBody)))
      status(performAction(cgtReference)) shouldBe OK
    }

    "return an error when delete a draft return returns and error" in {
      val cgtReference: String = "sampleCgtRef"

      mockTestOnlyDraftReturnsConnector(cgtReference)(Left(Error("Error")))
      status(performAction(cgtReference)) shouldBe INTERNAL_SERVER_ERROR
    }
  }
}
