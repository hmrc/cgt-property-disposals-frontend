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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftReturn
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ReturnsServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[ReturnsConnector]

  def mockStoreDraftReturn(draftReturn: DraftReturn)(response: Either[Error, HttpResponse]) =
    (mockConnector
      .storeDraftReturn(_: DraftReturn)(_: HeaderCarrier))
      .expects(draftReturn, *)
      .returning(EitherT.fromEither[Future](response))

  val service = new ReturnsServiceImpl(mockConnector)

  "ReturnsServiceImpl" when {

    "handling requests to store draft returns" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val draftReturn = sample[DraftReturn]

      "return an error" when {

        "the http call fails" in {
          mockStoreDraftReturn(draftReturn)(Left(Error("")))

          await(service.storeDraftReturn(draftReturn).value).isLeft shouldBe true
        }

        "the http call came back with a status other than 200" in {
          mockStoreDraftReturn(draftReturn)(Right(HttpResponse(INTERNAL_SERVER_ERROR)))

          await(service.storeDraftReturn(draftReturn).value).isLeft shouldBe true
        }

      }

      "return an ok response" when {

        "the http call came back with a 200" in {
          mockStoreDraftReturn(draftReturn)(Right(HttpResponse(OK)))

          await(service.storeDraftReturn(draftReturn).value) shouldBe Right(())
        }

      }

    }

  }

}
