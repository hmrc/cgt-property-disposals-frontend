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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.JsString
import play.api.test.Helpers._
import play.api.{Configuration, Mode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.HttpSupport
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftReturn
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}

import scala.concurrent.ExecutionContext.Implicits.global

class ReturnsConnectorImplSpec extends WordSpec with Matchers with MockFactory with HttpSupport {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        |microservice {
        |  services {
        |    cgt-property-disposals {
        |      protocol = http
        |      host     = host
        |      port     = 123
        |    }
        |  }
        |}
        |""".stripMargin
    )
  )

  val connector = new ReturnsConnectorImpl(mockHttp, new ServicesConfig(config, new RunMode(config, Mode.Test)))

  "ReturnsConnectorImpl" when {

    "handling requests to store a draft return" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()
      val expectedUrl                = s"http://host:123/draft-return"
      val draftReturn                = sample[DraftReturn]

      "do a get http call and return the result" in {
        List(
          HttpResponse(200),
          HttpResponse(200, Some(JsString("hi"))),
          HttpResponse(500)
        ).foreach { httpResponse =>
          withClue(s"For http response [${httpResponse.toString}]") {
            mockPost(expectedUrl, Map.empty, draftReturn)(Some(httpResponse))

            await(connector.storeDraftReturn(draftReturn).value) shouldBe Right(httpResponse)
          }
        }
      }

      "return an error" when {

        "the future fails" in {
          mockPost(expectedUrl, Map.empty, draftReturn)(None)

          await(connector.storeDraftReturn(draftReturn).value).isLeft shouldBe true
        }

      }

    }

  }
}
