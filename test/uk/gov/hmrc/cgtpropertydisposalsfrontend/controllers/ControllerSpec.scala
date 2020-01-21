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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import java.net.URLEncoder

import akka.stream.Materializer
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import play.api.inject.{Binding, bind}
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.{Application, Configuration, Play}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.{Metrics, MockMetrics}

import scala.concurrent.Future
import scala.reflect.ClassTag

trait ControllerSpec extends WordSpec with Matchers with BeforeAndAfterAll with MockFactory {

  val overrideBindings: List[GuiceableModule] = List.empty[GuiceableModule]

  lazy val additionalConfig = Configuration()

  def buildFakeApplication(): Application = {
    val metricsBinding: GuiceableModule = bind[Metrics].toInstance(MockMetrics.metrics)

    new GuiceApplicationBuilder()
      .configure(
        Configuration(
          ConfigFactory.parseString(
            """
              | metrics.jvm = false
              | metrics.logback = false
              | microservice.upscan-initiate.upscan-store.expiry-time = 1
          """.stripMargin
          )
        ) ++ additionalConfig
      )
      .disable[uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore]
      .overrides(metricsBinding :: overrideBindings: _*)
      .build()
  }

  lazy val fakeApplication: Application = buildFakeApplication()

  def instanceOf[A: ClassTag]: A = fakeApplication.injector.instanceOf[A]

  lazy implicit val materializer: Materializer = fakeApplication.materializer
  lazy val viewConfig                          = instanceOf[ViewConfig]

  abstract override def beforeAll(): Unit = {
    Play.start(fakeApplication)
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    Play.stop(fakeApplication)
    super.afterAll()
  }

  def message(messageKey: String, args: Any*)(implicit messagesApi: MessagesApi): String =
    messagesApi(messageKey, args: _*)(Lang.defaultLang)

  def checkIsTechnicalErrorPage(result: Future[Result])(implicit messagesApi: MessagesApi): Unit = {
    status(result)          shouldBe INTERNAL_SERVER_ERROR
    contentAsString(result) should include(message("global.error.InternalServerError500.title"))
  }

  def checkIsRedirect(result: Future[Result], expectedRedirectUrl: String): Unit = {
    status(result)           shouldBe SEE_OTHER
    redirectLocation(result) shouldBe Some(expectedRedirectUrl)
  }

  def checkIsRedirect(result: Future[Result], expectedRedirectCall: Call): Unit =
    checkIsRedirect(result, expectedRedirectCall.url)

  def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")

}
