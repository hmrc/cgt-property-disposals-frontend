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
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import play.api.http.HttpConfiguration
import play.api.i18n.{DefaultMessagesApi, DefaultMessagesApiProvider, Lang, Langs, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.mvc.{Call, Result}
import play.api.test.Helpers._
import play.api.{Application, Configuration, Environment, Logger, Play}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.{Metrics, MockMetrics}

import scala.concurrent.Future
import scala.reflect.ClassTag
import com.google.inject.{Inject, Singleton}

@Singleton
class TestMessagesApi(
  messages: Map[String, Map[String, String]],
  langs: Langs,
  langCookieName: String,
  langCookieSecure: Boolean,
  langCookieHttpOnly: Boolean,
  httpConfiguration: HttpConfiguration
) extends DefaultMessagesApi(
      messages,
      langs,
      langCookieName,
      langCookieSecure,
      langCookieHttpOnly,
      httpConfiguration
    ) {

  override protected def noMatch(key: String, args: Seq[Any])(implicit lang: Lang): String = {
    Logger.error(s"Could not find message for key: $key ${args.mkString("-")}")
    s"""not_found_message("$key")"""
  }
}

@Singleton
class TestDefaultMessagesApiProvider @Inject() (
  environment: Environment,
  config: Configuration,
  langs: Langs,
  httpConfiguration: HttpConfiguration
) extends DefaultMessagesApiProvider(environment, config, langs, httpConfiguration) {

  override lazy val get: MessagesApi = {
    new TestMessagesApi(
      loadAllMessages,
      langs,
      langCookieName = langCookieName,
      langCookieSecure = langCookieSecure,
      langCookieHttpOnly = langCookieHttpOnly,
      httpConfiguration = httpConfiguration
    )
  }
}

trait ControllerSpec extends WordSpec with Matchers with BeforeAndAfterAll with MockFactory {

  implicit val lang: Lang = Lang.defaultLang

  def overrideBindings: List[GuiceableModule] = List.empty[GuiceableModule]

  lazy val additionalConfig = Configuration()

  def buildFakeApplication(): Application = {
    val metricsBinding: GuiceableModule =
      bind[Metrics].toInstance(MockMetrics.metrics)

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
      .overrides(bind[MessagesApi].toProvider[TestDefaultMessagesApiProvider])
      .build()
  }

  lazy val fakeApplication: Application = buildFakeApplication()

  def instanceOf[A : ClassTag]: A = fakeApplication.injector.instanceOf[A]

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

  def messageFromMessageKey(messageKey: String, args: Any*)(implicit messagesApi: MessagesApi): String = {
    val m = messagesApi(messageKey, args: _*)(lang)
    if (m === messageKey) sys.error(s"Message key `$messageKey` is missing a message")
    m
  }

  def checkIsTechnicalErrorPage(
    result: Future[Result]
  )(implicit messagesApi: MessagesApi): Unit = {
    (status(result), redirectLocation(result)) shouldBe (INTERNAL_SERVER_ERROR -> None)
    contentAsString(result)                      should include(
      messageFromMessageKey("global.error.InternalServerError500.title")
    )
  }

  def checkIsRedirect(
    result: Future[Result],
    expectedRedirectUrl: String
  ): Unit = {
    status(result)           shouldBe SEE_OTHER
    redirectLocation(result) shouldBe Some(expectedRedirectUrl)
  }

  def checkIsRedirect(
    result: Future[Result],
    expectedRedirectCall: Call
  ): Unit =
    checkIsRedirect(result, expectedRedirectCall.url)

  def checkPageIsDisplayed(
    result: Future[Result],
    expectedTitle: String,
    contentChecks: Document => Unit = _ => (),
    expectedStatus: Int = OK
  ): Unit = {
    (status(result), redirectLocation(result)) shouldBe (expectedStatus -> None)
    status(result)                             shouldBe expectedStatus

    val doc = Jsoup.parse(contentAsString(result))
    doc.select("h1").text should include(expectedTitle)

    val bodyText = doc.select("body").text
    val regex    = """not_found_message\((.*?)\)""".r

    val regexResult = regex.findAllMatchIn(bodyText).toList
    if (regexResult.nonEmpty) fail(s"Missing message keys: ${regexResult.map(_.group(1)).mkString(", ")}")
    else succeed

    contentChecks(doc)
  }

  def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")

}
