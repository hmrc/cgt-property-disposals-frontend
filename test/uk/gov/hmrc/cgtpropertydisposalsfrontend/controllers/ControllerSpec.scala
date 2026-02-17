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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import com.google.inject.{Inject, Singleton}
import com.typesafe.config.ConfigFactory
import org.apache.pekko.stream.Materializer
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalactic.anyvals.{PosInt, PosZInt}
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.*
import play.api.http.HttpConfiguration
import play.api.i18n.*
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.mvc.{Call, Result}
import play.api.test.Helpers.*
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.{Metrics, TestMetrics}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.SampledScalaCheck

import java.net.URLEncoder
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.Try

@Singleton
class TestMessagesApi(
  messages: Map[String, Map[String, String]],
  langs: Langs,
  langCookieName: String,
  langCookieSecure: Boolean,
  langCookieHttpOnly: Boolean,
  httpConfiguration: HttpConfiguration
) extends DefaultMessagesApi(
      messages = messages,
      langs = langs,
      langCookieName = langCookieName,
      langCookieSecure = langCookieSecure,
      langCookieHttpOnly = langCookieHttpOnly,
      httpConfiguration = httpConfiguration
    )
    with Logging {

  override protected def noMatch(key: String, args: Seq[Any])(implicit lang: Lang): String = {
    logger.error(s"Could not find message for key: $key ${args.mkString("-")}")
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

  override lazy val get: MessagesApi =
    new TestMessagesApi(
      loadAllMessages,
      langs,
      langCookieName = langCookieName,
      langCookieSecure = langCookieSecure,
      langCookieHttpOnly = langCookieHttpOnly,
      httpConfiguration = httpConfiguration
    )
}

trait ControllerSpec extends AnyWordSpec with Matchers with BeforeAndAfterAll with MockFactory with SampledScalaCheck {

  private def toInt(key: String, default: Int): Int =
    sys.props.get(key).flatMap(value => Try(value.toInt).toOption).filter(_ > 0).getOrElse(default)

  implicit val lang: Lang = Lang("en")

  private val sampleMinSuccessful: Int = toInt("controllerSpec.minSuccessful", default = 6)
  private val sampleSizeRange: Int     = toInt("controllerSpec.sizeRange", default = 5)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = PosInt.ensuringValid(sampleMinSuccessful),
      sizeRange = PosInt.ensuringValid(sampleSizeRange),
      maxDiscardedFactor = 2.0
    )

  protected def overrideBindings: List[GuiceableModule] = List.empty[GuiceableModule]

  protected lazy val additionalConfig: Configuration = Configuration()

  def buildFakeApplication(): Application = {
    val metricsBinding: GuiceableModule = bind[Metrics].toInstance(new TestMetrics)

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
        ).withFallback(additionalConfig)
      )
      .disable[uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore]
      .overrides(metricsBinding :: overrideBindings*)
      .overrides(bind[MessagesApi].toProvider[TestDefaultMessagesApiProvider])
      .disable[Metrics]
      .build()
  }

  lazy val fakeApplication: Application = buildFakeApplication()

  def instanceOf[A : ClassTag]: A = fakeApplication.injector.instanceOf[A]

  lazy implicit val materializer: Materializer = fakeApplication.materializer
  protected lazy val viewConfig: ViewConfig    = instanceOf[ViewConfig]

  abstract override def beforeAll(): Unit = {
    Play.start(fakeApplication)
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    Play.stop(fakeApplication)
    super.afterAll()
  }

  def messageFromMessageKey(messageKey: String, args: Any*)(implicit messagesApi: MessagesApi): String = {
    val m = messagesApi(messageKey, args*)(using lang)
    if (m === messageKey) sys.error(s"Message key `$messageKey` is missing a message")
    m
  }

  def checkIsTechnicalErrorPage(
    result: Future[Result]
  )(implicit messagesApi: MessagesApi): Unit = {
    import cats.instances.int._
    import cats.syntax.eq._
    if (status(result) =!= INTERNAL_SERVER_ERROR) println(contentAsString(result))

    (status(result), redirectLocation(result)) shouldBe (INTERNAL_SERVER_ERROR -> None)
    contentAsString(result)                      should include(
      messageFromMessageKey("global.error.InternalServerError500.title")
    )
  }

  def checkIsRedirect(
    result: Future[Result],
    expectedRedirectUrl: String
  ): Unit = {
    import cats.instances.int._
    import cats.syntax.eq._
    if (status(result) =!= SEE_OTHER) println(contentAsString(result))

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
    expectedStatus: Int = OK,
    messageRegexPrefix: String = "not_found_message"
  ): Unit = {
    (status(result), redirectLocation(result)) shouldBe (expectedStatus -> None)
    status(result)                             shouldBe expectedStatus

    val doc = Jsoup.parse(contentAsString(result))
    doc.select("h1").text should include(expectedTitle)

    val bodyText = doc.select("body").text
    val regex    = raw"""$messageRegexPrefix\((.*?)\)""".r

    regex.findAllMatchIn(bodyText).toList match {
      case Nil         => contentChecks(doc)
      case regexResult => fail(s"Missing message keys: ${regexResult.map(_.group(1)).mkString(", ")}")
    }
  }

  def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")

}
