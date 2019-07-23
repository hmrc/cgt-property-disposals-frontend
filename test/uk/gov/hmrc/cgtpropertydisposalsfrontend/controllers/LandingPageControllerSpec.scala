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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import play.api.i18n.{Lang, MessagesApi}
import play.api.{Application, Configuration, Play}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.MessagesControllerComponents
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views

class LandingPageControllerSpec extends WordSpec with Matchers with BeforeAndAfterAll {

  def buildFakeApplication(): Application =
    new GuiceApplicationBuilder()
      .configure(Configuration(
        ConfigFactory.parseString(
          """
          | metrics.enabled       = false
        """.stripMargin
        )
      )).build()

  lazy val fakeApplication: Application = buildFakeApplication()

  override def beforeAll(): Unit = {
    Play.start(fakeApplication)
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    Play.stop(fakeApplication)
    super.afterAll()
  }

  lazy val controller: LandingPageController = new LandingPageController(
    fakeApplication.injector.instanceOf[MessagesControllerComponents],
    fakeApplication.injector.instanceOf[views.html.landing_page]
  )(fakeApplication.injector.instanceOf[ViewConfig])

  "The LandingPageController" must {

    "display the landing page" in {

      contentAsString(controller.landingPage()(FakeRequest())) should include(controller.messagesApi("landingPage.title")(Lang.defaultLang))
    }

  }

}
