package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers

import javax.inject.{Inject, Singleton}

import play.api.mvc._

import scala.concurrent.Future
import play.api.i18n.{I18nSupport, MessagesApi}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.AppConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views

@Singleton
class HelloWorld @Inject()(val messagesApi: MessagesApi, implicit val appConfig: AppConfig) extends FrontendController with I18nSupport {

  val helloWorld = Action.async { implicit request =>
    Future.successful(Ok(views.html.hello_world()))
  }

}
