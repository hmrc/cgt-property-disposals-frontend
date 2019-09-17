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

import cats.data.EitherT
import cats.instances.future._
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.{Form, FormError}
import play.api.data.Forms.{mapping, of, optional, text}
import play.api.data.format.Formatter
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{HasSAUTR, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.BusinessPartnerRecordService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views
import uk.gov.hmrc.referencechecker.SelfAssessmentReferenceChecker

import scala.concurrent.{ExecutionContext, Future}

class InsufficientConfidenceLevelController @Inject()(
                                                       val authenticatedAction: AuthenticatedAction,
                                                       val sessionDataAction: SessionDataAction,
                                                       val sessionStore: SessionStore,
                                                       val errorHandler: ErrorHandler,
                                                       val config: Configuration,
                                                       businessPartnerRecordService: BusinessPartnerRecordService,
                                                       doYouHaveANinoPage: views.html.do_you_have_a_nino,
                                                       doYouHaveAnSaUtrPage: views.html.do_you_have_an_sa_utr,
                                                       startRegistrationPage: views.html.registration.registration_start,
                                                       cc: MessagesControllerComponents)
                                                     (
                                                       implicit viewConfig: ViewConfig,
                                                       ec: ExecutionContext
                                                     )
  extends FrontendController(cc)
    with IvBehaviour
    with Logging
    with WithAuthAndSessionDataAction
    with DefaultRedirects
    with SessionUpdates {
  import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.toFuture
  import InsufficientConfidenceLevelController._

  private def withInsufficientConfidenceLevelUser(
    f: IndividualWithInsufficientConfidenceLevel => Future[Result])(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(i: IndividualWithInsufficientConfidenceLevel) => f(i)
      case other                             => defaultRedirect(other)
    }


  def doYouHaveNINO(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser{ case IndividualWithInsufficientConfidenceLevel(hasNino, _, _, _) =>
      val form = hasNino.fold(haveANinoForm)(haveANinoForm.fill)
      Ok(doYouHaveANinoPage(form))
    }
  }

  def doYouHaveNINOSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser { insufficientConfidenceLevel =>
      InsufficientConfidenceLevelController.haveANinoForm.bindFromRequest().fold(
        e => BadRequest(doYouHaveANinoPage(e)),
        hasNino =>
          updateSession(sessionStore, request)(
            _.copy(journeyStatus = Some(insufficientConfidenceLevel.copy(hasNino = Some(hasNino))))
          ).map{
            case Left(e) =>
            logger.warn("Could not update session after has NINO page submit",e )
            errorHandler.errorResult()

            case Right(_) =>
              if (hasNino) {
                redirectToIv
              } else {
                Redirect(routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr())
              }
          }
     )
    }
  }

  def doYouHaveAnSaUtr(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser{ case IndividualWithInsufficientConfidenceLevel(hasNino, hasSaUtr, _, _) =>
      hasNino.fold(
        SeeOther(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)
      ){ _ =>
        val form = hasSaUtr.fold(hasSaUtrForm)(s => hasSaUtrForm.fill(s.value))
        Ok(doYouHaveAnSaUtrPage(form, routes.InsufficientConfidenceLevelController.doYouHaveNINO()))
      }
    }
  }

  def doYouHaveSaUtrSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser { insufficientConfidenceLevel =>
    insufficientConfidenceLevel.hasNino.fold[Future[Result]](
      SeeOther(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)
    ) { _ =>
      hasSaUtrForm.bindFromRequest().fold(
        e => BadRequest(doYouHaveAnSaUtrPage(e, routes.InsufficientConfidenceLevelController.doYouHaveNINO())),
        maybeSautr =>
          updateSession(sessionStore, request)(
            _.copy(journeyStatus = Some(insufficientConfidenceLevel.copy(hasSautr = Some(HasSAUTR(maybeSautr)))))
          ).map {
            case Left(e) =>
              logger.warn("Could not update session after has SAUTR page submit", e)
              errorHandler.errorResult()

            case Right(_) =>
              maybeSautr.fold(
                Redirect(routes.RegistrationController.startRegistration())
              ){ _ =>
                Redirect(routes.StartController.start())
              }
          }
      )
    }
    }
  }

}


object InsufficientConfidenceLevelController {

  //don't want to use out-of-box boolean formatter - that one defaults null values to false
  val booleanFormatter: Formatter[Boolean] = new Formatter[Boolean] {

    override val format = Some(("format.boolean", Nil))

    def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Boolean] = {
      Either.fromOption(data.get(key), Seq(FormError(key, "error.required")))
        .flatMap{
          case "true"  => Right(true)
          case "false" => Right(false)
          case _       => Left(Seq(FormError(key, "error.boolean", Nil)))
        }
    }

    def unbind(key: String, value: Boolean): Map[String, String] = Map(key -> value.toString)
  }


  val haveANinoForm: Form[Boolean] =
    Form(
      mapping(
        "hasNino" -> of(booleanFormatter)
      )(identity)(Some(_))
    )


  val hasSaUtrForm: Form[Option[SAUTR]] = {
      val saUtrFormatter = new Formatter[Option[String]] {
        override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Option[String]] =
          data.get("hasSaUtr") match {
            // if "true" make sure we have an saUtr
            case Some("true") ⇒
              data.get("saUtr").filter(_.trim().nonEmpty) match {
                case Some(s) => if( SelfAssessmentReferenceChecker.isValid(s)) Right(Some(s)) else Left(Seq(FormError(key, "error.pattern")))
                case None => Left(Seq(FormError(key, "error.required")))
              }
            // if the value is "false" ignore any new saUtr that has been entered
            case _          ⇒ Right(None)
          }

        override def unbind(key: String, value: Option[String]): Map[String, String] =
          optional(text).withPrefix(key).unbind(value)
      }

      Form(
        mapping(
          "hasSaUtr" → text.verifying(l ⇒ l === "true" || l === "false"),
          "saUtr" → of(saUtrFormatter)
        ){
          case (_, sautr) => sautr.map(SAUTR(_))
        }{
          maybeSaUtr =>
            Some(maybeSaUtr.fold[(String,Option[String])]("false" -> None)(s => "true" -> Some(s.value)))
        }
      )
  }
}
