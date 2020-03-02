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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage

import cats.syntax.either._
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formats._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.routes
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{FormUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{IndividualUserType, MultipleDisposalsTriageAnswers, NumberOfProperties, SingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsAnswers, IncompleteMultipleDisposalsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.triage.{multipledisposals => triagePages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.MultipleDisposalsTriageController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MultipleDisposalsTriageController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  uuidGenerator: UUIDGenerator,
  cc: MessagesControllerComponents,
  val config: Configuration,
  guidancePage: triagePages.guidance,
  howManyProperties: triagePages.how_many_properties
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  def guidance(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, _, _) =>
        Ok(guidancePage())
    }
  }

  def guidanceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, _, _) =>
        Redirect(routes.MultipleDisposalsTriageController.howManyDisposals())
    }
  }

  def howManyDisposals(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, _, answers) =>
        val numberOfDisposals = answers.fold(_.numberOfProperties, c => Some(c.numberOfProperties))
        val form              = numberOfDisposals.fold(numberOfPropertiesForm)(numberOfPropertiesForm.fill)
        Ok(howManyProperties(form))
    }
  }

  def howManyDisposalsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    def redirectTo(state: Either[StartingNewDraftReturn, FillingOutReturn]): Call =
      triageAnswersFomState(state).fold(
        _ => routes.MultipleDisposalsTriageController.checkYourAnswers(),
        _ => routes.SingleDisposalsTriageController.checkYourAnswers()
      )

    withMultipleDisposalTriageAnswers(request) {
      case (_, state, triageAnswers) =>
        numberOfPropertiesForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                howManyProperties(formWithErrors)
              ), { numberOfProperties =>
              val np           = getNumberOfProperties(state)
              val updatedState = updateNumberOfProperties(state, np)
            }
          )

        Ok("not implemented yet")
    }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, _, triageAnswers) =>
        triageAnswers match {
          case IncompleteMultipleDisposalsAnswers(None, None) =>
            Redirect(routes.InitialTriageQuestionsController.howManyProperties())

          case IncompleteMultipleDisposalsAnswers(Some(_), _) =>
            Redirect(routes.MultipleDisposalsTriageController.guidance())

          case IncompleteMultipleDisposalsAnswers(Some(_), None) =>
            Redirect(routes.MultipleDisposalsTriageController.howManyDisposals())

          case c: CompleteMultipleDisposalsAnswers =>
            Ok(s"Got $c")

        }
    }
  }

  private def getIndividualUserType(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Option[IndividualUserType] =
    triageAnswersFomState(state).fold(
      _.fold(_.individualUserType, c => Some(c.individualUserType)),
      _.fold(_.individualUserType, c => Some(c.individualUserType))
    )

  private def updateNumberOfProperties(
    state: Either[StartingNewDraftReturn, FillingOutReturn],
    numberOfProperties: Option[Int]
  ): Either[StartingNewDraftReturn, FillingOutReturn] = {
    val individualUserType = getIndividualUserType(state)
    numberOfProperties match {
      case Some(1) =>
        val newTriageAnswers =
          IncompleteSingleDisposalTriageAnswers.empty.copy(
            individualUserType         = individualUserType,
            hasConfirmedSingleDisposal = true
          )

        state.bimap(
          _.copy(newReturnTriageAnswers = Right(newTriageAnswers)),
          fillingOutReturn =>
            fillingOutReturn.copy(
              draftReturn = fillingOutReturn.draftReturn.copy(
                triageAnswers = newTriageAnswers
              )
            )
        )

      case Some(2) =>
        val newTriageAnswers =
          IncompleteMultipleDisposalsAnswers.empty.copy(
            individualUserType = individualUserType,
            numberOfProperties = Some(2)
          )

        state.bimap(
          _.copy(newReturnTriageAnswers = Left(newTriageAnswers)),
          fillingOutReturn =>
            fillingOutReturn.copy(
              draftReturn = fillingOutReturn.draftReturn.copy(
                triageAnswers = newTriageAnswers
              )
            )
        )
    }
  }

  private def getNumberOfProperties(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Option[Int] =
    state.fold(
      _.newReturnTriageAnswers.fold(
        _ => Some(2),
        _.fold(
          incomplete => if (incomplete.hasConfirmedSingleDisposal) Some(1) else None,
          _ => Some(1)
        )
      ),
      _ => Some(1)
    )

  private def triageAnswersFomState(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers] =
    state.bimap(_.newReturnTriageAnswers, r => Right(r.draftReturn.triageAnswers)).merge

  private def withMultipleDisposalTriageAnswers(request: RequestWithSessionData[_])(
    f: (SessionData, Either[StartingNewDraftReturn, FillingOutReturn], MultipleDisposalsTriageAnswers) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((session, s @ StartingNewDraftReturn(_, _, _, Left(t)))) =>
        f(session, Left(s), t)

      case _ =>
        Redirect(uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController.start())
    }

}
object MultipleDisposalsTriageController {

  val numberOfPropertiesForm: Form[Int] = Form(
    mapping(
      "numberOfProperties" -> of[Int]
    )(identity)(Some(_))
  )

}
