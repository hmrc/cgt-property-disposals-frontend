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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Error
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{IndividualUserType, RepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{representee => representeePages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class RepresenteeController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  cyaPage: representeePages.check_your_answers,
  enterNamePage: representeePages.enter_name
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  import RepresenteeController._

  val capacitorsAndPersonalRepresentativesJourneyEnabled: Boolean =
    config.underlying.getBoolean("capacitors-and-personal-representatives.enabled")

  private def withCapacitorOrPersonalRepresentativeAnswers(request: RequestWithSessionData[_])(
    f: (
      Either[PersonalRepresentative.type, Capacitor.type],
      Either[StartingNewDraftReturn, FillingOutReturn],
      RepresenteeAnswers
    ) => Future[Result]
  ): Future[Result] = {
    def performAction(
      individualUserType: Option[IndividualUserType],
      journey: Either[StartingNewDraftReturn, FillingOutReturn],
      answers: RepresenteeAnswers
    ): Future[Result] =
      individualUserType match {
        case Some(PersonalRepresentative) =>
          if (capacitorsAndPersonalRepresentativesJourneyEnabled)
            f(Left(PersonalRepresentative), journey, answers)
          else
            Redirect(
              controllers.returns.triage.routes.CommonTriageQuestionsController
                .capacitorsAndPersonalRepresentativesNotHandled()
            )
        case Some(Capacitor) =>
          if (capacitorsAndPersonalRepresentativesJourneyEnabled)
            f(Right(Capacitor), journey, answers)
          else
            Redirect(
              controllers.returns.triage.routes.CommonTriageQuestionsController
                .capacitorsAndPersonalRepresentativesNotHandled()
            )
        case _ =>
          Redirect(controllers.returns.routes.TaskListController.taskList())
      }

    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(startingNewDraftReturn: StartingNewDraftReturn) =>
        val individualUserType =
          startingNewDraftReturn.newReturnTriageAnswers.fold(
            _.fold(_.individualUserType, _.individualUserType),
            _.fold(_.individualUserType, _.individualUserType)
          )
        val answers = startingNewDraftReturn.representeeAnswers.getOrElse(IncompleteRepresenteeAnswers.empty)
        performAction(individualUserType, Left(startingNewDraftReturn), answers)

      case Some(fillingOutReturn: FillingOutReturn) =>
        val individualUserType =
          fillingOutReturn.draftReturn.fold(
            _.triageAnswers.fold(_.individualUserType, _.individualUserType),
            _.triageAnswers.fold(_.individualUserType, _.individualUserType)
          )
        val answers = fillingOutReturn.draftReturn
          .fold(
            _.representeeAnswers,
            _.representeeAnswers
          )
          .getOrElse(IncompleteRepresenteeAnswers.empty)
        performAction(individualUserType, Right(fillingOutReturn), answers)

      case _ =>
        Redirect(controllers.routes.StartController.start())
    }
  }

  def enterName(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
      val backLink = answers.fold(
        _ => controllers.returns.triage.routes.CommonTriageQuestionsController.whoIsIndividualRepresenting(),
        _ => routes.RepresenteeController.checkYourAnswers()
      )
      val form = answers.fold(_.name, c => Some(c.name)).fold(nameForm)(nameForm.fill)

      Ok(enterNamePage(form, backLink, representativeType, journey.isRight))
    }
  }

  def enterNameSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
      lazy val backLink = answers.fold(
        _ => controllers.returns.triage.routes.CommonTriageQuestionsController.whoIsIndividualRepresenting(),
        _ => routes.RepresenteeController.checkYourAnswers()
      )

      nameForm
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(enterNamePage(formWithErrors, backLink, representativeType, journey.isRight)),
          name =>
            if (answers.fold(_.name, c => Some(c.name)).contains(name))
              Redirect(routes.RepresenteeController.checkYourAnswers())
            else {
              val newAnswers = IncompleteRepresenteeAnswers.empty.copy(name = Some(name))

              updateDraftReturnAndSession(newAnswers, journey).fold({ e =>
                logger.warn("Could not update draft return", e)
                errorHandler.errorResult()
              }, _ => Redirect(routes.RepresenteeController.checkYourAnswers()))

            }
        )

    }
  }

  private def updateDraftReturnAndSession(
    newAnswers: RepresenteeAnswers,
    currentJourney: Either[StartingNewDraftReturn, FillingOutReturn]
  )(implicit request: RequestWithSessionData[_]): EitherT[Future, Error, Unit] = {
    val newJourney =
      currentJourney.bimap(
        _.copy(representeeAnswers = Some(newAnswers)),
        fillingOutReturn =>
          fillingOutReturn.copy(draftReturn =
            fillingOutReturn.draftReturn.fold(
              _.copy(representeeAnswers = Some(newAnswers)),
              _.copy(representeeAnswers = Some(newAnswers))
            )
          )
      )

    for {
      _ <- newJourney.fold(
            _ => EitherT.pure[Future, Error](()),
            newFillingOutReturn =>
              returnsService.storeDraftReturn(
                newFillingOutReturn.draftReturn,
                newFillingOutReturn.subscribedDetails.cgtReference,
                newFillingOutReturn.agentReferenceNumber
              )
          )
      _ <- EitherT(
            updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newJourney.merge)))
          )
    } yield ()
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
      answers match {
        case IncompleteRepresenteeAnswers(None) =>
          Redirect(routes.RepresenteeController.enterName())

        case IncompleteRepresenteeAnswers(Some(name)) =>
          val completeAnswers = CompleteRepresenteeAnswers(name)
          Ok(cyaPage(completeAnswers, representativeType, journey.isRight))

        case c: CompleteRepresenteeAnswers =>
          Ok(cyaPage(c, representativeType, journey.isRight))

      }

    }
  }
}

object RepresenteeController {

  val nameForm: Form[IndividualName] = IndividualName.form("representeeFirstName", "representeeLastName")

}
