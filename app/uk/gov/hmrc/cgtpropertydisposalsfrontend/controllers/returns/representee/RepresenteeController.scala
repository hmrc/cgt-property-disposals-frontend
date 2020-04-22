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

import java.time.LocalDate

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import com.google.inject.Inject
import play.api.Configuration
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, NINO, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{NoReferenceId, RepresenteeCgtReference, RepresenteeNino, RepresenteeSautr}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DateOfDeath, IndividualUserType, RepresenteeAnswers, RepresenteeReferenceId}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{ConditionalRadioUtils, Error, FormUtils, TimeUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{representee => representeePages}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import play.api.data.Forms.{mapping, of}
import play.api.data.{Form, FormError}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption

class RepresenteeController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  cyaPage: representeePages.check_your_answers,
  enterNamePage: representeePages.enter_name,
  enterIdPage: representeePages.enter_reference_number,
  enterDateOfDeathPage: representeePages.enter_date_of_death
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
          formWithErrors => BadRequest(enterNamePage(formWithErrors, backLink, representativeType, journey.isRight)), {
            name =>
              val newAnswers = IncompleteRepresenteeAnswers.empty.copy(name = Some(name))

              updateDraftReturnAndSession(newAnswers, journey).fold({ e =>
                logger.warn("Could not update draft return", e)
                errorHandler.errorResult()
              }, _ => Redirect(routes.RepresenteeController.checkYourAnswers()))
          }
        )
    }
  }

  def enterId(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
      val backLink = answers.fold(
        _ =>
          representativeType.fold(
            _ => routes.RepresenteeController.enterDateOfDeath(),
            _ => routes.RepresenteeController.enterDateOfDeath()
          ),
        _ => routes.RepresenteeController.checkYourAnswers()
      )
      val form = answers.fold(_.id, c => Some(c.id)).fold(idForm)(idForm.fill)

      Ok(enterIdPage(form, backLink, journey.isRight))
    }
  }

  def enterIdSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
      lazy val backLink = answers.fold(
        _ =>
          representativeType.fold(
            _ => routes.RepresenteeController.enterDateOfDeath(),
            _ => routes.RepresenteeController.enterDateOfDeath()
          ),
        _ => routes.RepresenteeController.checkYourAnswers()
      )

      idForm
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(enterIdPage(formWithErrors, backLink, journey.isRight)),
          id =>
            if (answers.fold(_.id, c => Some(c.id)).contains(id))
              Redirect(routes.RepresenteeController.checkYourAnswers())
            else {
              val newAnswers = answers.fold(_.copy(id = Some(id)), _.copy(id = id))

              updateDraftReturnAndSession(newAnswers, journey).fold({ e =>
                logger.warn("Could not update draft return", e)
                errorHandler.errorResult()
              }, _ => Redirect(routes.RepresenteeController.checkYourAnswers()))
            }
        )

    }
  }

  def enterDateOfDeath(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
      representativeType match {
        case Right(Capacitor) => Redirect(routes.RepresenteeController.checkYourAnswers())
        case Left(PersonalRepresentative) => {
          val form =
            answers.fold(_.dateOfDeath, c => c.dateOfDeath).fold(dateOfDeathForm)(dateOfDeathForm.fill(_))
          val backLink = answers.fold(
            _ => routes.RepresenteeController.enterName(),
            _ => routes.RepresenteeController.checkYourAnswers()
          )
          Ok(enterDateOfDeathPage(form, backLink, journey.isRight))
        }
      }
    }
  }

  def enterDateOfDeathSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
      representativeType match {
        case Right(Capacitor) => Redirect(routes.RepresenteeController.checkYourAnswers())
        case Left(PersonalRepresentative) => {
          lazy val backLink = answers.fold(
            _ => routes.RepresenteeController.enterName(),
            _ => routes.RepresenteeController.checkYourAnswers()
          )
          dateOfDeathForm
            .bindFromRequest()
            .fold(
              formWithErrors => BadRequest(enterDateOfDeathPage(formWithErrors, backLink, journey.isRight)),
              dateOfDeath =>
                if (answers.fold(_.dateOfDeath, c => c.dateOfDeath).contains(dateOfDeath))
                  Redirect(routes.RepresenteeController.checkYourAnswers())
                else {
                  val newAnswers =
                    answers.fold(
                      _.copy(dateOfDeath = Some(dateOfDeath)),
                      _.copy(dateOfDeath = Some(dateOfDeath))
                    )
                  updateDraftReturnAndSession(newAnswers, journey).fold({ e =>
                    logger.warn("Could not update draft return", e)
                    errorHandler.errorResult()
                  }, _ => Redirect(routes.RepresenteeController.checkYourAnswers()))
                }
            )
        }
      }
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
        case IncompleteRepresenteeAnswers(None, _, _) =>
          Redirect(routes.RepresenteeController.enterName())

        case IncompleteRepresenteeAnswers(_, _, None) if (representativeType.isLeft) =>
          Redirect(routes.RepresenteeController.enterDateOfDeath())

        case IncompleteRepresenteeAnswers(_, None, _) =>
          Redirect(routes.RepresenteeController.enterId())

        case IncompleteRepresenteeAnswers(Some(name), Some(id), dateOfDeath) =>
          val completeAnswers = CompleteRepresenteeAnswers(name, id, dateOfDeath)
          Ok(cyaPage(completeAnswers, representativeType, journey.isRight))

        case c: CompleteRepresenteeAnswers =>
          Ok(cyaPage(c, representativeType, journey.isRight))

      }

    }
  }
}

object RepresenteeController {

  val nameForm: Form[IndividualName] = IndividualName.form("representeeFirstName", "representeeLastName")

  val key = "representee.dateOfDeath"
  val dateOfDeathForm: Form[DateOfDeath] = Form(
    mapping(
      "" -> of(
        TimeUtils.dateFormatter(
          Some(LocalDate.now().plusDays(1)),
          None,
          s"$key-day",
          s"$key-month",
          s"$key-year",
          key
        )
      )
    )(DateOfDeath(_))(d => Some(d.value))
  )

  val idForm: Form[RepresenteeReferenceId] = {
    val (outerId, ninoId, sautrId, cgtReferenceId) =
      ("representeeReferenceIdType", "representeeNino", "representeeSautr", "representeeCgtRef")

    val ninoInnerOption: InnerOption[RepresenteeNino] =
      InnerOption { data =>
        FormUtils
          .readValue(ninoId, data, identity)
          .map(_.toUpperCase.replaceAllLiterally(" ", ""))
          .flatMap(nino =>
            if (nino.length > 9)
              Left(FormError(ninoId, "error.tooLong"))
            else if (nino.length < 9)
              Left(FormError(ninoId, "error.tooShort"))
            else if (nino.exists(!_.isLetterOrDigit))
              Left(FormError(ninoId, "error.invalidCharacters"))
            else if (!Nino.isValid(nino))
              Left(FormError(ninoId, "error.pattern"))
            else
              Right(RepresenteeNino(NINO(nino)))
          )
          .leftMap(Seq(_))
      }

    val sautrInnerOption: InnerOption[RepresenteeSautr] =
      InnerOption { data =>
        FormUtils
          .readValue(sautrId, data, identity)
          .map(_.replaceAllLiterally(" ", ""))
          .flatMap(sautr =>
            if (sautr.exists(!_.isDigit))
              Left(FormError(sautrId, "error.invalid"))
            else if (sautr.length > 10)
              Left(FormError(sautrId, "error.tooLong"))
            else if (sautr.length < 10)
              Left(FormError(sautrId, "error.tooShort"))
            else
              Right(RepresenteeSautr(SAUTR(sautr)))
          )
          .leftMap(Seq(_))
      }

    val cgtReferenceInnerOption: InnerOption[RepresenteeCgtReference] =
      InnerOption(data => CgtReference.mapping.withPrefix(cgtReferenceId).bind(data).map(RepresenteeCgtReference))

    val formatter = ConditionalRadioUtils.formatter[RepresenteeReferenceId](outerId)(
      List(
        Left(cgtReferenceInnerOption),
        Left(ninoInnerOption),
        Left(sautrInnerOption),
        Right(NoReferenceId)
      )
    ) {
      case RepresenteeCgtReference(cgtReference) => Map(outerId -> "0", cgtReferenceId -> cgtReference.value)
      case RepresenteeNino(nino)                 => Map(outerId -> "1", ninoId -> nino.value)
      case RepresenteeSautr(sautr)               => Map(outerId -> "2", sautrId -> sautr.value)
      case NoReferenceId                         => Map(outerId -> "3")
    }

    Form(
      mapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

}
