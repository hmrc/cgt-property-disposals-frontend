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
import play.api.data.Forms.{mapping, of}
import play.api.data.{Form, FormError}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee.RepresenteeController.NameMatchError.{ServiceError, ValidationError}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.{routes => triageRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails.IndividualRepresenteeNameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.{CompleteRepresenteeAnswers, IncompleteRepresenteeAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{NoReferenceId, RepresenteeCgtReference, RepresenteeNino, RepresenteeSautr}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{IndividualUserType, RepresenteeAnswers, RepresenteeReferenceId, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, ConditionalRadioUtils, Error, FormUtils, NameMatchServiceError, TimeUtils, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.NameMatchRetryService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{representee => representeePages}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class RepresenteeController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  nameMatchRetryService: NameMatchRetryService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  cyaPage: representeePages.check_your_answers,
  enterNamePage: representeePages.enter_name,
  enterIdPage: representeePages.enter_reference_number,
  enterDateOfDeathPage: representeePages.enter_date_of_death,
  checkContactDetailsPage: representeePages.check_contact_details,
  confirmPersonPage: representeePages.confirm_person,
  nameMatchErrorPage: representeePages.name_match_error,
  changeContactNamePage: representeePages.change_contact_name,
  tooManyNameMatchFailuresPage: representeePages.too_many_name_match_failures
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  import RepresenteeController._

  private def withCapacitorOrPersonalRepresentativeAnswers(
    request: RequestWithSessionData[_]
  )(
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
          f(Left(PersonalRepresentative), journey, answers)

        case Some(Capacitor)              =>
          f(Right(Capacitor), journey, answers)

        case _                            =>
          Redirect(controllers.returns.routes.TaskListController.taskList())
      }

    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(startingNewDraftReturn: StartingNewDraftReturn) =>
        val individualUserType =
          startingNewDraftReturn.newReturnTriageAnswers.fold(
            _.fold(_.individualUserType, _.individualUserType),
            _.fold(_.individualUserType, _.individualUserType)
          )
        val answers            = startingNewDraftReturn.representeeAnswers.getOrElse(
          IncompleteRepresenteeAnswers.empty
        )
        performAction(individualUserType, Left(startingNewDraftReturn), answers)

      case Some(fillingOutReturn: FillingOutReturn)             =>
        val individualUserType =
          fillingOutReturn.draftReturn.fold(
            _.triageAnswers.fold(_.individualUserType, _.individualUserType),
            _.triageAnswers.fold(_.individualUserType, _.individualUserType),
            _.triageAnswers.fold(_.individualUserType, _.individualUserType),
            _.triageAnswers.fold(_.individualUserType, _.individualUserType)
          )
        val answers            = fillingOutReturn.draftReturn
          .fold(
            _.representeeAnswers,
            _.representeeAnswers,
            _.representeeAnswers,
            _.representeeAnswers
          )
          .getOrElse(IncompleteRepresenteeAnswers.empty)
        performAction(individualUserType, Right(fillingOutReturn), answers)

      case _                                                    =>
        Redirect(controllers.routes.StartController.start())
    }
  }

  private def withNotTooManyUnsuccessfulNameMatchAttempts(ggCredId: GGCredId)(
    f: Option[
      UnsuccessfulNameMatchAttempts[IndividualRepresenteeNameMatchDetails]
    ] => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    nameMatchRetryService
      .getNumberOfUnsuccessfulAttempts[IndividualRepresenteeNameMatchDetails](
        ggCredId
      )
      .foldF[Result](
        handleNameMatchServiceError(_),
        f
      )

  def enterName(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
        withNotTooManyUnsuccessfulNameMatchAttempts(
          journey.fold(_.ggCredId, _.ggCredId)
        ) { _ =>
          val backLink = answers.fold(
            _ =>
              controllers.returns.triage.routes.CommonTriageQuestionsController
                .whoIsIndividualRepresenting(),
            _ => routes.RepresenteeController.checkYourAnswers()
          )
          val form     = answers
            .fold(_.name, c => Some(c.name))
            .fold(nameForm)(nameForm.fill)

          Ok(
            enterNamePage(form, backLink, representativeType, journey.isRight)
          )
        }
      }
    }

  def enterNameSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
        withNotTooManyUnsuccessfulNameMatchAttempts(
          journey.fold(_.ggCredId, _.ggCredId)
        ) { _ =>
          lazy val backLink = answers.fold(
            _ =>
              controllers.returns.triage.routes.CommonTriageQuestionsController
                .whoIsIndividualRepresenting(),
            _ => routes.RepresenteeController.checkYourAnswers()
          )

          nameForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  enterNamePage(
                    formWithErrors,
                    backLink,
                    representativeType,
                    journey.isRight
                  )
                ),
              { name =>
                val newAnswers =
                  IncompleteRepresenteeAnswers.empty.copy(name = Some(name))

                updateDraftReturnAndSession(newAnswers, journey).fold(
                  { e =>
                    logger.warn("Could not update draft return", e)
                    errorHandler.errorResult()
                  },
                  _ => Redirect(routes.RepresenteeController.checkYourAnswers())
                )
              }
            )
        }
      }
    }

  def confirmPerson(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
        answers match {
          case IncompleteRepresenteeAnswers(
                Some(name),
                Some(id),
                _,
                None,
                false,
                false
              ) =>
            Ok(
              confirmPersonPage(
                id,
                name,
                representativeType,
                journey.isRight,
                confirmPersonForm,
                routes.RepresenteeController.enterId()
              )
            )

          case _ => Redirect(routes.RepresenteeController.checkYourAnswers())
        }
      }
    }

  def confirmPersonSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
        answers match {
          case incompleteRepresenteeAnswers @ IncompleteRepresenteeAnswers(
                Some(name),
                Some(id),
                _,
                _,
                _,
                _
              ) =>
            confirmPersonForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    confirmPersonPage(
                      id,
                      name,
                      representativeType,
                      journey.isRight,
                      formWithErrors,
                      routes.RepresenteeController.enterId()
                    )
                  ),
                { isYes =>
                  val newAnswers =
                    if (isYes)
                      incompleteRepresenteeAnswers.copy(
                        hasConfirmedPerson = true,
                        hasConfirmedContactDetails = false,
                        contactDetails = None
                      )
                    else IncompleteRepresenteeAnswers.empty

                  updateDraftReturnAndSession(newAnswers, journey)
                    .fold(
                      { e =>
                        logger.warn("Could not update draft return", e)
                        errorHandler.errorResult()
                      },
                      _ =>
                        Redirect(
                          routes.RepresenteeController.checkYourAnswers()
                        )
                    )
                }
              )

          case _ => Redirect(routes.RepresenteeController.checkYourAnswers())

        }

      }
    }

  def enterId(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
        withNotTooManyUnsuccessfulNameMatchAttempts(
          journey.fold(_.ggCredId, _.ggCredId)
        ) { _ =>
          val backLink = answers.fold(
            _ =>
              representativeType.fold(
                _ => routes.RepresenteeController.enterDateOfDeath(),
                _ => routes.RepresenteeController.enterName()
              ),
            _ => routes.RepresenteeController.checkYourAnswers()
          )
          val form     =
            answers.fold(_.id, c => Some(c.id)).fold(idForm)(idForm.fill)

          Ok(enterIdPage(form, backLink, journey.isRight))
        }
      }
    }

  def enterIdSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
        val ggCredId = journey.fold(_.ggCredId, _.ggCredId)
        withNotTooManyUnsuccessfulNameMatchAttempts(ggCredId) { unsuccessfulNameMatchAttempts =>
          answers.fold(_.name, c => Some(c.name)) match {
            case None       =>
              Redirect(routes.RepresenteeController.checkYourAnswers())

            case Some(name) =>
              lazy val backLink = answers.fold(
                _ =>
                  representativeType.fold(
                    _ => routes.RepresenteeController.enterDateOfDeath(),
                    _ => routes.RepresenteeController.enterDateOfDeath()
                  ),
                _ => routes.RepresenteeController.checkYourAnswers()
              )

              val result = for {
                matchedId <- idForm
                               .bindFromRequest()
                               .fold(
                                 formWithErrors =>
                                   EitherT.leftT(
                                     ValidationError(formWithErrors)
                                   ),
                                 id =>
                                   nameMatchRetryService
                                     .attemptNameMatch(
                                       IndividualRepresenteeNameMatchDetails(
                                         name,
                                         id
                                       ),
                                       ggCredId,
                                       unsuccessfulNameMatchAttempts
                                     )
                                     .leftMap(ServiceError)
                               )
                _         <- updateDraftReturnAndSession(
                               IncompleteRepresenteeAnswers.empty.copy(
                                 name = answers.fold(_.name, c => Some(c.name)),
                                 dateOfDeath = answers.fold(_.dateOfDeath, _.dateOfDeath),
                                 id = Some(matchedId)
                               ),
                               journey
                             ).leftMap[NameMatchError](e => ServiceError(NameMatchServiceError.BackendError(e)))
              } yield ()

              result.fold(
                {
                  case ValidationError(formWithErrors) =>
                    BadRequest(
                      enterIdPage(formWithErrors, backLink, journey.isRight)
                    )
                  case ServiceError(e)                 =>
                    handleNameMatchServiceError(e)
                },
                _ => Redirect(routes.RepresenteeController.checkYourAnswers())
              )
          }

        }
      }

    }

  def tooManyNameMatchAttempts(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request)((_, _, _) => Ok(tooManyNameMatchFailuresPage()))
    }

  def enterDateOfDeath(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
        representativeType match {
          case Right(Capacitor)             =>
            Redirect(routes.RepresenteeController.checkYourAnswers())
          case Left(PersonalRepresentative) =>
            val form     =
              answers
                .fold(_.dateOfDeath, c => c.dateOfDeath)
                .fold(dateOfDeathForm)(dateOfDeathForm.fill(_))
            val backLink = answers.fold(
              _ => routes.RepresenteeController.enterName(),
              _ => routes.RepresenteeController.checkYourAnswers()
            )
            Ok(enterDateOfDeathPage(form, backLink, journey.isRight))
        }
      }
    }

  def enterDateOfDeathSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
        representativeType match {
          case Right(Capacitor)             =>
            Redirect(routes.RepresenteeController.checkYourAnswers())
          case Left(PersonalRepresentative) =>
            lazy val backLink = answers.fold(
              _ => routes.RepresenteeController.enterName(),
              _ => routes.RepresenteeController.checkYourAnswers()
            )
            dateOfDeathForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    enterDateOfDeathPage(
                      formWithErrors,
                      backLink,
                      journey.isRight
                    )
                  ),
                dateOfDeath =>
                  if (
                    answers
                      .fold(_.dateOfDeath, c => c.dateOfDeath)
                      .contains(dateOfDeath)
                  )
                    Redirect(routes.RepresenteeController.checkYourAnswers())
                  else {
                    val newAnswers =
                      IncompleteRepresenteeAnswers.empty.copy(
                        name = answers.fold(_.name, c => Some(c.name)),
                        dateOfDeath = Some(dateOfDeath)
                      )
                    updateDraftReturnAndSession(newAnswers, journey).fold(
                      { e =>
                        logger.warn("Could not update draft return", e)
                        errorHandler.errorResult()
                      },
                      _ =>
                        Redirect(
                          routes.RepresenteeController.checkYourAnswers()
                        )
                    )
                  }
              )
        }
      }
    }

  def changeContactName(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (_, journey, answers) =>
        answers
          .fold(i => i.contactDetails, c => Some(c.contactDetails)) match {
          case None =>
            Redirect(routes.RepresenteeController.checkYourAnswers())
          case _    =>
            val backLink = answers.fold(
              _ => routes.RepresenteeController.checkContactDetails(),
              _ => routes.RepresenteeController.checkYourAnswers()
            )

            Ok(
              changeContactNamePage(
                ContactName.form,
                backLink,
                journey.isRight
              )
            )
        }
      }
    }

  def changeContactNameSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (_, journey, answers) =>
        val backLink = answers.fold(
          _ => routes.RepresenteeController.checkContactDetails(),
          _ => routes.RepresenteeController.checkYourAnswers()
        )
        ContactName.form
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                changeContactNamePage(
                  formWithErrors,
                  backLink,
                  journey.isRight
                )
              ),
            contactName =>
              if (
                answers
                  .fold(
                    _.contactDetails.map(_.contactName),
                    c => Some(c.contactDetails.contactName)
                  )
                  .contains(contactName)
              )
                Redirect(routes.RepresenteeController.checkYourAnswers())
              else {
                val newAnswers =
                  answers.fold(
                    i =>
                      i.copy(
                        contactDetails = i.contactDetails
                          .map(_.copy(contactName = contactName)),
                        hasConfirmedContactDetails = false
                      ),
                    c =>
                      IncompleteRepresenteeAnswers(
                        Some(c.name),
                        Some(c.id),
                        c.dateOfDeath,
                        Some(
                          c.contactDetails.copy(contactName = contactName)
                        ),
                        true,
                        false
                      )
                  )
                updateDraftReturnAndSession(
                  newAnswers,
                  journey,
                  clearDraftReturn = false
                ).fold(
                  { e =>
                    logger.warn("Could not update draft return", e)
                    errorHandler.errorResult()
                  },
                  _ => Redirect(routes.RepresenteeController.checkYourAnswers())
                )
              }
          )

      }
    }

  def checkContactDetails(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (_, journey, answers) =>
        val contactDetails = answers match {
          case c: CompleteRepresenteeAnswers                                 =>
            EitherT.pure[Future, Error](c.contactDetails)

          case IncompleteRepresenteeAnswers(_, _, _, Some(details), _, _)    =>
            EitherT.pure[Future, Error](details)

          case incompleteWithoutContactDetails: IncompleteRepresenteeAnswers =>
            val subscribedDetails     =
              journey.fold(_.subscribedDetails, _.subscribedDetails)
            val defaultContactDetails = RepresenteeContactDetails(
              subscribedDetails.contactName,
              subscribedDetails.address,
              subscribedDetails.emailAddress
            )
            val newAnswers            = incompleteWithoutContactDetails.copy(
              contactDetails = Some(defaultContactDetails),
              hasConfirmedContactDetails = false
            )

            updateDraftReturnAndSession(newAnswers, journey).map(_ => defaultContactDetails)
        }

        contactDetails.fold(
          { e =>
            logger.warn("Could not get representee contact details", e)
            errorHandler.errorResult()
          },
          details => Ok(checkContactDetailsPage(details, journey.isRight))
        )
      }
    }

  def checkContactDetailsSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (_, journey, answers) =>
        answers match {
          case i @ IncompleteRepresenteeAnswers(_, _, _, Some(_), _, false) =>
            updateDraftReturnAndSession(
              i.copy(hasConfirmedContactDetails = true),
              journey,
              clearDraftReturn = false
            ).fold(
              { e =>
                logger.warn("Could not update draft return or session", e)
                errorHandler.errorResult()
              },
              _ => Redirect(routes.RepresenteeController.checkYourAnswers())
            )

          case _                                                            => Redirect(routes.RepresenteeController.checkYourAnswers())
        }
      }
    }

  def nameMatchError(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request)((_, _, _) => Ok(nameMatchErrorPage()))
    }

  def nameMatchErrorSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (_, _, _) =>
        Redirect(routes.RepresenteeController.enterName())
      }
    }

  private def updateDraftReturnAndSession(
    newAnswers: RepresenteeAnswers,
    currentJourney: Either[StartingNewDraftReturn, FillingOutReturn],
    clearDraftReturn: Boolean = true
  )(implicit
    request: RequestWithSessionData[_]
  ): EitherT[Future, Error, Unit] = {
    val newJourney =
      currentJourney.bimap(
        _.copy(representeeAnswers = Some(newAnswers)),
        fillingOutReturn => {
          val individualUserType = fillingOutReturn.draftReturn.fold(
            _.triageAnswers.fold(_.individualUserType, _.individualUserType),
            _.triageAnswers.fold(_.individualUserType, _.individualUserType),
            _.triageAnswers.fold(_.individualUserType, _.individualUserType),
            _.triageAnswers.fold(_.individualUserType, _.individualUserType)
          )

          val newDraftReturn =
            if (clearDraftReturn)
              DraftSingleDisposalReturn.newDraftReturn(
                fillingOutReturn.draftReturn.id,
                IncompleteSingleDisposalTriageAnswers.empty.copy(
                  individualUserType = individualUserType
                ),
                Some(newAnswers)
              )
            else
              fillingOutReturn.draftReturn.fold(
                _.copy(representeeAnswers = Some(newAnswers)),
                _.copy(representeeAnswers = Some(newAnswers)),
                _.copy(representeeAnswers = Some(newAnswers)),
                _.copy(representeeAnswers = Some(newAnswers))
              )

          fillingOutReturn.copy(draftReturn = newDraftReturn)
        }
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
             updateSession(sessionStore, request)(
               _.copy(journeyStatus = Some(newJourney.merge))
             )
           )
    } yield ()
  }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) { (representativeType, journey, answers) =>
        answers match {
          case IncompleteRepresenteeAnswers(None, _, _, _, _, _)                              =>
            Redirect(routes.RepresenteeController.enterName())

          case IncompleteRepresenteeAnswers(_, _, None, _, _, _) if representativeType.isLeft =>
            Redirect(routes.RepresenteeController.enterDateOfDeath())

          case IncompleteRepresenteeAnswers(_, None, _, _, _, _)                              =>
            Redirect(routes.RepresenteeController.enterId())

          case IncompleteRepresenteeAnswers(
                Some(_),
                Some(_),
                _,
                _,
                false,
                _
              ) =>
            Redirect(routes.RepresenteeController.confirmPerson())

          case IncompleteRepresenteeAnswers(
                _,
                _,
                _,
                contactDetails,
                _,
                hasConfirmedContactDetails
              ) if contactDetails.isEmpty || !hasConfirmedContactDetails =>
            Redirect(routes.RepresenteeController.checkContactDetails())

          case IncompleteRepresenteeAnswers(
                Some(name),
                Some(id),
                dateOfDeath,
                Some(contactDetails),
                true,
                true
              ) =>
            val completeAnswers = CompleteRepresenteeAnswers(
              name,
              id,
              dateOfDeath,
              contactDetails
            )

            updateDraftReturnAndSession(
              completeAnswers,
              journey,
              clearDraftReturn = false
            ).fold(
              { e =>
                logger.warn("Could not update draft return or session", e)
                errorHandler.errorResult()
              },
              _ =>
                Ok(
                  cyaPage(
                    completeAnswers,
                    representativeType,
                    journey.isRight,
                    triageRoutes.CommonTriageQuestionsController
                      .whoIsIndividualRepresenting()
                  )
                )
            )

          case completeAnswers: CompleteRepresenteeAnswers                                    =>
            Ok(
              cyaPage(
                completeAnswers,
                representativeType,
                journey.isRight,
                triageRoutes.CommonTriageQuestionsController
                  .whoIsIndividualRepresenting()
              )
            )

        }

      }
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withCapacitorOrPersonalRepresentativeAnswers(request) {
        case _ =>
          Redirect(
            triage.routes.CommonTriageQuestionsController.howManyProperties()
          )
      }
    }

  private def handleNameMatchServiceError(
    nameMatchError: NameMatchServiceError[_]
  )(implicit request: RequestWithSessionData[_]): Result =
    nameMatchError match {
      case NameMatchServiceError.BackendError(error)           =>
        logger
          .warn("Encountered error while trying to perform name match", error)
        errorHandler.errorResult()

      case NameMatchServiceError.NameMatchFailed(u)            =>
        logger.info(
          s"Name match failed: ${u.unsuccessfulAttempts} attempts made out of a maximum ${u.maximumAttempts}"
        )
        Redirect(routes.RepresenteeController.nameMatchError())

      case NameMatchServiceError.TooManyUnsuccessfulAttempts() =>
        Redirect(routes.RepresenteeController.tooManyNameMatchAttempts())
    }
}

object RepresenteeController {

  sealed trait NameMatchError extends Product with Serializable

  object NameMatchError {

    final case class ServiceError(
      error: NameMatchServiceError[IndividualRepresenteeNameMatchDetails]
    ) extends NameMatchError

    final case class ValidationError(
      formWithErrors: Form[RepresenteeReferenceId]
    ) extends NameMatchError

  }

  val nameForm: Form[IndividualName] =
    IndividualName.form("representeeFirstName", "representeeLastName")

  val dateOfDeathForm: Form[DateOfDeath] = {
    val key = "dateOfDeath"
    Form(
      mapping(
        "" -> of(
          TimeUtils.dateFormatter(
            Some(LocalDate.now()),
            None,
            s"$key-day",
            s"$key-month",
            s"$key-year",
            key
          )
        )
      )(DateOfDeath(_))(d => Some(d.value))
    )
  }

  val idForm: Form[RepresenteeReferenceId] = {
    val (outerId, ninoId, sautrId, cgtReferenceId) =
      (
        "representeeReferenceIdType",
        "representeeNino",
        "representeeSautr",
        "representeeCgtRef"
      )

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
      InnerOption(data =>
        CgtReference.mapping
          .withPrefix(cgtReferenceId)
          .bind(data)
          .map(RepresenteeCgtReference)
      )

    val formatter =
      ConditionalRadioUtils.formatter[RepresenteeReferenceId](outerId)(
        List(
          Left(cgtReferenceInnerOption),
          Left(ninoInnerOption),
          Left(sautrInnerOption),
          Right(NoReferenceId)
        )
      ) {
        case RepresenteeCgtReference(cgtReference) =>
          Map(outerId -> "0", cgtReferenceId -> cgtReference.value)
        case RepresenteeNino(nino)                 => Map(outerId -> "1", ninoId -> nino.value)
        case RepresenteeSautr(sautr)               =>
          Map(outerId -> "2", sautrId -> sautr.value)
        case NoReferenceId                         => Map(outerId -> "3")
      }

    Form(
      mapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

  val confirmPersonForm: Form[Boolean] =
    Form(
      mapping(
        "confirmed" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

}
