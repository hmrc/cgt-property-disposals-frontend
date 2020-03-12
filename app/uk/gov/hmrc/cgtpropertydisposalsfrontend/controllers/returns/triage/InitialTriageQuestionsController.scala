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

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.IncompleteMultipleDisposalsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.NumberOfProperties.{MoreThanOne, One}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{IndividualUserType, MultipleDisposalsTriageAnswers, NumberOfProperties, SingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{FormUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{triage => triagePages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class InitialTriageQuestionsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  val config: Configuration,
  whoAreYouReportingForPage: triagePages.who_are_you_reporting_for,
  howManyPropertiesPage: triagePages.how_many_properties,
  capacitorsAndPersonalRepresentativesNotHandledPage: triagePages.capcitors_personal_representatives_not_handled
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  import InitialTriageQuestionsController._

  private def isIndividual(state: Either[StartingNewDraftReturn, FillingOutReturn]): Boolean =
    state.fold(_.subscribedDetails.userType(), _.subscribedDetails.userType()).isRight

  def whoIsIndividualRepresenting(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withState(request) {
      case (_, state) =>
        if (!isIndividual(state))
          Redirect(routes.InitialTriageQuestionsController.howManyProperties())
        else {
          val form = getIndividualUserType(state).fold(whoAreYouReportingForForm)(whoAreYouReportingForForm.fill)

          Ok(
            whoAreYouReportingForPage(
              form,
              None,
              state.isRight
            )
          )
        }
    }
  }

  def whoIsIndividualRepresentingSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withState(request) {
        case (_, state) =>
          if (!isIndividual(state))
            Redirect(routes.InitialTriageQuestionsController.howManyProperties())
          else {
            whoAreYouReportingForForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(whoAreYouReportingForPage(formWithErrors, None, state.isRight)), { individualUserType =>
                  val answers = triageAnswersFomState(state)
                  val oldIndividualUserType = answers.fold(
                    _.fold(_.individualUserType, c => c.individualUserType),
                    _.fold(_.individualUserType, c => c.individualUserType)
                  )
                  val redirectTo = answers.fold(
                    _ => routes.MultipleDisposalsTriageController.checkYourAnswers(),
                    _ => routes.SingleDisposalsTriageController.checkYourAnswers()
                  )

                  if (oldIndividualUserType.contains(individualUserType)) {
                    Redirect(redirectTo)
                  } else {

                    val updatedState = updateIndividualUserType(state, individualUserType)
                    val result =
                      for {
                        _ <- updatedState.fold(
                              _ => EitherT.pure(()),
                              fillingOutReturn =>
                                returnsService
                                  .storeDraftReturn(fillingOutReturn.draftReturn, fillingOutReturn.agentReferenceNumber)
                            )
                        _ <- EitherT(
                              updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedState.merge)))
                            )
                      } yield ()

                    result.fold(
                      { e =>
                        logger.warn("Could not perform updates", e)
                        errorHandler.errorResult()
                      },
                      _ => Redirect(redirectTo)
                    )
                  }
                }
              )
          }
      }
  }

  def howManyProperties(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withState(request) {
      case (_, state) =>
        val form =
          getNumberOfProperties(state).fold(numberOfPropertiesForm)(numberOfPropertiesForm.fill)
        Ok(
          howManyPropertiesPage(
            form,
            howManyPropertiesBackLink(state),
            state.isRight
          )
        )
    }
  }

  def howManyPropertiesSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    def redirectTo(state: Either[StartingNewDraftReturn, FillingOutReturn]): Call =
      triageAnswersFomState(state).fold(
        _ => routes.MultipleDisposalsTriageController.checkYourAnswers(),
        _ => routes.SingleDisposalsTriageController.checkYourAnswers()
      )

    withState(request) {
      case (_, state) =>
        numberOfPropertiesForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                howManyPropertiesPage(
                  formWithErrors,
                  howManyPropertiesBackLink(state),
                  state.isRight
                )
              ), { numberOfProperties =>
              if (getNumberOfProperties(state).contains(numberOfProperties)) {
                Redirect(redirectTo(state))
              } else {
                val updatedState = updateNumberOfProperties(state, numberOfProperties)

                val result =
                  for {
                    _ <- updatedState.fold(
                          _ => EitherT.pure(()),
                          fillingOutReturn =>
                            returnsService
                              .storeDraftReturn(fillingOutReturn.draftReturn, fillingOutReturn.agentReferenceNumber)
                        )
                    _ <- EitherT(updateSession(sessionStore, request)(_.copy(journeyStatus = Some(updatedState.merge))))
                  } yield ()

                result.fold(
                  { e =>
                    logger.warn("Could not perform updates", e)
                    errorHandler.errorResult()
                  },
                  _ => Redirect(redirectTo(updatedState))
                )
              }

            }
          )
    }
  }

  def capacitorsAndPersonalRepresentativesNotHandled(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withState(request) {
        case (_, state) =>
          val individualUserType = getIndividualUserType(state)
          if (individualUserType.contains(IndividualUserType.Capacitor) || individualUserType
                .contains(IndividualUserType.PersonalRepresentative))
            Ok(capacitorsAndPersonalRepresentativesNotHandledPage())
          else
            Redirect(
              triageAnswersFomState(state).fold(
                _ => routes.MultipleDisposalsTriageController.checkYourAnswers(),
                _ => routes.SingleDisposalsTriageController.checkYourAnswers()
              )
            )
      }
  }

  private def howManyPropertiesBackLink(state: Either[StartingNewDraftReturn, FillingOutReturn]): Option[Call] =
    if (!isIndividual(state))
      None
    else
      Some(
        triageAnswersFomState(state).fold(
          _.fold(
            _ => routes.InitialTriageQuestionsController.whoIsIndividualRepresenting(),
            _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
          ),
          _.fold(
            _ => routes.InitialTriageQuestionsController.whoIsIndividualRepresenting(),
            _ => routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        )
      )

  private def updateNumberOfProperties(
    state: Either[StartingNewDraftReturn, FillingOutReturn],
    numberOfProperties: NumberOfProperties
  ): Either[StartingNewDraftReturn, FillingOutReturn] = {
    val individualUserType = getIndividualUserType(state)
    numberOfProperties match {
      case NumberOfProperties.One =>
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

      case NumberOfProperties.MoreThanOne =>
        val newTriageAnswers =
          IncompleteMultipleDisposalsAnswers.empty.copy(
            individualUserType = individualUserType
          )

        state.bimap(
          _.copy(newReturnTriageAnswers = Left(newTriageAnswers)),
          fillingOutReturn =>
            fillingOutReturn.copy(
              draftReturn = fillingOutReturn.draftReturn.copy(
                triageAnswers = sys.error("not handled yet")
              )
            )
        )
    }
  }

  private def updateIndividualUserType(
    state: Either[StartingNewDraftReturn, FillingOutReturn],
    individualUserType: IndividualUserType
  ): Either[StartingNewDraftReturn, FillingOutReturn] = {
    val answers = triageAnswersFomState(state)
    state.bimap(
      _.copy(
        newReturnTriageAnswers = answers.bimap[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers](
          _.fold[MultipleDisposalsTriageAnswers](
            _.copy(individualUserType = Some(individualUserType)),
            _.copy(individualUserType = Some(individualUserType))
          ),
          _.fold(
            _.copy(individualUserType = Some(individualUserType)),
            _.copy(individualUserType = Some(individualUserType))
          )
        )
      ),
      r =>
        r.copy(
          draftReturn = r.draftReturn.copy(
            triageAnswers = r.draftReturn.triageAnswers.fold(
              _.fold(
                _.copy(individualUserType = Some(individualUserType)),
                _.copy(individualUserType = Some(individualUserType))
              ),
              _.fold(
                _.copy(individualUserType = Some(individualUserType)),
                _.copy(individualUserType = Some(individualUserType))
              )
            )
          )
        )
    )
  }

  private def getIndividualUserType(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Option[IndividualUserType] =
    triageAnswersFomState(state).fold(
      _.fold(_.individualUserType, c => c.individualUserType),
      _.fold(_.individualUserType, c => c.individualUserType)
    )

  private def getNumberOfProperties(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Option[NumberOfProperties] =
    state.fold(
      _.newReturnTriageAnswers.fold(
        _ => Some(NumberOfProperties.MoreThanOne),
        _.fold(
          incomplete =>
            if (incomplete.hasConfirmedSingleDisposal) Some(NumberOfProperties.One)
            else None,
          _ => Some(NumberOfProperties.One)
        )
      ),
      _ => Some(NumberOfProperties.One)
    )

  private def triageAnswersFomState(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers] =
    state.bimap(_.newReturnTriageAnswers, r => Right(r.draftReturn.triageAnswers)).merge

  private def withState(request: RequestWithSessionData[_])(
    f: (SessionData, Either[StartingNewDraftReturn, FillingOutReturn]) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((session, s: StartingNewDraftReturn)) =>
        f(session, Left(s))

      case Some((session, r: FillingOutReturn)) =>
        f(session, Right(r))

      case _ =>
        Redirect(uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController.start())
    }

}
object InitialTriageQuestionsController {

  val whoAreYouReportingForForm: Form[IndividualUserType] = Form(
    mapping(
      "individualUserType" -> of(
        FormUtils.radioFormFormatter("individualUserType", List(Self, Capacitor, PersonalRepresentative))
      )
    )(identity)(Some(_))
  )

  val numberOfPropertiesForm: Form[NumberOfProperties] = Form(
    mapping(
      "numberOfProperties" -> of(FormUtils.radioFormFormatter("numberOfProperties", List(One, MoreThanOne)))
    )(identity)(Some(_))
  )
}
