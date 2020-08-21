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

import java.time.LocalDate

import cats.data.EitherT
import cats.instances.future._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.order._
import com.google.inject.Inject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{StartingToAmendToFillingOutReturnBehaviour, representee}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingNewDraftReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin, Self}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.IncompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.NumberOfProperties.{MoreThanOne, One}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, FormUtils, SessionData, TimeUtils, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.{triage => triagePages}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.representee.{routes => representeeRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.homepage.{routes => homePageRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.Organisation
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator

import scala.concurrent.{ExecutionContext, Future}

class CommonTriageQuestionsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  uuidGenerator: UUIDGenerator,
  cc: MessagesControllerComponents,
  val config: Configuration,
  whoAreYouReportingForPage: triagePages.who_are_you_reporting_for,
  howManyPropertiesPage: triagePages.how_many_properties,
  ukResidentCanOnlyDisposeResidentialPage: triagePages.uk_resident_can_only_dispose_residential,
  disposalDateTooEarlyUkResidents: triagePages.disposal_date_too_early_uk_residents,
  disposalDateTooEarlyNonUkResidents: triagePages.disposal_date_too_early_non_uk_residents,
  previousReturnExistsWithSameCompletionDatePage: triagePages.previous_return_exists_with_same_completion_date,
  furtherReturnsHelpPage: triagePages.further_retuns_help,
  disposalDateInDifferentTaxYearPage: triagePages.disposaldate_in_different_taxyear
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with StartingToAmendToFillingOutReturnBehaviour {

  import CommonTriageQuestionsController._

  private def isIndividual(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Boolean =
    state
      .fold(_.subscribedDetails.userType(), _.subscribedDetails.userType())
      .isRight

  def whoIsIndividualRepresenting(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState { (_, state) =>
        if (!isIndividual(state))
          Redirect(routes.CommonTriageQuestionsController.howManyProperties())
        else {
          val form = {
            val f = whoAreYouReportingForForm(
              request.userType.contains(UserType.Agent)
            )
            getIndividualUserType(state).fold(f)(f.fill)
          }

          Ok(
            whoAreYouReportingForPage(
              form,
              None,
              state.isRight,
              state.fold(_ => false, _.isAmendReturn)
            )
          )
        }
      }
    }

  def whoIsIndividualRepresentingSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState { (_, state) =>
        if (!isIndividual(state))
          if (state.fold(_.isFurtherReturn, _.isFurtherOrAmendReturn).contains(true))
            Redirect(routes.CommonTriageQuestionsController.furtherReturnHelp())
          else
            Redirect(routes.CommonTriageQuestionsController.howManyProperties())
        else
          whoAreYouReportingForForm(request.userType.contains(UserType.Agent))
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  whoAreYouReportingForPage(
                    formWithErrors,
                    None,
                    state.isRight,
                    state.fold(_ => false, _.isAmendReturn)
                  )
                ),
              { individualUserType =>
                val answers = triageAnswersFomState(state)

                val oldIndividualUserType = answers.fold(
                  _.fold(_.individualUserType, c => c.individualUserType),
                  _.fold(_.individualUserType, c => c.individualUserType)
                )

                val updatedState =
                  updateIndividualUserType(state, individualUserType)

                val redirectTo =
                  if (
                    updatedState
                      .fold(_.isFurtherReturn, _.isFurtherOrAmendReturn)
                      .contains(true)
                  )
                    answers.fold(
                      _.fold(
                        _ => routes.CommonTriageQuestionsController.furtherReturnHelp(),
                        _ => redirectToCheckYourAnswers(state)
                      ),
                      _.fold(
                        _ => routes.CommonTriageQuestionsController.furtherReturnHelp(),
                        _ => redirectToCheckYourAnswers(state)
                      )
                    )
                  else redirectToCheckYourAnswers(state)

                if (oldIndividualUserType.contains(individualUserType))
                  Redirect(redirectTo)
                else {

                  val result =
                    for {
                      _ <- updatedState.fold(
                             _ => EitherT.pure[Future, Error](()),
                             returnsService.storeDraftReturn(_)
                           )
                      _ <- EitherT(
                             updateSession(sessionStore, request)(
                               _.copy(journeyStatus = Some(updatedState.merge))
                             )
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

  def furtherReturnHelp(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState { (_, state) =>
        val individualUserType = getIndividualUserType(state)
        val backLink           = individualUserType match {
          case _ if request.userType.contains(Organisation) => homePageRoutes.HomePageController.homepage()
          case Some(_: RepresentativeType)                  => representeeRoutes.RepresenteeController.isFirstReturn()
          case _                                            => routes.CommonTriageQuestionsController.whoIsIndividualRepresenting()
        }
        Ok(
          furtherReturnsHelpPage(
            backLink,
            state.fold(_.subscribedDetails.isATrust, _.subscribedDetails.isATrust),
            individualUserType,
            state.isRight
          )
        )
      }
    }

  def furtherReturnHelpSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState { (_, state) =>
        getIndividualUserType(state) match {
          case Some(_: RepresentativeType) => Redirect(representeeRoutes.RepresenteeController.enterName())
          case _                           => Redirect(routes.CommonTriageQuestionsController.howManyProperties())
        }
      }
    }

  def howManyProperties(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState { (_, state) =>
        val form =
          getNumberOfProperties(state).fold(numberOfPropertiesForm)(
            numberOfPropertiesForm.fill
          )
        Ok(
          howManyPropertiesPage(
            form,
            howManyPropertiesBackLink(state),
            state.isRight,
            state
              .fold(_.subscribedDetails.isATrust, _.subscribedDetails.isATrust),
            getRepresentativeType(state),
            state.fold(_ => false, _.isAmendReturn)
          )
        )
      }
    }

  def getRepresentativeType(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Option[RepresentativeType] =
    state
      .fold(
        _.newReturnTriageAnswers.fold(
          _.representativeType(),
          _.representativeType()
        ),
        _.draftReturn.representativeType()
      )

  def howManyPropertiesSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState { (_, state) =>
        numberOfPropertiesForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                howManyPropertiesPage(
                  formWithErrors,
                  howManyPropertiesBackLink(state),
                  state.isRight,
                  state.fold(
                    _.subscribedDetails.isATrust,
                    _.subscribedDetails.isATrust
                  ),
                  getRepresentativeType(state),
                  state.fold(_ => false, _.isAmendReturn)
                )
              ),
            numberOfProperties =>
              if (getNumberOfProperties(state).contains(numberOfProperties))
                Redirect(redirectToCheckYourAnswers(state))
              else {
                val updatedState =
                  updateNumberOfProperties(state, numberOfProperties)

                val result =
                  for {
                    _ <- updatedState.fold(
                           _ => EitherT.pure[Future, Error](()),
                           returnsService.storeDraftReturn(_)
                         )
                    _ <- EitherT(
                           updateSession(sessionStore, request)(
                             _.copy(journeyStatus = Some(updatedState.merge))
                           )
                         )
                  } yield ()

                result.fold(
                  { e =>
                    logger.warn("Could not perform updates", e)
                    errorHandler.errorResult()
                  },
                  _ => Redirect(redirectToCheckYourAnswers(updatedState))
                )
              }
          )
      }
    }

  def ukResidentCanOnlyDisposeResidential(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState { (_, state) =>
        val triageAnswers                      = triageAnswersFomState(state)
        val isAssetTypeNonResidential: Boolean = triageAnswers.fold(
          _.fold(
            _.wereAllPropertiesResidential.contains(false),
            _.assetTypes === List(AssetType.NonResidential)
          ),
          _.fold(
            _.assetType.contains(AssetType.NonResidential),
            _.assetType === AssetType.NonResidential
          )
        )
        val wasUkResident                      = triageAnswers.fold(
          _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())),
          _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk()))
        )
        lazy val backLink                      = triageAnswers.fold(
          _ =>
            routes.MultipleDisposalsTriageController
              .wereAllPropertiesResidential(),
          _ =>
            routes.SingleDisposalsTriageController
              .didYouDisposeOfAResidentialProperty()
        )

        wasUkResident match {
          case Some(true) if isAssetTypeNonResidential =>
            Ok(
              ukResidentCanOnlyDisposeResidentialPage(
                backLink,
                state.fold(_.subscribedDetails, _.subscribedDetails).isATrust,
                triageAnswers.fold(_.representativeType(), _.representativeType())
              )
            )
          case _                                       => Redirect(redirectToCheckYourAnswers(state))
        }
      }
    }

  def disposalDateTooEarly(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState { (_, state) =>
        val triageAnswers = triageAnswersFomState(state)
        lazy val backLink = triageAnswers.fold(
          _ =>
            routes.MultipleDisposalsTriageController
              .whenWereContractsExchanged(),
          _ => routes.SingleDisposalsTriageController.whenWasDisposalDate()
        )

        triageAnswers.fold(
          _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())),
          _.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk()))
        ) match {
          case None        => Redirect(redirectToCheckYourAnswers(state))
          case Some(wasUk) =>
            if (wasUk) Ok(disposalDateTooEarlyUkResidents(backLink))
            else Ok(disposalDateTooEarlyNonUkResidents(backLink))
        }
      }
    }

  def disposalsOfSharesTooEarly(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState { (_, state) =>
        val triageAnswers = triageAnswersFomState(state)
        lazy val backLink = triageAnswers.fold(
          _ => routes.MultipleDisposalsTriageController.disposalDateOfShares(),
          _ => routes.SingleDisposalsTriageController.disposalDateOfShares()
        )
        Ok(disposalDateTooEarlyNonUkResidents(backLink))
      }
    }

  def previousReturnExistsWithSameCompletionDate(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState { (_, state) =>
        val backLink =
          if (triageAnswersFomState(state).isLeft) routes.MultipleDisposalsTriageController.completionDate()
          else routes.SingleDisposalsTriageController.whenWasCompletionDate()

        Ok(
          previousReturnExistsWithSameCompletionDatePage(
            state.fold(_.subscribedDetails, _.subscribedDetails).isATrust,
            backLink
          )
        )
      }
    }

  private def amendReturnDisposalDateBackLink(state: Either[StartingNewDraftReturn, FillingOutReturn]): Call =
    state.fold(
      _ => controllers.routes.StartController.start(),
      _.draftReturn.fold(
        _ => routes.MultipleDisposalsTriageController.whenWereContractsExchanged(),
        _ => routes.SingleDisposalsTriageController.whenWasDisposalDate(),
        _ => routes.SingleDisposalsTriageController.disposalDateOfShares(),
        _ => routes.MultipleDisposalsTriageController.disposalDateOfShares(),
        _ => controllers.routes.StartController.start()
      )
    )

  def amendReturnDisposalDateDifferentTaxYear(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withState { (_, state) =>
        Ok(disposalDateInDifferentTaxYearPage(amendReturnDisposalDateBackLink(state)))
      }
    }

  private def redirectToCheckYourAnswers(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Call =
    triageAnswersFomState(state).fold(
      _ => routes.MultipleDisposalsTriageController.checkYourAnswers(),
      _ => routes.SingleDisposalsTriageController.checkYourAnswers()
    )

  private def isIndividualASelfUserType(
    triageAnswers: Either[
      MultipleDisposalsTriageAnswers,
      SingleDisposalTriageAnswers
    ]
  ): Boolean =
    triageAnswers
      .fold(
        m =>
          m.fold(
            _.individualUserType,
            c => c.individualUserType
          ),
        s =>
          s.fold(
            _.individualUserType,
            c => c.individualUserType
          )
      )
      .contains(IndividualUserType.Self)

  private def howManyPropertiesBackLink(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Option[Call] = {

    val isFurtherReturn = state.fold(
      _.isFurtherReturn.contains(true),
      _.isFurtherReturn.contains(true)
    )

    val triageAnswers  = triageAnswersFomState(state)
    val isSelfUserType = isIndividualASelfUserType(triageAnswers)

    if (isFurtherReturn)
      Some(
        triageAnswers.fold(
          _.fold(
            _ => routes.CommonTriageQuestionsController.furtherReturnHelp(),
            _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
          ),
          _.fold(
            _ => routes.CommonTriageQuestionsController.furtherReturnHelp(),
            _ => routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        )
      )
    else if (!isIndividual(state))
      None
    else
      Some(
        triageAnswers.fold(
          _.fold(
            _ =>
              if (!isSelfUserType) representee.routes.RepresenteeController.checkYourAnswers()
              else routes.CommonTriageQuestionsController.whoIsIndividualRepresenting(),
            _ => routes.MultipleDisposalsTriageController.checkYourAnswers()
          ),
          _.fold(
            _ =>
              if (isSelfUserType)
                routes.CommonTriageQuestionsController
                  .whoIsIndividualRepresenting()
              else representee.routes.RepresenteeController.checkYourAnswers(),
            _ => routes.SingleDisposalsTriageController.checkYourAnswers()
          )
        )
      )
  }

  private def updateNumberOfProperties(
    state: Either[StartingNewDraftReturn, FillingOutReturn],
    numberOfProperties: NumberOfProperties
  ): Either[StartingNewDraftReturn, FillingOutReturn] = {
    val individualUserType = getIndividualUserType(state)
    numberOfProperties match {
      case NumberOfProperties.One         =>
        val newTriageAnswers =
          IncompleteSingleDisposalTriageAnswers.empty.copy(
            individualUserType = individualUserType,
            hasConfirmedSingleDisposal = true
          )

        state.bimap(
          _.copy(newReturnTriageAnswers = Right(newTriageAnswers)),
          fillingOutReturn =>
            fillingOutReturn.copy(
              draftReturn = DraftSingleDisposalReturn.newDraftReturn(
                fillingOutReturn.draftReturn.id,
                newTriageAnswers,
                fillingOutReturn.draftReturn.representeeAnswers
              )
            )
        )

      case NumberOfProperties.MoreThanOne =>
        val newTriageAnswers =
          IncompleteMultipleDisposalsTriageAnswers.empty.copy(
            individualUserType = individualUserType
          )

        state.bimap(
          _.copy(newReturnTriageAnswers = Left(newTriageAnswers)),
          fillingOutReturn =>
            fillingOutReturn.copy(
              draftReturn = DraftMultipleDisposalsReturn.newDraftReturn(
                fillingOutReturn.draftReturn.id,
                newTriageAnswers,
                fillingOutReturn.draftReturn.representeeAnswers
              )
            )
        )
    }
  }

  private def updateIndividualUserType(
    state: Either[StartingNewDraftReturn, FillingOutReturn],
    individualUserType: IndividualUserType
  ): Either[StartingNewDraftReturn, FillingOutReturn] = {
    def updateSingleDisposalAnswers(
      i: SingleDisposalTriageAnswers
    ): IncompleteSingleDisposalTriageAnswers =
      i.unset(_.wasAUKResident)
        .unset(_.countryOfResidence)
        .unset(_.assetType)
        .unset(_.disposalDate)
        .unset(_.tooEarlyDisposalDate)
        .unset(_.completionDate)
        .copy(individualUserType = Some(individualUserType))

    def updateMultipleDisposalAnswers(
      i: MultipleDisposalsTriageAnswers
    ): IncompleteMultipleDisposalsTriageAnswers =
      i.unset(_.wasAUKResident)
        .unset(_.countryOfResidence)
        .unset(_.assetTypes)
        .unset(_.wereAllPropertiesResidential)
        .unset(_.taxYear)
        .unset(_.taxYearAfter6April2020)
        .unset(_.completionDate)
        .copy(individualUserType = Some(individualUserType))

    val answers       = triageAnswersFomState(state)
    val furtherReturn = isFurtherReturn(state)

    state.bimap(
      _.copy(
        newReturnTriageAnswers = answers
          .bimap[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers](
            updateMultipleDisposalAnswers,
            updateSingleDisposalAnswers
          ),
        representeeAnswers = None
      ),
      r =>
        r.copy(
          draftReturn = r.draftReturn.fold[DraftReturn](
            multiple =>
              multiple.copy(
                triageAnswers = updateMultipleDisposalAnswers(multiple.triageAnswers),
                representeeAnswers = None,
                examplePropertyDetailsAnswers = None,
                yearToDateLiabilityAnswers = None,
                supportingEvidenceAnswers = None,
                exemptionAndLossesAnswers = None,
                gainOrLossAfterReliefs = None
              ),
            single =>
              single.copy(
                triageAnswers = updateSingleDisposalAnswers(single.triageAnswers),
                representeeAnswers = None,
                propertyAddress = None,
                disposalDetailsAnswers = None,
                acquisitionDetailsAnswers = None,
                reliefDetailsAnswers = single.reliefDetailsAnswers
                  .map(_.unsetPrrAndLettingRelief(answers.fold(_.isPeriodOfAdmin(), _.isPeriodOfAdmin()))),
                yearToDateLiabilityAnswers = None,
                initialGainOrLoss = None,
                supportingEvidenceAnswers = None,
                exemptionAndLossesAnswers =
                  if (furtherReturn)
                    single.exemptionAndLossesAnswers.map(_.unset(_.inYearLosses).unset(_.previousYearsLosses))
                  else None,
                gainOrLossAfterReliefs = None
              ),
            singleIndirect =>
              singleIndirect.copy(
                triageAnswers = updateSingleDisposalAnswers(singleIndirect.triageAnswers),
                representeeAnswers = None,
                companyAddress = None,
                disposalDetailsAnswers = None,
                acquisitionDetailsAnswers = None,
                yearToDateLiabilityAnswers = None,
                supportingEvidenceAnswers = None,
                exemptionAndLossesAnswers =
                  if (furtherReturn)
                    singleIndirect.exemptionAndLossesAnswers.map(_.unset(_.inYearLosses).unset(_.previousYearsLosses))
                  else None,
                gainOrLossAfterReliefs = None
              ),
            multipleIndirect =>
              multipleIndirect.copy(
                triageAnswers = updateMultipleDisposalAnswers(multipleIndirect.triageAnswers),
                representeeAnswers = None,
                exampleCompanyDetailsAnswers = None,
                yearToDateLiabilityAnswers = None,
                supportingEvidenceAnswers = None,
                exemptionAndLossesAnswers =
                  if (furtherReturn)
                    multipleIndirect.exemptionAndLossesAnswers.map(_.unset(_.inYearLosses).unset(_.previousYearsLosses))
                  else None,
                gainOrLossAfterReliefs = None
              ),
            singleMixedUse =>
              singleMixedUse.copy(
                triageAnswers = updateSingleDisposalAnswers(singleMixedUse.triageAnswers),
                representeeAnswers = None,
                mixedUsePropertyDetailsAnswers = None,
                yearToDateLiabilityAnswers = None,
                supportingEvidenceAnswers = None,
                exemptionAndLossesAnswers =
                  if (furtherReturn)
                    singleMixedUse.exemptionAndLossesAnswers.map(_.unset(_.inYearLosses).unset(_.previousYearsLosses))
                  else None,
                gainOrLossAfterReliefs = None
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

  private def isFurtherReturn(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Boolean =
    state.fold(_.isFurtherReturn, _.isFurtherReturn).contains(true)

  private def getNumberOfProperties(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Option[NumberOfProperties] = {
    def numberOfProperties(
      singleDisposalTriageAnswers: SingleDisposalTriageAnswers
    ) =
      singleDisposalTriageAnswers.fold(
        incomplete =>
          if (incomplete.hasConfirmedSingleDisposal)
            Some(NumberOfProperties.One)
          else None,
        _ => Some(NumberOfProperties.One)
      )

    state.fold(
      _.newReturnTriageAnswers.fold(
        _ => Some(NumberOfProperties.MoreThanOne),
        numberOfProperties
      ),
      _.draftReturn.fold(
        _ => Some(NumberOfProperties.MoreThanOne),
        s => numberOfProperties(s.triageAnswers),
        s => numberOfProperties(s.triageAnswers),
        _ => Some(NumberOfProperties.MoreThanOne),
        s => numberOfProperties(s.triageAnswers)
      )
    )
  }

  private def triageAnswersFomState(
    state: Either[StartingNewDraftReturn, FillingOutReturn]
  ): Either[MultipleDisposalsTriageAnswers, SingleDisposalTriageAnswers] =
    state
      .bimap(
        _.newReturnTriageAnswers,
        _.draftReturn.triageAnswers()
      )
      .merge

  private def withState(
    f: (
      SessionData,
      Either[StartingNewDraftReturn, FillingOutReturn]
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((_, s: StartingToAmendReturn))        =>
        convertFromStartingAmendToFillingOutReturn(s, sessionStore, errorHandler, uuidGenerator)

      case Some((session, s: StartingNewDraftReturn)) =>
        f(session, Left(s))

      case Some((session, r: FillingOutReturn))       =>
        f(session, Right(r))

      case _                                          =>
        Redirect(
          uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController
            .start()
        )
    }

}
object CommonTriageQuestionsController {

  def whoAreYouReportingForForm(isAgent: Boolean): Form[IndividualUserType] = {
    val options =
      if (isAgent) List(Self, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin)
      else List(Self, Capacitor, PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin)

    Form(
      mapping(
        "individualUserType" -> of(
          FormUtils.radioFormFormatter(
            options
          )
        )
      )(identity)(Some(_))
    )
  }

  val numberOfPropertiesForm: Form[NumberOfProperties] = Form(
    mapping(
      "numberOfProperties" -> of(
        FormUtils
          .radioFormFormatter(List(One, MoreThanOne))
      )
    )(identity)(Some(_))
  )

  def sharesDisposalDateForm(
    personalRepresentativeDetails: Option[PersonalRepresentativeDetails]
  ): Form[ShareDisposalDate] = {
    val key = "sharesDisposalDate"
    Form(
      mapping(
        "" -> of(
          TimeUtils.dateFormatter(
            Some(LocalDate.now()),
            None,
            s"$key-day",
            s"$key-month",
            s"$key-year",
            key,
            List(TimeUtils.personalRepresentativeDateValidation(personalRepresentativeDetails, key))
          )
        )
      )(ShareDisposalDate(_))(d => Some(d.value))
    )
  }

}
