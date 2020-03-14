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
import cats.instances.boolean._
import cats.instances.future._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError, Forms}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.MultipleDisposalsTriageController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.StartingNewDraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error, FormUtils, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.AssetType._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AssetType, IndividualUserType, MultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsAnswers, IncompleteMultipleDisposalsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, FormUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{ReturnsService, TaxYearService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.html.returns.triage.{multipledisposals => triagePages}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MultipleDisposalsTriageController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  taxYearService: TaxYearService,
  uuidGenerator: UUIDGenerator,
  cc: MessagesControllerComponents,
  val config: Configuration,
  guidancePage: triagePages.guidance,
  howManyPropertiesPage: triagePages.how_many_properties,
  wereYouAUKResidentPage: triagePages.were_you_a_uk_resident,
  wereAllPropertiesResidentialPage: triagePages.were_all_properties_residential,
  countryOfResidencePage: triagePages.country_of_residence,
  taxYearExchangedPage: triagePages.tax_year_exchanged,
  assetTypeForNonUkResidentsPage: triagePages.asset_type_for_non_uk_residents
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
        Ok(howManyPropertiesPage(form, routes.MultipleDisposalsTriageController.guidance()))
    }
  }

  def howManyDisposalsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        numberOfPropertiesForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                howManyPropertiesPage(formWithErrors, routes.MultipleDisposalsTriageController.guidance())
              ), { numberOfProperties =>
              if (answers.fold(_.numberOfProperties, c => Some(c.numberOfProperties)).contains(numberOfProperties)) {
                Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
              } else {
                val newAnswersWithRedirectTo =
                  if (numberOfProperties > 1)
                    Left[MultipleDisposalsTriageAnswers, IncompleteSingleDisposalTriageAnswers](
                      answers.fold[MultipleDisposalsTriageAnswers](
                        _.copy(numberOfProperties = Some(numberOfProperties)),
                        _.copy(numberOfProperties = numberOfProperties)
                      )
                    ) -> routes.MultipleDisposalsTriageController.checkYourAnswers()
                  else
                    Right[MultipleDisposalsTriageAnswers, IncompleteSingleDisposalTriageAnswers](
                      IncompleteSingleDisposalTriageAnswers.empty.copy(
                        individualUserType         = answers.fold(_.individualUserType, _.individualUserType),
                        hasConfirmedSingleDisposal = true
                      )
                    ) -> routes.SingleDisposalsTriageController.checkYourAnswers()

                val newState = state.copy(newReturnTriageAnswers = newAnswersWithRedirectTo._1)

                updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newState))).map {
                  case Left(e) =>
                    logger.warn("Could not update session", e)
                    errorHandler.errorResult()

                  case Right(_) =>
                    Redirect(newAnswersWithRedirectTo._2)
                }

              }

            }
          )
    }
  }

  def wereYouAUKResident(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, _, answers) =>
        val wereYouUKResident = answers.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk()))
        val form              = wereYouUKResident.fold(wasAUkResidentForm)(wasAUkResidentForm.fill)
        Ok(wereYouAUKResidentPage(form, routes.MultipleDisposalsTriageController.howManyDisposals()))
    }
  }

  def wereYouAUKResidentSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        wasAUkResidentForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                wereYouAUKResidentPage(formWithErrors, routes.MultipleDisposalsTriageController.howManyDisposals())
              ), { wereUKResident =>
              if (answers.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk())).contains(wereUKResident)) {
                Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
              } else {
                val updatedAnswers = answers.fold[MultipleDisposalsTriageAnswers](
                  incomplete =>
                    incomplete.copy(
                      wasAUKResident               = Some(wereUKResident),
                      countryOfResidence           = None,
                      wereAllPropertiesResidential = None,
                      assetType                    = None
                    ),
                  complete =>
                    IncompleteMultipleDisposalsAnswers(
                      individualUserType           = complete.individualUserType,
                      numberOfProperties           = Some(complete.numberOfProperties),
                      wasAUKResident               = Some(wereUKResident),
                      countryOfResidence           = None,
                      wereAllPropertiesResidential = None,
                      assetType                    = None,
                      taxYearAfter6April2020       = None,
                      taxYear                      = None
                    )
                )

                val newState = state.copy(newReturnTriageAnswers = Left(updatedAnswers))

                updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newState))).map {
                  case Left(e) =>
                    logger.warn("Could not update session", e)
                    errorHandler.errorResult()

                  case Right(_) =>
                    Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                }
              }
            }
          )
    }
  }

  def wereAllPropertiesResidential(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withMultipleDisposalTriageAnswers(request) {
        case (_, _, answers) =>
          val werePropertiesResidential =
            answers.fold(_.wereAllPropertiesResidential, c => Some(c.assetType == AssetType.Residential))
          val form =
            werePropertiesResidential.fold(wereAllPropertiesResidentialForm)(wereAllPropertiesResidentialForm.fill)
          Ok(wereAllPropertiesResidentialPage(form, routes.MultipleDisposalsTriageController.wereYouAUKResident()))
      }
  }

  def wereAllPropertiesResidentialSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withMultipleDisposalTriageAnswers(request) {
        case (_, state, answers) =>
          wereAllPropertiesResidentialForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  wereAllPropertiesResidentialPage(
                    formWithErrors,
                    routes.MultipleDisposalsTriageController.wereYouAUKResident()
                  )
                ), { wereAllPropertiesResidential =>
                if (answers
                      .fold(_.wereAllPropertiesResidential, c => Some(isResidentialAssetType(c.assetType)))
                      .contains(wereAllPropertiesResidential)) {
                  Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                } else {
                  val updatedAnswers = answers.fold[MultipleDisposalsTriageAnswers](
                    incomplete =>
                      incomplete.copy(
                        wereAllPropertiesResidential = Some(wereAllPropertiesResidential),
                        assetType                    = Some(assetType(wereAllPropertiesResidential))
                      ),
                    complete =>
                      IncompleteMultipleDisposalsAnswers(
                        individualUserType           = complete.individualUserType,
                        numberOfProperties           = Some(complete.numberOfProperties),
                        wasAUKResident               = Some(true),
                        countryOfResidence           = Some(Country.uk),
                        wereAllPropertiesResidential = Some(wereAllPropertiesResidential),
                        assetType                    = Some(assetType(wereAllPropertiesResidential)),
                        taxYearAfter6April2020       = None,
                        taxYear                      = None
                      )
                  )

                  val newState = state.copy(newReturnTriageAnswers = Left(updatedAnswers))

                  updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newState))).map {
                    case Left(e) =>
                      logger.warn("Could not update session", e)
                      errorHandler.errorResult()

                    case Right(_) =>
                      Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                  }
                }
              }
            )
      }
  }

  def countryOfResidence(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, _, answers) =>
        val wasUkResident = answers.fold(_.wasAUKResident, c => Some(c.countryOfResidence.isUk()))

        if (!wasUkResident.contains(false)) {
          Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
        } else {
          val countryOfResidence =
            answers.fold(_.countryOfResidence, c => Some(c.countryOfResidence))
          val form =
            countryOfResidence.fold(countryOfResidenceForm)(countryOfResidenceForm.fill)
          Ok(countryOfResidencePage(form, routes.MultipleDisposalsTriageController.wereYouAUKResident()))
        }
    }
  }

  def countryOfResidenceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, answers) =>
        countryOfResidenceForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                countryOfResidencePage(
                  formWithErrors,
                  routes.MultipleDisposalsTriageController.wereYouAUKResident()
                )
              ), { countryOfResidence =>
              if (answers
                    .fold(_.countryOfResidence, c => Some(c.countryOfResidence))
                    .contains(countryOfResidence)) {
                Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
              } else {
                val updatedAnswers =
                  answers.fold[MultipleDisposalsTriageAnswers](
                    _.copy(countryOfResidence = Some(countryOfResidence)),
                    _.copy(countryOfResidence = countryOfResidence)
                  )

                val newState = state.copy(newReturnTriageAnswers = Left(updatedAnswers))

                updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newState))).map {
                  case Left(e) =>
                    logger.warn("Could not update session", e)
                    errorHandler.errorResult()

                  case Right(_) =>
                    Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                }
              }

            }
          )
    }
  }

  def whenWereContractsExchanged(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, _, answers) =>
        val taxYearExchanged = answers.fold(_.taxYearAfter6April2020, _ => Some(true))
        val form             = taxYearExchanged.fold(taxYearExchangedForm)(taxYearExchangedForm.fill)
        Ok(
          taxYearExchangedPage(
            form,
            taxYearBackLink(answers.fold(_.wasAUKResident.contains(true), _.countryOfResidence.isUk()))
          )
        )
    }
  }

  def whenWereContractsExchangedSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withMultipleDisposalTriageAnswers(request) {
        case (_, state, answers) =>
          taxYearExchangedForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  taxYearExchangedPage(
                    formWithErrors,
                    routes.MultipleDisposalsTriageController.wereAllPropertiesResidential()
                  )
                ), { taxYearAfter6April2020 =>
                if (answers.fold(_.taxYearAfter6April2020, _ => Some(true)).contains(taxYearAfter6April2020)) {
                  Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                } else {

                  val result =
                    for {
                      taxYear <- if (taxYearAfter6April2020) taxYearService.taxYear(LocalDateUtils.today())
                                else EitherT.pure[Future, Error](None)
                      answers <- EitherT.fromEither[Future](
                                  updateTaxYearToAnswers(taxYearAfter6April2020, taxYear, answers)
                                )
                      newState = state.copy(newReturnTriageAnswers = Left(answers))
                      _ <- EitherT(
                            updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newState)))
                          )
                    } yield ()

                  result.fold(
                    { e =>
                      logger.warn("Could not find tax year or update session", e)
                      errorHandler.errorResult()
                    },
                    _ => Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                  )

                }
              }
            )
      }
  }

  def assetTypeForNonUkResidents(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, _, answers) =>
        val assetType = answers.fold(_.assetType, c => Some(c.assetType))
        val form      = assetType.fold(assetTypeForNonUkResidentsForm)(assetTypeForNonUkResidentsForm.fill)
        Ok(assetTypeForNonUkResidentsPage(form, routes.MultipleDisposalsTriageController.countryOfResidence()))
    }
  }

  def assetTypeForNonUkResidentsSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withMultipleDisposalTriageAnswers(request) {
        case (_, state, answers) =>
          assetTypeForNonUkResidentsForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  assetTypeForNonUkResidentsPage(
                    formWithErrors,
                    routes.MultipleDisposalsTriageController.countryOfResidence()
                  )
                ), { assetType =>
                if (answers.fold(_.assetType, c => Some(c.assetType)).contains(assetType)) {
                  Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                } else {
                  val updatedAnswers =
                    answers.fold[MultipleDisposalsTriageAnswers](
                      _.copy(assetType = Some(assetType)),
                      _.copy(assetType = assetType)
                    )
                  val newState = state.copy(newReturnTriageAnswers = Left(updatedAnswers))

                  updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newState))).map {
                    case Left(e) =>
                      logger.warn("Could not update session", e)
                      errorHandler.errorResult()

                    case Right(_) =>
                      Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                  }
                }
              }
            )
      }
  }

  private def updateTaxYearToAnswers(
    taxYearAfter6April2020: Boolean,
    taxYear: Option[TaxYear],
    answers: MultipleDisposalsTriageAnswers
  ): Either[Error, MultipleDisposalsTriageAnswers] =
    taxYear match {
      case None if taxYearAfter6April2020 =>
        Left(Error("Could not find tax year"))
      case _ =>
        Right(
          answers.fold[MultipleDisposalsTriageAnswers](
            incomplete =>
              incomplete.copy(
                taxYearAfter6April2020 = Some(taxYearAfter6April2020),
                taxYear                = taxYear
              ),
            complete =>
              IncompleteMultipleDisposalsAnswers(
                individualUserType           = complete.individualUserType,
                numberOfProperties           = Some(complete.numberOfProperties),
                wasAUKResident               = Some(complete.countryOfResidence.isUk()),
                countryOfResidence           = Some(complete.countryOfResidence),
                wereAllPropertiesResidential = Some(isResidentialAssetType(complete.assetType)),
                assetType                    = Some(complete.assetType),
                taxYearAfter6April2020       = Some(taxYearAfter6April2020),
                taxYear                      = taxYear
              )
          )
        )
    }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, state, triageAnswers) =>
        val isIndividual = state.subscribedDetails.userType().isRight

        triageAnswers match {
          case IncompleteMultipleDisposalsAnswers(None, _, _, _, _, _, _, _) if isIndividual =>
            Redirect(routes.CommonTriageQuestionsController.whoIsIndividualRepresenting())

          case IncompleteMultipleDisposalsAnswers(Some(IndividualUserType.Capacitor), _, _, _, _, _, _, _) =>
            Redirect(routes.CommonTriageQuestionsController.capacitorsAndPersonalRepresentativesNotHandled())

          case IncompleteMultipleDisposalsAnswers(
              Some(IndividualUserType.PersonalRepresentative),
              _,
              _,
              _,
              _,
              _,
              _,
              _
              ) =>
            Redirect(routes.CommonTriageQuestionsController.capacitorsAndPersonalRepresentativesNotHandled())

          case IncompleteMultipleDisposalsAnswers(_, None, _, _, _, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.guidance())

          case IncompleteMultipleDisposalsAnswers(_, _, None, _, _, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.wereYouAUKResident())

          case IncompleteMultipleDisposalsAnswers(_, _, Some(false), None, _, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.countryOfResidence())

          case IncompleteMultipleDisposalsAnswers(_, _, Some(false), _, _, None, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.assetTypeForNonUkResidents())

          case IncompleteMultipleDisposalsAnswers(_, _, Some(false), _, _, Some(assetTypes), _, _)
              if (assetTypes =!= List(AssetType.Residential) && assetTypes =!= List(AssetType.NonResidential)) =>
            Redirect(routes.CommonTriageQuestionsController.assetTypeNotYetImplemented())

          case IncompleteMultipleDisposalsAnswers(_, _, Some(true), _, None, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.wereAllPropertiesResidential())

          case IncompleteMultipleDisposalsAnswers(_, _, Some(true), _, Some(false), _, _, _) =>
            Redirect(routes.CommonTriageQuestionsController.ukResidentCanOnlyDisposeResidential())

          case IncompleteMultipleDisposalsAnswers(_, _, _, _, _, _, None, _) =>
            Redirect(routes.MultipleDisposalsTriageController.whenWereContractsExchanged())

          case IncompleteMultipleDisposalsAnswers(_, _, _, _, _, _, Some(false), _) =>
            Redirect(routes.CommonTriageQuestionsController.disposalDateTooEarly())

          case IncompleteMultipleDisposalsAnswers(_, _, _, _, _, _, Some(true), Some(_)) =>
            Ok(s"All properties contracts were exchanged after 06th April, 2020")

          case IncompleteMultipleDisposalsAnswers(_, _, _, _, _, _, Some(true), None) =>
            logger.warn("No tax year was found when we expected one")
            errorHandler.errorResult()

          case c: CompleteMultipleDisposalsAnswers =>
            Ok(s"Got $c")
        }
    }
  }

  private def taxYearBackLink(wasAUKResident: Boolean): Call =
    if (wasAUKResident) routes.MultipleDisposalsTriageController.wereAllPropertiesResidential()
    else routes.MultipleDisposalsTriageController.assetTypeForNonUkResidents()

  private def isResidentialAssetType(assetType: List[AssetType]): Boolean =
    assetType match {
      case AssetType.Residential :: Nil => true
      case _                            => false
    }

  private def assetType(isResidential: Boolean): List[AssetType] =
    if (isResidential) List(AssetType.Residential) else List(AssetType.NonResidential)

  private def withMultipleDisposalTriageAnswers(request: RequestWithSessionData[_])(
    f: (SessionData, StartingNewDraftReturn, MultipleDisposalsTriageAnswers) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((session, s @ StartingNewDraftReturn(_, _, _, Left(t)))) =>
        f(session, s, t)

      case _ =>
        Redirect(uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes.StartController.start())
    }

}

object MultipleDisposalsTriageController {

  val numberOfPropertiesForm: Form[Int] = {
    val numberOfDisposalsKey = "multipleDisposalsNumberOfProperties"

    val numberOfPropertiesFormatter: Formatter[Int] = {
      def validateNumberOfProperties(i: Int): Either[FormError, Int] =
        if (i <= 0) Left(FormError(numberOfDisposalsKey, "error.tooSmall"))
        else if (i > 999) Left(FormError(numberOfDisposalsKey, "error.tooLong"))
        else Right(i)

      new Formatter[Int] {
        override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Int] = {
          val result =
            FormUtils.readValue(key, data, _.toInt).flatMap(validateNumberOfProperties)
          result.leftMap(Seq(_))
        }
        override def unbind(key: String, value: Int): Map[String, String] =
          Map(key -> value.toString)
      }
    }

    Form(
      mapping(
        numberOfDisposalsKey -> of(numberOfPropertiesFormatter)
      )(identity)(Some(_))
    )
  }

  val wasAUkResidentForm: Form[Boolean] = Form(
    mapping(
      "multipleDisposalsWereYouAUKResident" -> of(BooleanFormatter.formatter)
    )(identity)(Some(_))
  )

  val wereAllPropertiesResidentialForm: Form[Boolean] = Form(
    mapping(
      "multipleDisposalsWereAllPropertiesResidential" -> of(BooleanFormatter.formatter)
    )(identity)(Some(_))
  )

  val countryOfResidenceForm: Form[Country] = Form(
    mapping(
      "countryCode" -> of(Country.formatter)
    )(identity)(Some(_))
  )

  val taxYearExchangedForm: Form[Boolean] =
    Form(
      mapping(
        "multipleDisposalsTaxYear" -> of(
          FormUtils.radioFormFormatter("multipleDisposalsTaxYear", List(true, false))
        )
      )(identity)(Some(_))
    )

  val assetTypeForNonUkResidentsForm: Form[List[AssetType]] = {
    Form(
      mapping(
        "multipleDisposalsAssetTypeForNonUkResidents" -> Forms
          .list(of(FormUtils.checkBoxAssetTypeFormFormatter))
          .verifying("error.required", _.nonEmpty)
      )(identity)(Some(_))
    )
  }

}
