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
import cats.syntax.either._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.{Form, FormError}
import play.api.data.Forms.{mapping, of}
import play.api.data.format.Formatter
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.triage.MultipleDisposalsTriageController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.StartingNewDraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Country
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error, FormUtils, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{AssetType, MultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsAnswers, IncompleteMultipleDisposalsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.IncompleteSingleDisposalTriageAnswers
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
  taxYearExchangedPage: triagePages.tax_year_exchanged
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
                        individualUserType         = answers.fold(_.individualUserType, c => Some(c.individualUserType)),
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
                      individualUserType           = Some(complete.individualUserType),
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
                      .fold(_.wereAllPropertiesResidential, c => Some(c.assetType.isResidential()))
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
                        individualUserType           = Some(complete.individualUserType),
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

  def whenWereContractsExchanged(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, _, answers) =>
        val taxYearExchanged = answers.fold(_.taxYearAfter6April2020, c => Some(true))
        val form             = taxYearExchanged.fold(taxYearExchangedForm)(taxYearExchangedForm.fill)
        Ok(taxYearExchangedPage(form, routes.MultipleDisposalsTriageController.wereAllPropertiesResidential()))
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
                ), { taxYearExchanged =>
                if (answers.fold(_.taxYearAfter6April2020, _ => Some(true)).contains(taxYearExchanged)) {
                  Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers())
                } else {

                  val result = for {
                    taxYear <- taxYearService.taxYear(LocalDateUtils.today())
                    multipleDisposalsTriageAnswers <- EitherT.fromEither[Future](
                                                       updateTaxYearToAnswers(taxYearExchanged, taxYear, answers)
                                                     )
                    newState = state.copy(newReturnTriageAnswers = Left(multipleDisposalsTriageAnswers))
                    _ <- EitherT(
                          updateSession(sessionStore, request)(_.copy(journeyStatus = Some(newState)))
                        )
                  } yield ()

                  result.fold({ e =>
                    logger.warn("Could not update session", e)
                    errorHandler.errorResult()
                  }, _ => Redirect(routes.MultipleDisposalsTriageController.checkYourAnswers()))

                }
              }
            )
      }
  }

  private def updateTaxYearToAnswers(
    taxYearExchanged: Boolean,
    taxYear: Option[TaxYear],
    answers: MultipleDisposalsTriageAnswers
  ): Either[Error, MultipleDisposalsTriageAnswers] =
    taxYear match {
      case None =>
        Left(Error("You cannot use this service"))
      case Some(t) =>
        Right(
          answers.fold[MultipleDisposalsTriageAnswers](
            incomplete =>
              incomplete.copy(
                taxYearAfter6April2020 = Some(taxYearExchanged),
                taxYear                = Some(t)
              ),
            complete =>
              IncompleteMultipleDisposalsAnswers(
                individualUserType           = Some(complete.individualUserType),
                numberOfProperties           = Some(complete.numberOfProperties),
                wasAUKResident               = Some(complete.countryOfResidence.isUk()),
                countryOfResidence           = Some(complete.countryOfResidence),
                wereAllPropertiesResidential = Some(complete.assetType.isResidential()),
                assetType                    = Some(complete.assetType),
                taxYearAfter6April2020       = Some(taxYearExchanged),
                taxYear                      = Some(t)
              )
          )
        )
    }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withMultipleDisposalTriageAnswers(request) {
      case (_, _, triageAnswers) =>
        triageAnswers match {
          case IncompleteMultipleDisposalsAnswers(None, _, _, _, _, _, _, _) =>
            Redirect(routes.InitialTriageQuestionsController.howManyProperties())

          case IncompleteMultipleDisposalsAnswers(Some(_), None, _, _, _, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.guidance())

          case IncompleteMultipleDisposalsAnswers(_, None, _, _, _, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.howManyDisposals())

          case IncompleteMultipleDisposalsAnswers(_, _, None, _, _, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.wereYouAUKResident())

          case IncompleteMultipleDisposalsAnswers(_, _, Some(true), _, None, _, _, _) =>
            Redirect(routes.MultipleDisposalsTriageController.wereAllPropertiesResidential())

          case IncompleteMultipleDisposalsAnswers(_, _, Some(false), _, _, _, _, _) =>
            Ok("Non-UK Residents not handled yet")

          case IncompleteMultipleDisposalsAnswers(_, _, _, _, _, _, None, _) =>
            Redirect(routes.MultipleDisposalsTriageController.whenWereContractsExchanged())

          case IncompleteMultipleDisposalsAnswers(_, _, _, _, _, _, Some(taxYear), _) =>
            Ok(s"Were all properties contracts exchanged after 06th April, 2020: $taxYear")

          case c: CompleteMultipleDisposalsAnswers =>
            Ok(s"Got $c")

        }
    }
  }

  private def assetType(isResidential: Boolean): AssetType =
    if (isResidential) AssetType.Residential else AssetType.NonResidential

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

  val taxYearExchangedForm: Form[Boolean] =
    Form(
      mapping(
        "taxYear" -> of(
          FormUtils.radioFormFormatter("taxYear", List(true, false))
        )
      )(identity)(Some(_))
    )

}
