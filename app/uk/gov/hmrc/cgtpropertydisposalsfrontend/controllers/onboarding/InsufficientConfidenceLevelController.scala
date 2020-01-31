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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.InsufficientConfidenceLevelController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.TryingToGetIndividualsFootprint
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{AlreadySubscribedWithDifferentGGAccount, SubscriptionStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, GGCredId, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{HandOffTIvEvent, WrongGGAccountEvent}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails.{IndividualNameMatchDetails, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecord, NameMatchError, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.email.Email
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error, JourneyStatus}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.onboarding.BusinessPartnerRecordNameMatchRetryStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.BusinessPartnerRecordNameMatchRetryService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class InsufficientConfidenceLevelController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  val config: Configuration,
  auditService: AuditService,
  bprNameMatchService: BusinessPartnerRecordNameMatchRetryService,
  sautrNameMatchRetryStore: BusinessPartnerRecordNameMatchRetryStore,
  doYouHaveANinoPage: views.html.onboarding.do_you_have_a_nino,
  doYouHaveAnSaUtrPage: views.html.onboarding.do_you_have_an_sa_utr,
  enterSautrAndNamePage: views.html.onboarding.enter_sa_utr_and_name,
  tooManyUnsuccessfulNameMatchesPage: views.html.onboarding.too_many_name_match_attempts,
  cc: MessagesControllerComponents
)(
  implicit viewConfig: ViewConfig,
  ec: ExecutionContext
) extends FrontendController(cc)
    with IvBehaviour
    with Logging
    with WithAuthAndSessionDataAction
    with SessionUpdates {

  private def withInsufficientConfidenceLevelUser(
    f: TryingToGetIndividualsFootprint => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(i: TryingToGetIndividualsFootprint) => f(i)
      case _                                        => Redirect(controllers.routes.StartController.start())
    }

  def doYouHaveNINO(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request: RequestWithSessionData[AnyContent] =>
      withInsufficientConfidenceLevelUser {
        case TryingToGetIndividualsFootprint(hasNino, _, _, _) =>
          val form = hasNino.fold(haveANinoForm)(haveANinoForm.fill)
          Ok(doYouHaveANinoPage(form))
      }
  }

  def doYouHaveNINOSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser { insufficientConfidenceLevel =>
      InsufficientConfidenceLevelController.haveANinoForm
        .bindFromRequest()
        .fold(
          e => BadRequest(doYouHaveANinoPage(e)),
          hasNino =>
            updateSession(sessionStore, request)(
              _.copy(journeyStatus = Some(insufficientConfidenceLevel.copy(hasNino = Some(hasNino))))
            ).map {
              case Left(e) =>
                logger.warn("Could not update session after has NINO page submit", e)
                errorHandler.errorResult()

              case Right(_) =>
                if (hasNino) {
                  auditService.sendEvent(
                    "handOffToIv",
                    HandOffTIvEvent(insufficientConfidenceLevel.ggCredId.value, request.uri),
                    "handoff-to-iv"
                  )
                  redirectToIv
                } else {
                  Redirect(routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr())
                }
            }
        )
    }
  }

  def doYouHaveAnSaUtr(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser {
      case TryingToGetIndividualsFootprint(hasNino, hasSaUtr, _, _) =>
        hasNino.fold(
          SeeOther(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)
        ) { _ =>
          val form = hasSaUtr.fold(hasSaUtrForm)(hasSaUtrForm.fill)
          Ok(doYouHaveAnSaUtrPage(form, routes.InsufficientConfidenceLevelController.doYouHaveNINO()))
        }
    }
  }

  def doYouHaveSaUtrSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser { insufficientConfidenceLevel =>
      insufficientConfidenceLevel.hasNino.fold[Future[Result]](
        SeeOther(routes.InsufficientConfidenceLevelController.doYouHaveNINO().url)
      ) { _ =>
        hasSaUtrForm
          .bindFromRequest()
          .fold(
            e => BadRequest(doYouHaveAnSaUtrPage(e, routes.InsufficientConfidenceLevelController.doYouHaveNINO())),
            hasSautr =>
              updateSession(sessionStore, request)(
                _.copy(journeyStatus = Some(insufficientConfidenceLevel.copy(hasSautr = Some(hasSautr))))
              ).map {
                case Left(e) =>
                  logger.warn("Could not update session after has SAUTR page submit", e)
                  errorHandler.errorResult()

                case Right(_) =>
                  if (hasSautr)
                    Redirect(routes.InsufficientConfidenceLevelController.enterSautrAndName())
                  else
                    Redirect(routes.RegistrationController.selectEntityType())
              }
          )
      }
    }
  }

  def enterSautrAndName(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser { insufficientConfidenceLevel =>
      (insufficientConfidenceLevel.hasNino, insufficientConfidenceLevel.hasSautr) match {
        case (Some(false), Some(true)) =>
          bprNameMatchService
            .getNumberOfUnsuccessfulAttempts[IndividualNameMatchDetails](insufficientConfidenceLevel.ggCredId)
            .fold(
              handleNameMatchError, { _ =>
                Ok(
                  enterSautrAndNamePage(
                    InsufficientConfidenceLevelController.sautrAndNameForm,
                    routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr()
                  )
                )
              }
            )

        case _ =>
          Redirect(routes.InsufficientConfidenceLevelController.doYouHaveNINO())
      }
    }
  }

  def enterSautrAndNameSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withInsufficientConfidenceLevelUser { insufficientConfidenceLevel =>
      (insufficientConfidenceLevel.hasNino, insufficientConfidenceLevel.hasSautr) match {
        case (Some(false), Some(true)) =>
          val result =
            for {
              unsuccessfulAttempts <- bprNameMatchService.getNumberOfUnsuccessfulAttempts[IndividualNameMatchDetails](
                                       insufficientConfidenceLevel.ggCredId
                                     )
              bprWithCgtReference <- {
                InsufficientConfidenceLevelController.sautrAndNameForm
                  .bindFromRequest()
                  .fold[EitherT[
                    Future,
                    NameMatchError[IndividualNameMatchDetails],
                    (BusinessPartnerRecord, Option[CgtReference])
                  ]](
                    e => EitherT.fromEither[Future](Left(NameMatchError.ValidationError(e))), {
                      individualNameMatchDetails =>
                        attemptNameMatchAndUpdateSession(
                          individualNameMatchDetails,
                          insufficientConfidenceLevel.ggCredId,
                          insufficientConfidenceLevel.ggEmail,
                          unsuccessfulAttempts
                        )
                    }
                  )
              }
            } yield bprWithCgtReference

          result
            .fold(
              handleNameMatchError, {
                case (_, maybeCgtReference) =>
                  Redirect(
                    maybeCgtReference.fold(controllers.routes.StartController.start()) { cgtReference =>
                      auditService.sendEvent(
                        "accessWithWrongGGAccount",
                        WrongGGAccountEvent(Some(cgtReference.value), insufficientConfidenceLevel.ggCredId.value),
                        "access-with-wrong-gg-account"
                      )
                      routes.SubscriptionController.alreadySubscribedWithDifferentGGAccount()
                    }
                  )
              }
            )

        case _ =>
          Redirect(routes.InsufficientConfidenceLevelController.doYouHaveNINO())
      }
    }
  }

  def tooManyAttempts(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request: RequestWithSessionData[AnyContent] =>
      withInsufficientConfidenceLevelUser { insufficientConfidenceLevel =>
        bprNameMatchService
          .getNumberOfUnsuccessfulAttempts[IndividualNameMatchDetails](
            insufficientConfidenceLevel.ggCredId
          )
          .value
          .map {
            case Left(NameMatchError.TooManyUnsuccessfulAttempts()) =>
              Ok(tooManyUnsuccessfulNameMatchesPage(routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr()))
            case Left(otherNameMatchError) => handleNameMatchError(otherNameMatchError)
            case Right(_)                  => Redirect(routes.InsufficientConfidenceLevelController.enterSautrAndName())
          }
      }
  }

  private def attemptNameMatchAndUpdateSession(
    individualNameMatchDetails: IndividualNameMatchDetails,
    ggCredId: GGCredId,
    ggEmail: Option[Email],
    previousUnsuccessfulAttempt: Option[UnsuccessfulNameMatchAttempts[IndividualNameMatchDetails]]
  )(
    implicit hc: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): EitherT[Future, NameMatchError[IndividualNameMatchDetails], (BusinessPartnerRecord, Option[CgtReference])] =
    for {
      bprWithCgtReference <- bprNameMatchService
                              .attemptBusinessPartnerRecordNameMatch(
                                individualNameMatchDetails,
                                ggCredId,
                                previousUnsuccessfulAttempt
                              )
                              .subflatMap {
                                case (bpr, cgtReference) =>
                                  if (bpr.name.isLeft) {
                                    Left(
                                      NameMatchError
                                        .BackendError(Error("Found BPR for trust but expected one for an individual"))
                                    )
                                  } else {
                                    Right(bpr -> cgtReference)
                                  }
                              }
      _ <- EitherT(
            updateSession(sessionStore, request)(
              _.copy(journeyStatus = Some(
                bprWithCgtReference._2.fold[JourneyStatus](
                  SubscriptionStatus.SubscriptionMissingData(bprWithCgtReference._1, None, ggCredId, ggEmail)
                )(cgtReference => AlreadySubscribedWithDifferentGGAccount(ggCredId, Some(cgtReference)))
              )
              )
            )
          ).leftMap[NameMatchError[IndividualNameMatchDetails]](NameMatchError.BackendError)
    } yield bprWithCgtReference

  private def handleNameMatchError(
    nameMatchError: NameMatchError[IndividualNameMatchDetails]
  )(implicit request: RequestWithSessionData[_]): Result = nameMatchError match {
    case NameMatchError.BackendError(error) =>
      logger.warn("Could not get BPR with entered SA UTR", error)
      errorHandler.errorResult()

    case NameMatchError.ValidationError(formWithErrors) =>
      BadRequest(enterSautrAndNamePage(formWithErrors, routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr()))

    case NameMatchError.NameMatchFailed(unsuccessfulAttempts) =>
      val form = InsufficientConfidenceLevelController.sautrAndNameForm
        .fill(unsuccessfulAttempts.lastDetailsTried)
        .withUnsuccessfulAttemptsError(unsuccessfulAttempts)
      BadRequest(enterSautrAndNamePage(form, routes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr()))

    case NameMatchError.TooManyUnsuccessfulAttempts() =>
      Redirect(routes.InsufficientConfidenceLevelController.tooManyAttempts())
  }

}

object InsufficientConfidenceLevelController {

  val haveANinoForm: Form[Boolean] =
    Form(
      mapping(
        "hasNino" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

  val hasSaUtrForm: Form[Boolean] =
    Form(
      mapping(
        "hasSaUtr" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

  val sautrAndNameForm: Form[IndividualNameMatchDetails] =
    Form(
      mapping(
        "saUtr"     -> SAUTR.mapping,
        "firstName" -> IndividualName.mapping,
        "lastName"  -> IndividualName.mapping
      ) {
        case (sautr, firstName, lastName) =>
          IndividualNameMatchDetails(IndividualName(firstName, lastName), sautr)
      } { individualNameMatchDetails =>
        Some(
          (
            individualNameMatchDetails.sautr,
            individualNameMatchDetails.name.firstName,
            individualNameMatchDetails.name.lastName
          )
        )
      }
    )

  implicit class SAUTRAndNameFormOps(private val form: Form[IndividualNameMatchDetails]) extends AnyVal {

    def withUnsuccessfulAttemptsError(
      numberOfUnsuccessfulNameMatchAttempts: UnsuccessfulNameMatchAttempts[IndividualNameMatchDetails]
    ): Form[IndividualNameMatchDetails] =
      form
        .withGlobalError(
          "enterSaUtr.error.notFound",
          numberOfUnsuccessfulNameMatchAttempts.unsuccessfulAttempts,
          numberOfUnsuccessfulNameMatchAttempts.maximumAttempts
        )
  }

}
