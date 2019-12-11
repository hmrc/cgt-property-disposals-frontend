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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.DeterminingIfOrganisationIsTrustController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.Metrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.DeterminingIfOrganisationIsTrust
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UserType.Organisation
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{GGCredId, TRN}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.TrustName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails.TrustNameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.{BusinessPartnerRecord, NameMatchError, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.BusinessPartnerRecordNameMatchRetryService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class DeterminingIfOrganisationIsTrustController @Inject()(
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  errorHandler: ErrorHandler,
  sessionStore: SessionStore,
  bprNameMatchService: BusinessPartnerRecordNameMatchRetryService,
  metrics: Metrics,
  doYouWantToReportForATrustPage: views.html.onboarding.subscription.do_you_want_to_report_for_a_trust,
  reportWithCorporateTaxPage: views.html.onboarding.subscription.report_with_corporate_tax,
  doYouHaveATrnPage: views.html.onboarding.subscription.do_you_have_a_trn,
  registerYourTrustPage: views.html.onboarding.register_your_trust,
  enterTrnAndNamePage: views.html.onboarding.subscription.enter_trn_and_trust_name,
  tooManyAttemptsPage: views.html.onboarding.subscription.too_many_trn_name_match_attempts,
  cc: MessagesControllerComponents
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def withValidUser(
    request: RequestWithSessionData[_]
  )(f: DeterminingIfOrganisationIsTrust => Future[Result]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(d: DeterminingIfOrganisationIsTrust) => f(d)
      case _                                         => Redirect(controllers.routes.StartController.start())
    }

  def doYouWantToReportForATrust(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { determiningIfOrganisationIsTrust =>
      val form =
        determiningIfOrganisationIsTrust.isReportingForTrust.fold(doYouWantToReportForATrustForm)(
          doYouWantToReportForATrustForm.fill
        )
      Ok(doYouWantToReportForATrustPage(form, controllers.routes.StartController.weNeedMoreDetails()))
    }
  }

  def doYouWantToReportForATrustSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withValidUser(request) { determiningIfOrganisationIsTrust =>
        doYouWantToReportForATrustForm
          .bindFromRequest()
          .fold(
            formWithError =>
              BadRequest(doYouWantToReportForATrustPage(formWithError, controllers.routes.StartController.weNeedMoreDetails())), {
              isReportingForTrust =>
                updateSession(sessionStore, request)(
                  _.copy(
                    journeyStatus = Some(
                      DeterminingIfOrganisationIsTrust(
                        determiningIfOrganisationIsTrust.ggCredId,
                        Some(isReportingForTrust),
                        None
                      )
                    )
                  )
                ).map {
                  case Left(e) =>
                    logger.warn("Could not update session data with reporting for trust answer", e)
                    errorHandler.errorResult(request.sessionData.flatMap(_.userType))

                case Right(_) =>
                  if (isReportingForTrust)
                    Redirect(routes.DeterminingIfOrganisationIsTrustController.doYouHaveATrn())
                  else {
                    metrics.nonTrustOrganisationCounter.inc()
                    Redirect(routes.DeterminingIfOrganisationIsTrustController.reportWithCorporateTax())
                  }
              }
            }
          )
      }
  }

  def reportWithCorporateTax(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { determiningIfOrganisationIsTrust =>
      if (determiningIfOrganisationIsTrust.isReportingForTrust.contains(false)) {
        Ok(reportWithCorporateTaxPage(routes.DeterminingIfOrganisationIsTrustController.doYouWantToReportForATrust()))
      } else {
        Redirect(controllers.routes.StartController.start())
      }
    }
  }

  def doYouHaveATrn(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { determiningIfOrganisationIsTrust =>
      if (determiningIfOrganisationIsTrust.isReportingForTrust.contains(true)) {
        val form =
          determiningIfOrganisationIsTrust.hasTrn.fold(doYouHaveATrnForm)(doYouHaveATrnForm.fill)
        Ok(doYouHaveATrnPage(form, routes.DeterminingIfOrganisationIsTrustController.doYouWantToReportForATrust()))
      } else {
        Redirect(controllers.routes.StartController.start())
      }
    }
  }

  def doYouHaveATrnSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { determiningIfOrganisationIsTrust =>
      if (determiningIfOrganisationIsTrust.isReportingForTrust.contains(true)) {
        doYouHaveATrnForm
          .bindFromRequest()
          .fold(
            formWithError =>
              BadRequest(
                doYouHaveATrnPage(
                  formWithError,
                  routes.DeterminingIfOrganisationIsTrustController.doYouWantToReportForATrust()
                )
              ), { hasTrn =>
              updateSession(sessionStore, request)(
                _.copy(journeyStatus = Some(determiningIfOrganisationIsTrust.copy(hasTrn = Some(hasTrn))))
              ).map {
                case Left(e) =>
                  logger.warn("Could not update session data with has TRN answer", e)
                  errorHandler.errorResult(request.sessionData.flatMap(_.userType))

                case Right(_) =>
                  if (hasTrn)
                    Redirect(routes.DeterminingIfOrganisationIsTrustController.enterTrn())
                  else {
                    metrics.unregisteredTrustCounter.inc()
                    Redirect(routes.DeterminingIfOrganisationIsTrustController.registerYourTrust())
                  }
              }
            }
          )
      } else {
        Redirect(controllers.routes.StartController.start())
      }

    }
  }

  def enterTrn(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { determiningIfOrganisationIsTrust =>
      determiningIfOrganisationIsTrust.hasTrn match {
        case Some(true) =>
          bprNameMatchService
            .getNumberOfUnsuccessfulAttempts[TrustNameMatchDetails](determiningIfOrganisationIsTrust.ggCredId)
            .fold(
              handleNameMatchError, { numberOfUnsuccessfulNameMatchAttempts =>
                val form = numberOfUnsuccessfulNameMatchAttempts.fold(
                  enterTrnAndNameForm
                )(
                  enterTrnAndNameForm.withUnsuccessfulAttemptsError
                )
                Ok(enterTrnAndNamePage(form, routes.DeterminingIfOrganisationIsTrustController.doYouHaveATrn()))
              }
            )
        case _ => Redirect(controllers.routes.StartController.start())
      }
    }
  }

  def enterTrnSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { determiningIfOrganisationIsTrust =>
      determiningIfOrganisationIsTrust.hasTrn match {
        case Some(true) =>
          val result =
            for {
              unsuccessfulAttempts <- bprNameMatchService.getNumberOfUnsuccessfulAttempts[TrustNameMatchDetails](
                                       determiningIfOrganisationIsTrust.ggCredId
                                     )
              bpr <- {
                enterTrnAndNameForm
                  .bindFromRequest()
                  .fold[EitherT[Future, NameMatchError[TrustNameMatchDetails], BusinessPartnerRecord]](
                    e => EitherT.fromEither[Future](Left(NameMatchError.ValidationError(e))), { trustNameMatchDetails =>
                      attemptNameMatchAndUpdateSession(
                        trustNameMatchDetails,
                        determiningIfOrganisationIsTrust.ggCredId,
                        unsuccessfulAttempts
                      )
                    }
                  )
              }
            } yield bpr

          result
            .fold(
              handleNameMatchError,
              _ => Redirect(controllers.routes.StartController.start())
            )

        case _ => Redirect(controllers.routes.StartController.start())
      }
    }
  }

  def registerYourTrust(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { _ =>
      Ok(registerYourTrustPage(routes.DeterminingIfOrganisationIsTrustController.doYouHaveATrn()))
    }
  }

  def tooManyAttempts(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidUser(request) { determiningIfOrganisationIsTrust =>
      bprNameMatchService
        .getNumberOfUnsuccessfulAttempts[TrustNameMatchDetails](
          determiningIfOrganisationIsTrust.ggCredId
        )
        .value
        .map {
          case Left(NameMatchError.TooManyUnsuccessfulAttempts()) => Ok(tooManyAttemptsPage())
          case Left(otherNameMatchError)                          => handleNameMatchError(otherNameMatchError)
          case Right(_)                                           => Redirect(routes.DeterminingIfOrganisationIsTrustController.enterTrn())
        }
    }
  }

  private def attemptNameMatchAndUpdateSession(
    trustNameMatchDetails: TrustNameMatchDetails,
    ggCredId: GGCredId,
    previousUnsuccessfulAttempt: Option[UnsuccessfulNameMatchAttempts[TrustNameMatchDetails]]
  )(
    implicit hc: HeaderCarrier,
    request: RequestWithSessionData[_]
  ): EitherT[Future, NameMatchError[TrustNameMatchDetails], BusinessPartnerRecord] =
    for {
      bpr <- bprNameMatchService
              .attemptBusinessPartnerRecordNameMatch(
                trustNameMatchDetails,
                ggCredId,
                previousUnsuccessfulAttempt
              )
              .subflatMap(
                bpr =>
                  if (bpr.name.isRight) {
                    Left(NameMatchError.BackendError(Error("Found BPR for individual but expected one for a trust")))
                  } else {
                    Right(bpr)
                  }
              )
      _ <- EitherT(
            updateSession(sessionStore, request)(
              _.copy(journeyStatus = Some(SubscriptionStatus.SubscriptionMissingData(bpr, None, ggCredId)))
            )
          ).leftMap[NameMatchError[TrustNameMatchDetails]](NameMatchError.BackendError)
    } yield bpr

  private def handleNameMatchError(
    nameMatchError: NameMatchError[TrustNameMatchDetails]
  )(implicit request: RequestWithSessionData[_]): Result = nameMatchError match {
    case NameMatchError.BackendError(error) =>
      logger.warn("Could not get BPR with entered TRN", error)
      errorHandler.errorResult(request.sessionData.flatMap(_.userType))

    case NameMatchError.ValidationError(formWithErrors) =>
      BadRequest(enterTrnAndNamePage(formWithErrors, routes.DeterminingIfOrganisationIsTrustController.doYouHaveATrn()))

    case NameMatchError.NameMatchFailed(unsuccessfulAttempts) =>
      val form = enterTrnAndNameForm
        .fill(unsuccessfulAttempts.lastDetailsTried)
        .withUnsuccessfulAttemptsError(unsuccessfulAttempts)
      BadRequest(enterTrnAndNamePage(form, routes.DeterminingIfOrganisationIsTrustController.doYouHaveATrn()))

    case NameMatchError.TooManyUnsuccessfulAttempts() =>
      Redirect(routes.DeterminingIfOrganisationIsTrustController.tooManyAttempts())
  }

}

object DeterminingIfOrganisationIsTrustController {

  val doYouWantToReportForATrustForm: Form[Boolean] =
    Form(
      mapping(
        "isReportingForATrust" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

  val doYouHaveATrnForm: Form[Boolean] =
    Form(
      mapping(
        "hasTrn" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

  val enterTrnAndNameForm: Form[TrustNameMatchDetails] =
    Form(
      mapping(
        "trn"       -> TRN.mapping,
        "trustName" -> TrustName.mapping
      ) {
        case (trn, trustName) => TrustNameMatchDetails(TrustName(trustName), TRN(trn))
      } { trustNameMatchDetails =>
        Some((trustNameMatchDetails.trn.value, trustNameMatchDetails.name.value))
      }
    )

  implicit class TRNAndTrustNameFormOps(val form: Form[TrustNameMatchDetails]) extends AnyVal {

    def withUnsuccessfulAttemptsError(
      numberOfUnsuccessfulNameMatchAttempts: UnsuccessfulNameMatchAttempts[TrustNameMatchDetails]
    ): Form[TrustNameMatchDetails] =
      form
        .withGlobalError(
          "enterTrn.error.notFound",
          numberOfUnsuccessfulNameMatchAttempts.unsuccessfulAttempts,
          numberOfUnsuccessfulNameMatchAttempts.maximumAttempts
        )

  }

}
