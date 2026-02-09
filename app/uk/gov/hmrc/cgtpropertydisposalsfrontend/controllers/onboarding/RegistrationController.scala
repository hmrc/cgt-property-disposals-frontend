/*
 * Copyright 2023 HM Revenue & Customs
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
import cats.instances.int._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, number}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.{routes => onboardingRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.metrics.Metrics
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.TryingToGetIndividualsFootprint
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{AlreadySubscribedWithDifferentGGAccount, Registering, RegistrationStatus, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.AddressSource
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.email.EmailSource
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{CgtReference, SapNumber}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, ContactNameSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionResponse.{AlreadySubscribed, SubscriptionSuccessful}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{RegistrationRequestEvent, SubscriptionRequestEvent, WrongGGAccountEvent}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.{RegistrationDetails, SubscribedDetails, SubscriptionDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, given}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RegistrationController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val auditService: AuditService,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  subscriptionService: SubscriptionService,
  metrics: Metrics,
  selectEntityTypePage: views.html.onboarding.registration.select_entity_type,
  wrongGGAccountForTrustPage: views.html.onboarding.wrong_gg_account_for_trust,
  checkYourDetailsPage: views.html.onboarding.registration.check_your_details,
  cc: MessagesControllerComponents
)(implicit
  viewConfig: ViewConfig,
  ec: ExecutionContext
) extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {
  import RegistrationController._

  private def withValidUser(request: RequestWithSessionData[?])(
    f: Either[TryingToGetIndividualsFootprint, RegistrationStatus] => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(
            u @ TryingToGetIndividualsFootprint(Some(false), Some(false), _, _)
          ) =>
        f(Left(u))

      case Some(r: RegistrationStatus) =>
        f(Right(r))

      case _ =>
        SeeOther(controllers.routes.StartController.start().url)
    }

  def selectEntityType(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidUser(request) { status =>
        val form = {
          val blankForm = RegistrationController.selectEntityTypeForm
          status.fold(
            _ => blankForm,
            {
              case RegistrationStatus.IndividualWantsToRegisterTrust(_) =>
                blankForm.fill(EntityType.Trust)

              case _ =>
                blankForm.fill(EntityType.Individual)
            }
          )
        }

        Ok(
          selectEntityTypePage(
            form,
            onboardingRoutes.InsufficientConfidenceLevelController.doYouHaveAnSaUtr()
          )
        )
      }
    }

  def selectEntityTypeSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidUser(request) { status =>
        RegistrationController.selectEntityTypeForm
          .bindFromRequest()
          .fold(
            e =>
              BadRequest(
                selectEntityTypePage(
                  e,
                  onboardingRoutes.InsufficientConfidenceLevelController
                    .enterSautrAndName()
                )
              ),
            { entityType =>
              val ggCredId = status.fold(_.ggCredId, _.ggCredId)

              val (newRegistrationStatus, redirectTo): (
                RegistrationStatus,
                Call
              ) = entityType match {
                case EntityType.Individual =>
                  RegistrationStatus.IndividualSupplyingInformation(
                    None,
                    None,
                    status.fold(_.ggEmail, _ => None),
                    status.fold(
                      _.ggEmail.map(_ => EmailSource.GovernmentGateway),
                      _.emailSource
                    ),
                    ggCredId
                  ) -> name.routes.RegistrationEnterIndividualNameController
                    .enterIndividualName()
                case EntityType.Trust      =>
                  metrics.individualWantingToRegisterTrustCounter.inc()
                  RegistrationStatus.IndividualWantsToRegisterTrust(ggCredId) ->
                    onboardingRoutes.RegistrationController.wrongGGAccountForTrusts()
              }

              (status, newRegistrationStatus) match {
                case (
                      Right(
                        _: RegistrationStatus.IndividualSupplyingInformation
                      ),
                      _: RegistrationStatus.IndividualSupplyingInformation
                    ) | (
                      Right(
                        RegistrationStatus.IndividualWantsToRegisterTrust(_)
                      ),
                      RegistrationStatus.IndividualWantsToRegisterTrust(_)
                    ) =>
                  Redirect(redirectTo)

                case _ =>
                  updateSession(sessionStore, request.toSession)(
                    _.copy(journeyStatus = Some(newRegistrationStatus))
                  ).map {
                    case Left(e)  =>
                      logger.warn("Could not update registration status", e)
                      errorHandler.errorResult()
                    case Right(_) =>
                      Redirect(redirectTo)
                  }
              }
            }
          )
      }
    }

  def wrongGGAccountForTrusts(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidUser(request) {
        case Right(RegistrationStatus.IndividualWantsToRegisterTrust(_)) =>
          Ok(
            wrongGGAccountForTrustPage(
              onboardingRoutes.RegistrationController.selectEntityType()
            )
          )

        case _ =>
          Redirect(onboardingRoutes.RegistrationController.selectEntityType())
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(r: RegistrationStatus.RegistrationReady) =>
          Ok(checkYourDetailsPage(r.registrationDetails))

        case Some(
              RegistrationStatus
                .IndividualSupplyingInformation(None, _, _, _, _)
            ) =>
          Redirect(
            name.routes.RegistrationEnterIndividualNameController
              .enterIndividualName()
          )

        case Some(
              RegistrationStatus
                .IndividualSupplyingInformation(_, None, _, _, _)
            ) =>
          Redirect(address.routes.RegistrationEnterAddressController.isUk())

        case Some(
              RegistrationStatus.IndividualSupplyingInformation(
                Some(name),
                Some(address),
                None,
                _,
                ggCredId
              )
            ) =>
          updateSession(sessionStore, request.toSession)(
            _.copy(journeyStatus =
              Some(
                RegistrationStatus
                  .IndividualMissingEmail(name, address, ggCredId)
              )
            )
          ).map {
            case Left(error) =>
              logger.warn("Could not update session for enter registration email journey", error)
              errorHandler.errorResult()

            case Right(_) =>
              Redirect(
                email.routes.RegistrationEnterEmailController.enterEmail()
              )
          }

        case Some(
              RegistrationStatus
                .IndividualSupplyingInformation(
                  Some(name),
                  Some(address),
                  Some(email),
                  Some(emailSource),
                  ggCredId
                )
            ) =>
          val r = RegistrationStatus.RegistrationReady(
            RegistrationDetails(name, email, address, emailSource),
            ggCredId
          )
          updateSession(sessionStore, request.toSession)(_.copy(journeyStatus = Some(r)))
            .map {
              case Left(error) =>
                logger.warn("Could not update session for registration ready", error)
                errorHandler.errorResult()

              case Right(_) =>
                Ok(checkYourDetailsPage(r.registrationDetails))
            }

        case _ =>
          Redirect(controllers.routes.StartController.start())

      }

    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(
              RegistrationStatus
                .RegistrationReady(registrationDetails, ggCredId)
            ) =>
          val result = for {
            _                    <- EitherT(
                                      updateSession(sessionStore, request.toSession)(
                                        _.copy(
                                          journeyStatus = Some(Registering)
                                        )
                                      )
                                    )
            registrationResponse <- {
              auditService.sendEvent(
                "registrationRequest",
                RegistrationRequestEvent.fromRegistrationDetails(
                  registrationDetails
                ),
                "registration-request"
              )
              subscriptionService.registerWithoutId(registrationDetails)
            }
            subscriptionResponse <- {
              val subscriptionDetails = toSubscriptionDetails(
                registrationDetails,
                registrationResponse.sapNumber
              )

              auditService
                .sendEvent(
                  "subscriptionRequest",
                  SubscriptionRequestEvent.fromSubscriptionDetails(
                    subscriptionDetails
                  ),
                  "subscription-request"
                )
              subscriptionService.subscribe(subscriptionDetails, request.authenticatedRequest.request.messages.lang)
            }
            _                    <- EitherT(subscriptionResponse match {
                                      case SubscriptionSuccessful(cgtReferenceNumber) =>
                                        updateSession(sessionStore, request.toSession)(
                                          _.copy(
                                            journeyStatus = Some(
                                              Subscribed(
                                                SubscribedDetails(
                                                  Right(registrationDetails.name),
                                                  Some(registrationDetails.emailAddress),
                                                  registrationDetails.address,
                                                  ContactName(
                                                    s"${registrationDetails.name.firstName} ${registrationDetails.name.lastName}"
                                                  ),
                                                  CgtReference(cgtReferenceNumber),
                                                  None,
                                                  registeredWithId = false
                                                ),
                                                ggCredId,
                                                None,
                                                List.empty,
                                                List.empty
                                              )
                                            )
                                          )
                                        )
                                      case AlreadySubscribed                          =>
                                        updateSession(sessionStore, request.toSession)(
                                          _.copy(journeyStatus =
                                            Some(
                                              AlreadySubscribedWithDifferentGGAccount(ggCredId, None)
                                            )
                                          )
                                        )
                                    })
          } yield subscriptionResponse

          result.fold(
            { e =>
              logger.warn("Could not register without id and subscribe", e)
              errorHandler.errorResult()
            },
            {
              case SubscriptionSuccessful(cgtReferenceNumber) =>
                logger.info(s"Successfully subscribed with cgt id $cgtReferenceNumber")
                Redirect(onboardingRoutes.SubscriptionController.subscribed())
              case AlreadySubscribed                          =>
                logger.info("Response to subscription request indicated that the user has already subscribed to cgt")
                auditService.sendEvent(
                  "accessWithWrongGGAccount",
                  WrongGGAccountEvent(None, ggCredId.value),
                  "access-with-wrong-gg-account"
                )
                Redirect(onboardingRoutes.SubscriptionController.alreadySubscribedWithDifferentGGAccount())
            }
          )

        case _ =>
          Redirect(controllers.routes.StartController.start())
      }
    }

  private def toSubscriptionDetails(
    registrationDetails: RegistrationDetails,
    sapNumber: SapNumber
  ): SubscriptionDetails =
    SubscriptionDetails(
      Right(registrationDetails.name),
      registrationDetails.emailAddress,
      registrationDetails.address,
      ContactName(
        s"${registrationDetails.name.firstName} ${registrationDetails.name.lastName}"
      ),
      sapNumber,
      registrationDetails.emailSource,
      AddressSource.ManuallyEntered,
      ContactNameSource.ManuallyEntered
    )

}

object RegistrationController {

  sealed trait EntityType extends Product with Serializable

  object EntityType {
    case object Individual extends EntityType
    case object Trust extends EntityType
  }

  private val selectEntityTypeForm =
    Form(
      mapping(
        "entityType" -> number
          .verifying("invalid", a => a === 0 || a === 1)
          .transform[EntityType](
            value => if (value === 0) EntityType.Trust else EntityType.Individual,
            {
              case EntityType.Individual => 1
              case EntityType.Trust      => 0
            }
          )
      )(identity)(Some(_))
    )
}
