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
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{AlreadySubscribedWithDifferentGGAccount, Subscribed}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscriptionResponse.{AlreadySubscribed, SubscriptionSuccessful}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.{OnboardingAuditService, SubscriptionService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class SubscriptionController @Inject()(
  subscriptionService: SubscriptionService,
  sessionStore: SessionStore,
  errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val subscriptionDetailsAction: SubscriptionReadyAction,
  val auditService: OnboardingAuditService,
  alreadySubscribedWithDifferentGGAccountPage: views.html.onboarding.already_subscribed_with_different_gg_account,
  checkYourDetailsPage: views.html.onboarding.subscription.check_your_details,
  subscribedPage: views.html.onboarding.subscription.subscribed,
  changeGGAccountPage: views.html.onboarding.change_gg_account
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithSubscriptionDetailsActions
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates {

  def checkYourDetails(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady { implicit request: RequestWithSubscriptionReady[AnyContent] =>
      Ok(checkYourDetailsPage(request.subscriptionReady.subscriptionDetails))
    }

  def checkYourDetailsSubmit(): Action[AnyContent] =
    authenticatedActionWithSubscriptionReady.async { implicit request =>
      val details = request.subscriptionReady.subscriptionDetails
      val result = for {
        subscriptionResponse <- subscriptionService.subscribe(details)
        _ <- EitherT(
              subscriptionResponse match {
                case SubscriptionSuccessful(cgtReferenceNumber) =>
                  updateSession(sessionStore, request)(
                    _.copy(
                      journeyStatus = Some(
                        Subscribed(
                          SubscribedDetails(
                            details.name,
                            details.emailAddress,
                            details.address,
                            details.contactName,
                            CgtReference(cgtReferenceNumber),
                            None,
                            registeredWithId = true
                          ),
                          request.subscriptionReady.ggCredId
                        )
                      )
                    )
                  )
                case AlreadySubscribed =>
                  updateSession(sessionStore, request)(
                    _.copy(
                      journeyStatus = Some(AlreadySubscribedWithDifferentGGAccount(request.subscriptionReady.ggCredId))
                    )
                  )
              }
            )
      } yield subscriptionResponse

      result.fold(
        { e =>
          logger.warn("Could not subscribe", e)
          errorHandler.errorResult(request.sessionData.userType)
        }, {
          case SubscriptionSuccessful(cgtReferenceNumber) => {
            logger.info(s"Successfully subscribed with cgt id $cgtReferenceNumber")
            auditService
              .sendSubscriptionRequestEvent(
                details,
                routes.SubscriptionController.checkYourDetailsSubmit().url
              )
            Redirect(routes.SubscriptionController.subscribed())
          }
          case AlreadySubscribed =>
            logger.info("Response to subscription request indicated that the user has already subscribed to cgt")
            auditService.sendAccessWithWrongGGAccountEvent(
              request.subscriptionReady.ggCredId,
              routes.SubscriptionController.checkYourDetailsSubmit().url
            )
            Redirect(routes.SubscriptionController.alreadySubscribedWithDifferentGGAccount())
        }
      )
    }

  def subscribed(): Action[AnyContent] = authenticatedActionWithSessionData { implicit request =>
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(Subscribed(accountDetails, _)) => Ok(subscribedPage(accountDetails))
      case _                                   => Redirect(controllers.routes.StartController.start())
    }
  }

  def alreadySubscribedWithDifferentGGAccount(): Action[AnyContent] = authenticatedActionWithSessionData {
    implicit request =>
      request.sessionData.flatMap(_.journeyStatus) match {
        case Some(AlreadySubscribedWithDifferentGGAccount(_)) => Ok(alreadySubscribedWithDifferentGGAccountPage())
        case _                                                => Redirect(controllers.routes.StartController.start())
      }
  }

  def changeGGAccountForSubscription(): Action[AnyContent] = authenticatedActionWithSubscriptionReady {
    implicit request =>
      Ok(changeGGAccountPage(routes.SubscriptionController.checkYourDetails()))
  }

}
