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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.name

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts.{routes => nameRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{ContactNameController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.SubscriptionStatus.SubscriptionReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, ContactNameSource}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.OnboardingAuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubscriptionChangeContactNameController @Inject()(
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  cc: MessagesControllerComponents,
  val auditService: OnboardingAuditService,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  val enterContactNamePage: views.html.onboarding.contactname.contact_name
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging
    with ContactNameController[SubscriptionReady] {

  override val isSubscribedJourney: Boolean = false

  override def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, SubscriptionReady)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, s: SubscriptionReady)) => Right(sessionData -> s)
      case _                                         => Left(Redirect(controllers.routes.StartController.start()))
    }

  override def updateContactName(journey: SubscriptionReady, contactName: ContactName)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, Error, SubscriptionReady] = {
    auditService.sendSubscriptionContactNameChangedEvent(
      journey.subscriptionDetails.contactName.value,
      contactName.value,
      nameRoutes.SubscribedChangeContactNameController.enterContactNameSubmit().url
    )

    val source =
      if (journey.subscriptionDetails.contactName === contactName) journey.subscriptionDetails.contactNameSource
      else ContactNameSource.ManuallyEntered

    EitherT.rightT[Future, Error](
      journey.copy(
        subscriptionDetails = journey.subscriptionDetails.copy(contactName = contactName, contactNameSource = source)
      )
    )
  }

  override def contactName(journey: SubscriptionReady): Option[ContactName] =
    Some(journey.subscriptionDetails.contactName)

  override protected lazy val backLinkCall: Call =
    controllers.onboarding.routes.SubscriptionController.checkYourDetails()
  override protected lazy val enterContactNameSubmitCall: Call =
    routes.SubscriptionChangeContactNameController.enterContactNameSubmit()
  override protected lazy val continueCall: Call =
    controllers.onboarding.routes.SubscriptionController.checkYourDetails()

}
