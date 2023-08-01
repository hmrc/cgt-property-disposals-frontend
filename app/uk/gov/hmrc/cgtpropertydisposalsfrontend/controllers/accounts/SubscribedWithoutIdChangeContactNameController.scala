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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.accounts

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Call, MessagesControllerComponents, Request, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.name.IndividualNameController
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.Subscribed
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{ContactName, IndividualName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedUpdateDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.SubscribedContactNameChangedEvent
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.SubscriptionService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubscribedWithoutIdChangeContactNameController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val subscriptionService: SubscriptionService,
  cc: MessagesControllerComponents,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  val enterNamePage: views.html.onboarding.name.enter_name,
  auditService: AuditService
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging
    with IndividualNameController[Subscribed] {

  override val isSubscribedJourney: Boolean = true

  override def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, Subscribed)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, r: Subscribed)) => Right(sessionData -> r)
      case _                                  => Left(Redirect(controllers.routes.StartController.start()))
    }

  override def updateName(journey: Subscribed, name: IndividualName)(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, Subscribed] = {
    val contactName            = s"${name.firstName} ${name.lastName}"
    val journeyWithUpdatedName =
      journey.subscribedDetails
        .copy(name = Right(name), contactName = ContactName(contactName))

    if (journey.subscribedDetails === journeyWithUpdatedName)
      EitherT.rightT[Future, Error](journey)
    else {
      auditService.sendEvent(
        "contactNameChanged",
        SubscribedContactNameChangedEvent(
          journey.subscribedDetails.contactName.value,
          contactName,
          journey.subscribedDetails.cgtReference.value,
          journey.agentReferenceNumber.isDefined,
          journey.agentReferenceNumber.map(_.value)
        ),
        "contact-name-changed"
      )

      subscriptionService
        .updateSubscribedDetails(
          SubscribedUpdateDetails(
            journeyWithUpdatedName,
            journey.subscribedDetails
          )
        )
        .map(_ => journey.copy(journeyWithUpdatedName))
    }

  }

  override def name(journey: Subscribed): Option[IndividualName] =
    journey.subscribedDetails.name.toOption

  override protected lazy val backLinkCall: Call        =
    controllers.accounts.routes.AccountController.manageYourDetails()
  override protected lazy val enterNameSubmitCall: Call =
    routes.SubscribedWithoutIdChangeContactNameController
      .enterIndividualNameSubmit()
  override protected lazy val continueCall: Call        =
    controllers.accounts.routes.AccountController.contactNameUpdated()

}
