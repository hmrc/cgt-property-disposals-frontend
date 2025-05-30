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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.onboarding.name

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Call, MessagesControllerComponents, Request, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.RegistrationReady
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.RegistrationContactNameChangedEvent
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RegistrationChangeIndividualNameController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val auditService: AuditService,
  cc: MessagesControllerComponents,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  val enterNamePage: views.html.onboarding.name.enter_name
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging
    with IndividualNameController[RegistrationReady] {

  override val isSubscribedJourney: Boolean = false

  override def validJourney(
    request: RequestWithSessionData[?]
  ): Either[Result, (SessionData, RegistrationReady)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, r: RegistrationReady)) => Right(sessionData -> r)
      case _                                         => Left(Redirect(controllers.routes.StartController.start()))
    }

  override def updateName(journey: RegistrationReady, name: IndividualName)(implicit
    hc: HeaderCarrier,
    request: Request[?]
  ): EitherT[Future, Error, RegistrationReady] = {
    auditService.sendEvent(
      "registrationContactNameChanged",
      RegistrationContactNameChangedEvent(
        s"${journey.registrationDetails.name.firstName} ${journey.registrationDetails.name.lastName}",
        s"${name.firstName} ${name.lastName}"
      ),
      "registration-contact-name-changed"
    )
    EitherT.rightT[Future, Error](
      journey.copy(registrationDetails = journey.registrationDetails.copy(name = name))
    )
  }

  override def name(journey: RegistrationReady): Option[IndividualName] =
    Some(journey.registrationDetails.name)

  override protected lazy val backLinkCall: Call        =
    controllers.onboarding.routes.RegistrationController.checkYourAnswers()
  override protected lazy val enterNameSubmitCall: Call =
    routes.RegistrationChangeIndividualNameController
      .enterIndividualNameSubmit()
  override protected lazy val continueCall: Call        =
    controllers.onboarding.routes.RegistrationController.checkYourAnswers()

}
