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
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.RegistrationStatus.IndividualSupplyingInformation
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.IndividualName
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class RegistrationEnterIndividualNameController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  cc: MessagesControllerComponents,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  val enterNamePage: views.html.onboarding.name.enter_name
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging
    with IndividualNameController[IndividualSupplyingInformation] {

  override val isSubscribedJourney: Boolean = false

  override def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, IndividualSupplyingInformation)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, i: IndividualSupplyingInformation)) =>
        Right(sessionData -> i)
      case _                                                      => Left(Redirect(controllers.routes.StartController.start()))
    }

  override def updateName(
    journey: IndividualSupplyingInformation,
    name: IndividualName
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, IndividualSupplyingInformation] =
    EitherT.rightT[Future, Error](journey.copy(name = Some(name)))

  override def name(
    journey: IndividualSupplyingInformation
  ): Option[IndividualName] = journey.name

  override protected lazy val backLinkCall: Call        =
    controllers.onboarding.routes.RegistrationController.selectEntityType()
  override protected lazy val enterNameSubmitCall: Call =
    routes.RegistrationEnterIndividualNameController.enterIndividualNameSubmit()
  override protected lazy val continueCall: Call        =
    controllers.onboarding.address.routes.RegistrationEnterAddressController
      .isUk()

}
