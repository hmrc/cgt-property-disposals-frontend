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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.initialgainorloss

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.initialgainorloss.InitialGainOrLossController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.validateAmountOfMoney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{ConditionalRadioUtils, FormUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class InitialGainOrLossController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  cc: MessagesControllerComponents,
  initialGainOrLossesPage: views.html.returns.initialgainorloss.initial_gain_or_loss,
  checkYourAnswersPage: views.html.returns.initialgainorloss.check_your_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging {

  def enterInitialGainOrLoss: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers(request) { (journeyStatus, draftSingleDisposalReturn, answer) =>
        val isATrust           = journeyStatus.subscribedDetails.isATrust
        val representativeType =
          draftSingleDisposalReturn.triageAnswers.representativeType()
        Ok(
          initialGainOrLossesPage(
            answer.fold(initialGainOrLossForm)(value => initialGainOrLossForm.fill(value.inPounds())),
            getBackLink(answer),
            isATrust,
            representativeType
          )
        )
      }
    }

  def submitInitialGainOrLoss: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers(request) {
        case (fillingOutReturn, draftReturn, answers) =>
          val backLink           = getBackLink(answers)
          val isATrust           = fillingOutReturn.subscribedDetails.isATrust
          val representativeType =
            draftReturn.triageAnswers.representativeType()
          initialGainOrLossForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  initialGainOrLossesPage(
                    formWithErrors,
                    backLink,
                    isATrust,
                    representativeType
                  )
                ),
              value =>
                if (answers.map(_.inPounds()).contains(value))
                  Redirect(
                    routes.InitialGainOrLossController.checkYourAnswers()
                  )
                else {
                  val updatedDraftReturn =
                    draftReturn.copy(
                      initialGainOrLoss = Some(AmountInPence.fromPounds(value)),
                      reliefDetailsAnswers = draftReturn.reliefDetailsAnswers
                        .map(_.unsetPrrAndLettingRelief()),
                      yearToDateLiabilityAnswers = draftReturn.yearToDateLiabilityAnswers
                        .flatMap(_.unsetAllButIncomeDetails())
                    )

                  val result = for {
                    _ <- returnsService.storeDraftReturn(
                           updatedDraftReturn,
                           fillingOutReturn.subscribedDetails.cgtReference,
                           fillingOutReturn.agentReferenceNumber
                         )
                    _ <- EitherT(
                           updateSession(sessionStore, request)(
                             _.copy(journeyStatus =
                               Some(
                                 fillingOutReturn
                                   .copy(draftReturn = updatedDraftReturn)
                               )
                             )
                           )
                         )
                  } yield ()

                  result.fold(
                    { e =>
                      logger.warn("Could not update draft return", e)
                      errorHandler.errorResult()
                    },
                    _ =>
                      Redirect(
                        routes.InitialGainOrLossController.checkYourAnswers()
                      )
                  )
                }
            )
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers(request) { (journeyStatus, draftSingleDisposalReturn, answers) =>
        val isATrust           = journeyStatus.subscribedDetails.isATrust
        val representativeType =
          draftSingleDisposalReturn.triageAnswers.representativeType()
        answers match {
          case Some(completeInitialGainOrLossAnswers) =>
            Ok(
              checkYourAnswersPage(
                completeInitialGainOrLossAnswers,
                isATrust,
                representativeType
              )
            )

          case None                                   =>
            Redirect(
              routes.InitialGainOrLossController.enterInitialGainOrLoss()
            )
        }
      }
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers(request) { (_, _, _) =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
      }
    }

  private def getBackLink(initialGainOrLoss: Option[AmountInPence]): Call =
    initialGainOrLoss.fold(
      controllers.returns.routes.TaskListController.taskList()
    )(_ => routes.InitialGainOrLossController.checkYourAnswers())

  private def withFillingOutReturnAndAnswers(
    request: RequestWithSessionData[_]
  )(
    processReturnAndAnswersIntoResult: (
      FillingOutReturn,
      DraftSingleDisposalReturn,
      Option[AmountInPence]
    ) => Future[Result]
  ): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {

      case Some(
            fillingOutReturn @ FillingOutReturn(
              _,
              _,
              _,
              d: DraftSingleDisposalReturn,
              _
            )
          ) =>
        processReturnAndAnswersIntoResult(
          fillingOutReturn,
          d,
          d.initialGainOrLoss
        )

      case _ => Redirect(controllers.routes.StartController.start())
    }

}

object InitialGainOrLossController {

  val initialGainOrLossForm: Form[BigDecimal] = {
    val (outerId, gainId, lossId) = ("initialGainOrLoss", "gain", "loss")

    def innerOption(id: String): InnerOption[BigDecimal] =
      InnerOption { data =>
        FormUtils
          .readValue(id, data, identity)
          .flatMap(
            validateAmountOfMoney(
              id,
              _ <= 0,
              _ > MoneyUtils.maxAmountOfPounds
            )(_)
          )
          .leftMap {
            Seq(_)
          }
      }

    val formatter = ConditionalRadioUtils.formatter(outerId)(
      List(
        Left(innerOption(gainId)),
        Left(innerOption(lossId).map(_ * -1)),
        Right(BigDecimal(0))
      )
    ) { d =>
      if (d > 0)
        Map(
          outerId -> "0",
          gainId  -> MoneyUtils.formatAmountOfMoneyWithoutPoundSign(d)
        )
      else if (d < 0)
        Map(
          outerId   -> "1",
          lossId    -> MoneyUtils.formatAmountOfMoneyWithoutPoundSign((d * -1))
        )
      else
        Map(outerId -> "2")
    }

    Form[BigDecimal](
      mapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

}
