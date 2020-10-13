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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.gainorlossafterreliefs

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.StartingToAmendToFillingOutReturnBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ConditionalRadioUtils.InnerOption
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{ConditionalRadioUtils, FormUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils.validateAmountOfMoney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DraftReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.{FurtherReturnCalculationEligibility, FurtherReturnCalculationEligibilityUtil, ReturnsService}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class GainOrLossAfterReliefsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  uuidGenerator: UUIDGenerator,
  cc: MessagesControllerComponents,
  gainOrLossAfterReliefsPage: views.html.returns.gainorlossafterreliefs.gain_or_loss_after_reliefs,
  val glarCalculatorEligibilityUtil: FurtherReturnCalculationEligibilityUtil,
  checkYourAnswersPage: views.html.returns.gainorlossafterreliefs.check_your_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with SessionUpdates
    with Logging
    with StartingToAmendToFillingOutReturnBehaviour {

  import GainOrLossAfterReliefsController._

  def enterGainOrLossAfterReliefs(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers { (fillingOutReturn, draftReturn, answer) =>
        glarCalculatorEligibilityUtil
          .isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn)
          .fold[Result](
            { e =>
              logger.warn("Could not check for calculation eligibility", e)
              errorHandler.errorResult()
            },
            furtherReturnEligibility =>
              Ok(
                gainOrLossAfterReliefsPage(
                  answer.fold(gainOrLossAfterReliefsForm)(value => gainOrLossAfterReliefsForm.fill(value.inPounds())),
                  answer.fold(controllers.returns.routes.TaskListController.taskList())(_ =>
                    routes.GainOrLossAfterReliefsController.checkYourAnswers()
                  ),
                  fillingOutReturn.subscribedDetails.isATrust,
                  draftReturn.representativeType(),
                  draftReturn.triageAnswers().isLeft,
                  fillingOutReturn.isAmendReturn,
                  furtherReturnEligibility match {
                    case FurtherReturnCalculationEligibility.Eligible(calculation, _, _) => Some(calculation)
                    case FurtherReturnCalculationEligibility.Ineligible(_)               => None
                  }
                )
              )
          )
      }
    }

  def enterGainOrLossAfterReliefsSubmit: Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers { case (fillingOutReturn, draftReturn, answer) =>
        gainOrLossAfterReliefsForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              glarCalculatorEligibilityUtil
                .isEligibleForFurtherReturnOrAmendCalculation(fillingOutReturn)
                .fold(
                  err => {
                    logger.warn("Could not check for calculation eligibility", err)
                    errorHandler.errorResult()
                  },
                  eligibility =>
                    BadRequest(
                      gainOrLossAfterReliefsPage(
                        formWithErrors,
                        answer.fold(controllers.returns.routes.TaskListController.taskList())(_ =>
                          routes.GainOrLossAfterReliefsController.checkYourAnswers()
                        ),
                        fillingOutReturn.subscribedDetails.isATrust,
                        draftReturn.representativeType(),
                        draftReturn.triageAnswers().isLeft,
                        fillingOutReturn.isAmendReturn,
                        eligibility match {
                          case FurtherReturnCalculationEligibility.Eligible(calculation, _, _) => Some(calculation)
                          case FurtherReturnCalculationEligibility.Ineligible(_)               => None
                        }
                      )
                    )
                ),
            value =>
              if (answer.map(_.inPounds()).contains(value))
                Redirect(
                  routes.GainOrLossAfterReliefsController.checkYourAnswers()
                )
              else {
                val updatedAmount      = AmountInPence.fromPounds(value)
                val updatedDraftReturn =
                  draftReturn.fold(
                    _.copy(
                      gainOrLossAfterReliefs = Some(updatedAmount),
                      exemptionAndLossesAnswers = None,
                      yearToDateLiabilityAnswers = None
                    ),
                    _.copy(
                      gainOrLossAfterReliefs = Some(updatedAmount),
                      exemptionAndLossesAnswers = None,
                      yearToDateLiabilityAnswers = None
                    ),
                    _.copy(
                      gainOrLossAfterReliefs = Some(updatedAmount),
                      exemptionAndLossesAnswers = None,
                      yearToDateLiabilityAnswers = None
                    ),
                    _.copy(
                      gainOrLossAfterReliefs = Some(updatedAmount),
                      exemptionAndLossesAnswers = None,
                      yearToDateLiabilityAnswers = None
                    ),
                    _.copy(
                      gainOrLossAfterReliefs = Some(updatedAmount),
                      exemptionAndLossesAnswers = None,
                      yearToDateLiabilityAnswers = None
                    )
                  )

                val updatedJourney = fillingOutReturn.copy(draftReturn = updatedDraftReturn)

                val result = for {
                  _ <- returnsService.storeDraftReturn(updatedJourney)
                  _ <- EitherT(
                         updateSession(sessionStore, request)(
                           _.copy(journeyStatus = Some(updatedJourney))
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
                      routes.GainOrLossAfterReliefsController.checkYourAnswers()
                    )
                )
              }
          )
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers { (journeyStatus, draftReturn, answers) =>
        answers match {
          case Some(completeInitialGainOrLossAnswers) =>
            Ok(
              checkYourAnswersPage(
                completeInitialGainOrLossAnswers,
                journeyStatus.subscribedDetails.isATrust,
                draftReturn.representativeType(),
                draftReturn.triageAnswers().isLeft
              )
            )

          case None =>
            Redirect(
              routes.GainOrLossAfterReliefsController.enterGainOrLossAfterReliefs()
            )
        }
      }
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndAnswers { (_, _, _) =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
      }
    }

  private def withFillingOutReturnAndAnswers(
    processReturnAndAnswersIntoResult: (
      FillingOutReturn,
      DraftReturn,
      Option[AmountInPence]
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(_.journeyStatus) match {
      case Some(s: StartingToAmendReturn) =>
        convertFromStartingAmendToFillingOutReturn(s, sessionStore, errorHandler, uuidGenerator)

      case Some(
            fillingOutReturn @ FillingOutReturn(
              _,
              _,
              _,
              d,
              _,
              _
            )
          ) if fillingOutReturn.isFurtherOrAmendReturn.contains(true) =>
        processReturnAndAnswersIntoResult(
          fillingOutReturn,
          d,
          d.gainOrLossAfterReliefs
        )

      case _ => Redirect(controllers.routes.StartController.start())
    }

}

object GainOrLossAfterReliefsController {

  val gainOrLossAfterReliefsForm: Form[BigDecimal] = {
    val (outerId, gainId, lossId) = ("gainOrLossAfterReliefs", "gainAfterReliefs", "lossAfterReliefs")

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
