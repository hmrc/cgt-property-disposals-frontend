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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails

import cats.Eq
import cats.data.EitherT
import cats.instances.bigDecimal._
import cats.instances.future._
import cats.syntax.either._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.data.Forms.{of, mapping => formMapping}
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.http.Writeable
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.SessionUpdates
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.StartingToAmendToFillingOutReturnBehaviour
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.disposaldetails.DisposalDetailsController._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.AmountInPence._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.MoneyUtils
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.DisposalDetailsAnswers.{CompleteDisposalDetailsAnswers, IncompleteDisposalDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{ConditionalRadioUtils, FormUtils, NumberUtils, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class DisposalDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val errorHandler: ErrorHandler,
  returnsService: ReturnsService,
  uuidGenerator: UUIDGenerator,
  cc: MessagesControllerComponents,
  val config: Configuration,
  howMuchDidYouOwnPage: views.html.returns.disposaldetails.how_much_did_you_own,
  disposalPricePage: views.html.returns.disposaldetails.disposal_price,
  disposalFeesPage: views.html.returns.disposaldetails.disposal_fees,
  checkYouAnswers: views.html.returns.disposaldetails.check_your_answers
)(implicit viewConfig: ViewConfig, ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with StartingToAmendToFillingOutReturnBehaviour {

  type JourneyState =
    Either[DraftSingleIndirectDisposalReturn, DraftSingleDisposalReturn]

  private def withFillingOutReturnAndDisposalDetailsAnswers(
    f: (
      SessionData,
      FillingOutReturn,
      JourneyState,
      DisposalDetailsAnswers
    ) => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((_, s: StartingToAmendReturn)) =>
        convertFromStartingAmendToFillingOutReturn(s, sessionStore, errorHandler, uuidGenerator)

      case Some(
            (s, r @ FillingOutReturn(_, _, _, d: DraftSingleDisposalReturn, _, _, _))
          ) =>
        d.disposalDetailsAnswers
          .fold[Future[Result]](
            f(s, r, Right(d), IncompleteDisposalDetailsAnswers.empty)
          )(f(s, r, Right(d), _))

      case Some(
            (
              s,
              r @ FillingOutReturn(
                _,
                _,
                _,
                d: DraftSingleIndirectDisposalReturn,
                _,
                _,
                _
              )
            )
          ) =>
        val answers = IncompleteDisposalDetailsAnswers.empty.copy(
          shareOfProperty = Some(ShareOfProperty.Full)
        )
        d.disposalDetailsAnswers
          .fold[Future[Result]](
            f(s, r, Left(d), answers)
          )(f(s, r, Left(d), _))

      case _ => Redirect(controllers.routes.StartController.start())
    }

  private def withDisposalMethod(draftReturn: JourneyState)(
    f: DisposalMethod => Future[Result]
  ): Future[Result] =
    draftReturn
      .fold(_.triageAnswers, _.triageAnswers)
      .fold(_.disposalMethod, c => Some(c.disposalMethod)) match {
      case Some(method) => f(method)
      case _            => Redirect(controllers.routes.StartController.start())
    }

  private def withDisposalMethodAndShareOfProperty(
    draftReturn: JourneyState,
    answers: DisposalDetailsAnswers
  )(
    f: (DisposalMethod, ShareOfProperty) => Future[Result]
  ): Future[Result] =
    withDisposalMethod(draftReturn) { method =>
      answers.fold(_.shareOfProperty, c => Some(c.shareOfProperty)) match {
        case Some(share) => f(method, share)
        case _           => Redirect(routes.DisposalDetailsController.howMuchDidYouOwn())
      }
    }

  private def displayPage[A, P : Writeable, R](answers: DisposalDetailsAnswers)(
    form: DisposalDetailsAnswers => Form[A]
  )(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: DisposalDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  ): Future[Result] =
    if (requiredPreviousAnswer(answers).isDefined) {
      val backLink = answers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ =>
          controllers.returns.disposaldetails.routes.DisposalDetailsController
            .checkYourAnswers()
      )

      Ok(page(form(answers), backLink))
    } else
      Redirect(redirectToIfNoRequiredPreviousAnswer)

  private def submitBehaviour[A, P : Writeable, R](
    fillingOutReturn: FillingOutReturn,
    draftReturn: JourneyState,
    answers: DisposalDetailsAnswers
  )(
    form: Form[A]
  )(
    page: (Form[A], Call) => P
  )(
    requiredPreviousAnswer: DisposalDetailsAnswers => Option[R],
    redirectToIfNoRequiredPreviousAnswer: Call
  )(
    updateAnswers: (A, DisposalDetailsAnswers, JourneyState) => JourneyState
  )(implicit
    request: RequestWithSessionData[_]
  ): Future[Result] =
    if (requiredPreviousAnswer(answers).isDefined) {
      lazy val backLink = answers.fold(
        _ => redirectToIfNoRequiredPreviousAnswer,
        _ =>
          controllers.returns.disposaldetails.routes.DisposalDetailsController
            .checkYourAnswers()
      )
      form
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest(page(formWithErrors, backLink)),
          { value =>
            val newDraftReturn = updateAnswers(value, answers, draftReturn)
            val newJourney     =
              fillingOutReturn.copy(draftReturn = newDraftReturn.merge).withForceDisplayGainOrLossAfterReliefsForAmends

            val result = for {
              _ <- if (newDraftReturn.merge === draftReturn.merge)
                     EitherT.pure(())
                   else
                     returnsService.storeDraftReturn(newJourney)
              _ <- EitherT(
                     updateSession(sessionStore, request)(
                       _.copy(journeyStatus = Some(newJourney))
                     )
                   )
            } yield ()

            result.fold(
              { e =>
                logger.warn("Could not update draft return", e)
                errorHandler.errorResult()
              },
              _ => Redirect(routes.DisposalDetailsController.checkYourAnswers())
            )
          }
        )
    } else
      Redirect(redirectToIfNoRequiredPreviousAnswer)

  private def representativeType(state: JourneyState) =
    state.fold(
      _.triageAnswers.representativeType(),
      _.triageAnswers.representativeType()
    )

  private def isIndirectDisposal(state: JourneyState): Boolean =
    state match {
      case Right(_) => false
      case Left(_)  => true
    }

  def howMuchDidYouOwn(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndDisposalDetailsAnswers { case (_, fillingOutReturn, state, answers) =>
        displayPage(answers)(
          form = _.fold(
            _.shareOfProperty
              .fold(shareOfPropertyForm)(shareOfPropertyForm.fill),
            c => shareOfPropertyForm.fill(c.shareOfProperty)
          )
        )(
          page = howMuchDidYouOwnPage(
            _,
            _,
            fillingOutReturn.subscribedDetails.isATrust,
            representativeType(state),
            fillingOutReturn.isAmendReturn
          )
        )(
          requiredPreviousAnswer = _ => Some(()),
          redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
        )
      }
    }

  def howMuchDidYouOwnSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndDisposalDetailsAnswers { case (_, fillingOutReturn, state, answers) =>
        submitBehaviour(fillingOutReturn, state, answers)(
          form = shareOfPropertyForm
        )(
          page = howMuchDidYouOwnPage(
            _,
            _,
            fillingOutReturn.subscribedDetails.isATrust,
            representativeType(state),
            fillingOutReturn.isAmendReturn
          )
        )(
          requiredPreviousAnswer = _ => Some(()),
          redirectToIfNoRequiredPreviousAnswer = controllers.returns.routes.TaskListController.taskList()
        )(
          updateAnswers = { (percentage, answers, draftReturn) =>
            if (
              answers
                .fold(_.shareOfProperty, c => Some(c.shareOfProperty))
                .contains(percentage)
            )
              draftReturn
            else {
              val isFurtherOrAmendReturn = fillingOutReturn.isFurtherOrAmendReturn.contains(true)
              val newAnswers             = answers
                .unset(_.disposalPrice)
                .unset(_.disposalFees)
                .copy(shareOfProperty = Some(percentage))

              draftReturn.bimap(
                i =>
                  i.copy(
                    disposalDetailsAnswers = Some(newAnswers),
                    acquisitionDetailsAnswers = None,
                    yearToDateLiabilityAnswers = i.yearToDateLiabilityAnswers
                      .flatMap(_.unsetAllButIncomeDetails()),
                    gainOrLossAfterReliefs = None,
                    exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else i.exemptionAndLossesAnswers
                  ),
                s =>
                  s.copy(
                    disposalDetailsAnswers = Some(newAnswers),
                    acquisitionDetailsAnswers = None,
                    initialGainOrLoss = None,
                    reliefDetailsAnswers = s.reliefDetailsAnswers
                      .map(_.unsetPrrAndLettingRelief(s.triageAnswers.isPeriodOfAdmin)),
                    exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else s.exemptionAndLossesAnswers,
                    yearToDateLiabilityAnswers = s.yearToDateLiabilityAnswers
                      .flatMap(_.unsetAllButIncomeDetails()),
                    gainOrLossAfterReliefs = None
                  )
              )
            }
          }
        )
      }
    }

  private def disposalPriceBackLink(state: JourneyState): Call =
    if (isIndirectDisposal(state))
      controllers.returns.routes.TaskListController.taskList()
    else
      controllers.returns.disposaldetails.routes.DisposalDetailsController.howMuchDidYouOwn()

  def whatWasDisposalPrice(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndDisposalDetailsAnswers { case (_, fillingOutReturn, state, answers) =>
        withDisposalMethodAndShareOfProperty(state, answers) { case (disposalMethod, _) =>
          displayPage(answers)(
            form = _.fold(
              _.disposalPrice.fold(disposalPriceForm)(a => disposalPriceForm.fill(a.inPounds())),
              c => disposalPriceForm.fill(c.disposalPrice.inPounds())
            )
          )(
            page = disposalPricePage(
              _,
              _,
              disposalMethod,
              fillingOutReturn.subscribedDetails.isATrust,
              representativeType(state),
              isIndirectDisposal(state),
              fillingOutReturn.isAmendReturn
            )
          )(
            requiredPreviousAnswer = _.fold(_.shareOfProperty, c => Some(c.shareOfProperty)),
            redirectToIfNoRequiredPreviousAnswer = disposalPriceBackLink(state)
          )
        }
      }
    }

  def whatWasDisposalPriceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndDisposalDetailsAnswers { case (_, fillingOutReturn, state, answers) =>
        withDisposalMethodAndShareOfProperty(state, answers) { case (disposalMethod, _) =>
          submitBehaviour(fillingOutReturn, state, answers)(
            form = disposalPriceForm
          )(
            page = disposalPricePage(
              _,
              _,
              disposalMethod,
              fillingOutReturn.subscribedDetails.isATrust,
              representativeType(state),
              isIndirectDisposal(state),
              fillingOutReturn.isAmendReturn
            )
          )(
            requiredPreviousAnswer = _.fold(_.shareOfProperty, c => Some(c.shareOfProperty)),
            redirectToIfNoRequiredPreviousAnswer = disposalPriceBackLink(state)
          )(
            updateAnswers = { (price, answers, draftReturn) =>
              if (
                answers
                  .fold(_.disposalPrice, c => Some(c.disposalPrice))
                  .exists(_.inPounds() === price)
              )
                draftReturn
              else {
                val isFurtherOrAmendReturn = fillingOutReturn.isFurtherOrAmendReturn.contains(true)
                val newAnswers             = answers.fold(
                  _.copy(disposalPrice = Some(fromPounds(price))),
                  _.copy(disposalPrice = fromPounds(price))
                )

                draftReturn.bimap(
                  i =>
                    i.copy(
                      disposalDetailsAnswers = Some(newAnswers),
                      gainOrLossAfterReliefs = None,
                      exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else i.exemptionAndLossesAnswers,
                      yearToDateLiabilityAnswers = i.yearToDateLiabilityAnswers
                        .flatMap(_.unsetAllButIncomeDetails())
                    ),
                  s =>
                    s.copy(
                      disposalDetailsAnswers = Some(newAnswers),
                      initialGainOrLoss = None,
                      reliefDetailsAnswers = s.reliefDetailsAnswers
                        .map(_.unsetPrrAndLettingRelief(s.triageAnswers.isPeriodOfAdmin)),
                      gainOrLossAfterReliefs = None,
                      exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else s.exemptionAndLossesAnswers,
                      yearToDateLiabilityAnswers = s.yearToDateLiabilityAnswers.flatMap(
                        _.unsetAllButIncomeDetails()
                      )
                    )
                )
              }
            }
          )
        }
      }
    }

  def whatWereDisposalFees(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndDisposalDetailsAnswers { case (_, fillingOutReturn, state, answers) =>
        withDisposalMethodAndShareOfProperty(state, answers) { case (_, _) =>
          displayPage(answers)(
            form = _.fold(
              _.disposalFees.fold(disposalFeesForm)(a => disposalFeesForm.fill(a.inPounds())),
              c => disposalFeesForm.fill(c.disposalFees.inPounds())
            )
          )(
            page = disposalFeesPage(
              _,
              _,
              fillingOutReturn.subscribedDetails.isATrust,
              representativeType(state),
              isIndirectDisposal(state),
              fillingOutReturn.isAmendReturn
            )
          )(
            requiredPreviousAnswer = _.fold(_.disposalPrice, c => Some(c.disposalPrice)),
            redirectToIfNoRequiredPreviousAnswer = controllers.returns.disposaldetails.routes.DisposalDetailsController
              .whatWasDisposalPrice()
          )
        }
      }
    }

  def whatWereDisposalFeesSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndDisposalDetailsAnswers { case (_, fillingOutReturn, state, answers) =>
        withDisposalMethodAndShareOfProperty(state, answers) { case (_, _) =>
          submitBehaviour(fillingOutReturn, state, answers)(
            form = disposalFeesForm
          )(
            page = disposalFeesPage(
              _,
              _,
              fillingOutReturn.subscribedDetails.isATrust,
              representativeType(state),
              isIndirectDisposal(state),
              fillingOutReturn.isAmendReturn
            )
          )(
            requiredPreviousAnswer = _.fold(_.disposalPrice, c => Some(c.disposalPrice)),
            redirectToIfNoRequiredPreviousAnswer = controllers.returns.disposaldetails.routes.DisposalDetailsController
              .whatWasDisposalPrice()
          )(
            updateAnswers = { (price, answers, draftReturn) =>
              if (
                answers
                  .fold(_.disposalFees, c => Some(c.disposalFees))
                  .exists(_.inPounds() === price)
              )
                draftReturn
              else {
                val isFurtherOrAmendReturn = fillingOutReturn.isFurtherOrAmendReturn.contains(true)
                val newAnswers             =
                  answers.fold(
                    _.copy(disposalFees = Some(fromPounds(price))),
                    _.copy(disposalFees = fromPounds(price))
                  )

                draftReturn.bimap(
                  i =>
                    i.copy(
                      disposalDetailsAnswers = Some(newAnswers),
                      gainOrLossAfterReliefs = None,
                      exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else i.exemptionAndLossesAnswers,
                      yearToDateLiabilityAnswers = i.yearToDateLiabilityAnswers
                        .flatMap(_.unsetAllButIncomeDetails())
                    ),
                  s =>
                    s.copy(
                      disposalDetailsAnswers = Some(newAnswers),
                      initialGainOrLoss = None,
                      reliefDetailsAnswers = s.reliefDetailsAnswers
                        .map(_.unsetPrrAndLettingRelief(s.triageAnswers.isPeriodOfAdmin)),
                      gainOrLossAfterReliefs = None,
                      exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else s.exemptionAndLossesAnswers,
                      yearToDateLiabilityAnswers = s.yearToDateLiabilityAnswers.flatMap(
                        _.unsetAllButIncomeDetails()
                      )
                    )
                )
              }
            }
          )
        }
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndDisposalDetailsAnswers { case (_, fillingOutReturn, state, answers) =>
        withDisposalMethod(state) { case (disposalMethod) =>
          answers match {

            case IncompleteDisposalDetailsAnswers(None, _, _) =>
              Redirect(routes.DisposalDetailsController.howMuchDidYouOwn())

            case IncompleteDisposalDetailsAnswers(_, None, _) =>
              Redirect(
                routes.DisposalDetailsController.whatWasDisposalPrice()
              )

            case IncompleteDisposalDetailsAnswers(_, _, None) =>
              Redirect(
                routes.DisposalDetailsController.whatWereDisposalFees()
              )

            case IncompleteDisposalDetailsAnswers(
                  Some(share),
                  Some(price),
                  Some(fees)
                ) =>
              val completeAnswers    =
                CompleteDisposalDetailsAnswers(share, price, fees)
              val updatedDraftReturn =
                state.fold(
                  _.copy(disposalDetailsAnswers = Some(completeAnswers)),
                  _.copy(disposalDetailsAnswers = Some(completeAnswers))
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
                  logger.warn("Could not update session", e)
                  errorHandler.errorResult
                },
                _ =>
                  Ok(
                    checkYouAnswers(
                      completeAnswers,
                      disposalMethod,
                      fillingOutReturn.subscribedDetails.isATrust,
                      representativeType(state),
                      isIndirectDisposal(state)
                    )
                  )
              )

            case answers: CompleteDisposalDetailsAnswers =>
              Ok(
                checkYouAnswers(
                  answers,
                  disposalMethod,
                  fillingOutReturn.subscribedDetails.isATrust,
                  representativeType(state),
                  isIndirectDisposal(state)
                )
              )
          }

        }
      }
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withFillingOutReturnAndDisposalDetailsAnswers { case _ =>
        Redirect(controllers.returns.routes.TaskListController.taskList())
      }
    }

}

object DisposalDetailsController {

  val shareOfPropertyForm: Form[ShareOfProperty] = {

    val formatter: Formatter[ShareOfProperty] = {
      val (shareOfPropertyKey, percentageKey) =
        "shareOfProperty" -> "percentageShare"

      def validatePercentage(s: String): Either[FormError, ShareOfProperty] =
        Try(BigDecimal(s)).toEither
          .leftMap(_ => FormError(percentageKey, "error.invalid"))
          .flatMap { d =>
            if (d > 100) Left(FormError(percentageKey, "error.tooLarge"))
            else if (d < 0) Left(FormError(percentageKey, "error.tooSmall"))
            else if (NumberUtils.numberHasMoreThanNDecimalPlaces(d, 2))
              Left(FormError(percentageKey, "error.tooManyDecimals"))
            else if (d === BigDecimal(100)) Right(ShareOfProperty.Full)
            else if (d === BigDecimal(50)) Right(ShareOfProperty.Half)
            else Right(ShareOfProperty.Other(d))
          }

      ConditionalRadioUtils.formatter(shareOfPropertyKey)(
        List(
          Right(ShareOfProperty.Full),
          Right(ShareOfProperty.Half),
          Left(
            ConditionalRadioUtils.InnerOption(data =>
              FormUtils
                .readValue(percentageKey, data, identity)
                .flatMap(validatePercentage)
                .leftMap(Seq(_))
            )
          )
        )
      ) {
        case ShareOfProperty.Full                   => Map(shareOfPropertyKey -> "0")
        case ShareOfProperty.Half                   => Map(shareOfPropertyKey -> "1")
        case ShareOfProperty.Other(percentageValue) =>
          Map(
            shareOfPropertyKey -> "2",
            percentageKey      -> percentageValue.toString
          )
      }
    }

    Form(
      formMapping(
        "" -> of(formatter)
      )(identity)(Some(_))
    )
  }

  implicit val fillingOutReturnEq: Eq[FillingOutReturn] = Eq.fromUniversalEquals

  val disposalPriceForm: Form[BigDecimal] =
    Form(
      formMapping(
        "disposalPrice" -> of(
          MoneyUtils
            .amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

  val disposalFeesForm: Form[BigDecimal] =
    Form(
      formMapping(
        "disposalFees" -> of(
          MoneyUtils
            .amountInPoundsFormatter(_ < 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

}
