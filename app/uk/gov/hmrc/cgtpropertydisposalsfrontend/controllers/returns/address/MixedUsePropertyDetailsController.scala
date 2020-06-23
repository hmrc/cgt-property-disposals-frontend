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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address

import cats.data.EitherT
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => addressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MixedUsePropertyDetailsAnswers.{CompleteMixedUsePropertyDetailsAnswers, IncompleteMixedUsePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, JourneyStatus, SessionData}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.EnteringSingleMixedUsePropertyDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MixedUsePropertyDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val ukAddressLookupService: UKAddressLookupService,
  returnsService: ReturnsService,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val enterPostcodePage: views.html.address.enter_postcode,
  val selectAddressPage: views.html.address.select_address,
  val addressDisplay: views.html.components.address_display,
  val enterUkAddressPage: views.html.address.enter_uk_address,
  val enterNonUkAddressPage: views.html.address.enter_nonUk_address,
  val isUkPage: views.html.address.isUk,
  val exitPage: views.html.address.exit_page,
  singleMixedUseGuidance: views.html.returns.address.single_mixed_use_guidance,
  disposalValuePage: views.html.returns.address.single_mixed_use_disposal_price,
  acquisitionValuePage: views.html.returns.address.single_mixed_use_acquisition_price,
  singleMixedUseCheckYourAnswersPage: views.html.returns.address.single_mixed_use_check_your_answers
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with AddressController[EnteringSingleMixedUsePropertyDetails] {

  import MixedUsePropertyDetailsController._

  val toJourneyStatus: EnteringSingleMixedUsePropertyDetails => JourneyStatus = _.journey

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, EnteringSingleMixedUsePropertyDetails)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, f @ FillingOutReturn(_, _, _, draftReturn: DraftSingleMixedUseDisposalReturn))) =>
        val answers = draftReturn.mixedUsePropertyDetailsAnswers
          .getOrElse(IncompleteMixedUsePropertyDetailsAnswers.empty)
        Right(
          sessionData -> EnteringSingleMixedUsePropertyDetails(
            f,
            draftReturn.copy(mixedUsePropertyDetailsAnswers = Some(answers)),
            draftReturn.triageAnswers.representativeType(),
            f.subscribedDetails.isATrust,
            answers
          )
        )

      case _                                                                                                  => Left(Redirect(controllers.routes.StartController.start()))
    }

  def updateAddress(
    journey: EnteringSingleMixedUsePropertyDetails,
    address: Address,
    isManuallyEnteredAddress: Boolean
  )(implicit hc: HeaderCarrier, request: Request[_]): EitherT[Future, Error, JourneyStatus] =
    address match {
      case _: NonUkAddress =>
        EitherT.leftT[Future, JourneyStatus](
          Error("Got non uk address in returns journey but expected uk address")
        )
      case a: UkAddress    =>
        val answers            = journey.draftReturn.mixedUsePropertyDetailsAnswers.getOrElse(
          IncompleteMixedUsePropertyDetailsAnswers.empty
        )
        val updatedDraftReturn = journey.draftReturn
          .copy(mixedUsePropertyDetailsAnswers = Some(answers.unset(_.address).copy(address = Some(a))))

        returnsService
          .storeDraftReturn(
            updatedDraftReturn,
            journey.journey.subscribedDetails.cgtReference,
            journey.journey.agentReferenceNumber
          )
          .map(_ => journey.journey.copy(draftReturn = updatedDraftReturn))

    }

  def isATrust(journey: EnteringSingleMixedUsePropertyDetails): Boolean =
    journey.journey.subscribedDetails.isATrust

  protected lazy val isUkCall: Call                                                     = enterPostcodeCall
  protected lazy val isUkSubmitCall: Call                                               = enterPostcodeCall
  protected lazy val enterNonUkAddressCall: Call                                        = enterPostcodeCall
  protected lazy val enterNonUkAddressSubmitCall: Call                                  = enterPostcodeCall
  protected lazy val backLinkCall: EnteringSingleMixedUsePropertyDetails => Call        = _ => enterPostcodeCall
  override val enterPostcodePageBackLink: EnteringSingleMixedUsePropertyDetails => Call =
    _.answers.fold(
      _ => routes.MixedUsePropertyDetailsController.singleMixedUseGuidance(),
      _ => routes.MixedUsePropertyDetailsController.checkYourAnswers()
    )

  protected lazy val ukAddressNotAllowedExitPageCall: Option[Call] = None

  protected lazy val enterUkAddressCall: Call       = addressRoutes.MixedUsePropertyDetailsController.enterUkAddress()
  protected lazy val enterUkAddressSubmitCall: Call =
    addressRoutes.MixedUsePropertyDetailsController.enterUkAddressSubmit()
  protected lazy val enterPostcodeCall: Call        = addressRoutes.MixedUsePropertyDetailsController.enterPostcode()
  protected lazy val enterPostcodeSubmitCall: Call  =
    addressRoutes.MixedUsePropertyDetailsController.enterPostcodeSubmit()
  protected lazy val selectAddressCall: Call        = addressRoutes.MixedUsePropertyDetailsController.selectAddress()
  protected lazy val selectAddressSubmitCall: Call  =
    addressRoutes.MixedUsePropertyDetailsController.selectAddressSubmit()
  protected lazy val continueCall: Call             = addressRoutes.MixedUsePropertyDetailsController.checkYourAnswers()

  def singleMixedUseGuidance(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        val backLink =
          r.answers
            .fold(
              _ => controllers.returns.routes.TaskListController.taskList(),
              _ => routes.MixedUsePropertyDetailsController.checkYourAnswers()
            )
        Ok(
          singleMixedUseGuidance(
            backLink
          )
        )
      }
    }

  def singleMixedUseGuidanceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, _) =>
        Redirect(enterPostcodeCall)
      }
    }

  def enterDisposalValue(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        val answers       = r.answers
        val backLink      = answers.fold(
          _ => routes.MixedUsePropertyDetailsController.enterPostcode(),
          _ => routes.MixedUsePropertyDetailsController.checkYourAnswers()
        )
        val disposalPrice = answers.fold(_.disposalPrice, c => Some(c.disposalPrice))
        val form          = disposalPrice.fold(disposalPriceForm)(c => disposalPriceForm.fill(c.inPounds))
        Ok(
          disposalValuePage(
            form,
            backLink,
            r.journey.subscribedDetails.isATrust,
            r.representativeType
          )
        )
      }
    }

  def enterDisposalValueSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        val answers  = r.answers
        val backLink = answers.fold(
          _ => routes.MixedUsePropertyDetailsController.enterPostcode(),
          _ => routes.MixedUsePropertyDetailsController.checkYourAnswers()
        )
        disposalPriceForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                disposalValuePage(
                  formWithErrors,
                  backLink,
                  r.journey.subscribedDetails.isATrust,
                  r.representativeType
                )
              ),
            disposalPrice =>
              if (
                answers
                  .fold(_.disposalPrice, c => Some(c.disposalPrice))
                  .contains(AmountInPence.fromPounds(disposalPrice))
              )
                Redirect(
                  routes.MixedUsePropertyDetailsController.checkYourAnswers()
                )
              else {
                val updatedAnswers     =
                  answers
                    .fold(
                      _.copy(disposalPrice = Some(AmountInPence.fromPounds(disposalPrice))),
                      _.copy(disposalPrice = AmountInPence.fromPounds(disposalPrice))
                    )
                val updatedDraftReturn = r.draftReturn.copy(
                  mixedUsePropertyDetailsAnswers = Some(updatedAnswers),
                  yearToDateLiabilityAnswers = None
                )
                val result             = updateDraftReturnAndSession(r, updatedDraftReturn)

                result.fold(
                  { e =>
                    logger.warn("Could not update draft return", e)
                    errorHandler.errorResult()
                  },
                  _ =>
                    Redirect(
                      routes.MixedUsePropertyDetailsController.checkYourAnswers()
                    )
                )
              }
          )
      }
    }

  private def updateDraftReturnAndSession(
    r: EnteringSingleMixedUsePropertyDetails,
    updatedDraftReturn: DraftSingleMixedUseDisposalReturn
  )(implicit request: RequestWithSessionData[AnyContent]) =
    for {
      _ <- returnsService.storeDraftReturn(
             updatedDraftReturn,
             r.journey.subscribedDetails.cgtReference,
             r.journey.agentReferenceNumber
           )
      _ <- EitherT(
             updateSession(sessionStore, request)(
               _.copy(journeyStatus =
                 Some(
                   r.journey.copy(draftReturn = updatedDraftReturn)
                 )
               )
             )
           )
    } yield ()

  def enterAcquisitionValue(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        val answers = r.answers

        val backLink = answers.fold(
          _ => routes.MixedUsePropertyDetailsController.enterDisposalValue(),
          _ => routes.MixedUsePropertyDetailsController.checkYourAnswers()
        )

        val acquisitionPrice = answers
          .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))

        val form = acquisitionPrice.fold(acquisitionPriceForm)(c => acquisitionPriceForm.fill(c.inPounds))

        Ok(
          acquisitionValuePage(
            form,
            backLink,
            r.journey.subscribedDetails.isATrust,
            r.representativeType
          )
        )
      }
    }

  def enterAcquisitionValueSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        val answers  = r.answers
        val backLink = routes.MixedUsePropertyDetailsController.enterDisposalValue()

        acquisitionPriceForm
          .bindFromRequest()
          .fold(
            formWithErrors =>
              BadRequest(
                acquisitionValuePage(
                  formWithErrors,
                  backLink,
                  r.journey.subscribedDetails.isATrust,
                  r.representativeType
                )
              ),
            acquisitionPrice =>
              if (
                answers
                  .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))
                  .contains(AmountInPence.fromPounds(acquisitionPrice))
              )
                Redirect(
                  routes.MixedUsePropertyDetailsController.checkYourAnswers()
                )
              else {
                val updatedAnswers     =
                  answers
                    .fold(
                      _.copy(acquisitionPrice = Some(AmountInPence.fromPounds(acquisitionPrice))),
                      _.copy(acquisitionPrice = AmountInPence.fromPounds(acquisitionPrice))
                    )
                val updatedDraftReturn = r.draftReturn.copy(
                  mixedUsePropertyDetailsAnswers = Some(updatedAnswers),
                  yearToDateLiabilityAnswers = None
                )
                val result             = updateDraftReturnAndSession(r, updatedDraftReturn)

                result.fold(
                  { e =>
                    logger.warn("Could not update draft return", e)
                    errorHandler.errorResult()
                  },
                  _ =>
                    Redirect(
                      routes.MixedUsePropertyDetailsController.checkYourAnswers()
                    )
                )
              }
          )
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) {
        case (_, r) =>
          r.answers match {
            case IncompleteMixedUsePropertyDetailsAnswers(None, _, _)                  =>
              Redirect(routes.MixedUsePropertyDetailsController.singleMixedUseGuidance())
            case IncompleteMixedUsePropertyDetailsAnswers(_, None, _)                  =>
              Redirect(routes.MixedUsePropertyDetailsController.enterDisposalValue())
            case IncompleteMixedUsePropertyDetailsAnswers(_, _, None)                  =>
              Redirect(routes.MixedUsePropertyDetailsController.enterAcquisitionValue())
            case IncompleteMixedUsePropertyDetailsAnswers(Some(a), Some(dp), Some(ap)) =>
              val completeAnswers    = CompleteMixedUsePropertyDetailsAnswers(a, dp, ap)
              val updatedDraftReturn = r.draftReturn.copy(mixedUsePropertyDetailsAnswers = Some(completeAnswers))
              val result             = updateDraftReturnAndSession(r, updatedDraftReturn)

              result.fold(
                _ => errorHandler.errorResult(),
                _ =>
                  Ok(
                    singleMixedUseCheckYourAnswersPage(
                      completeAnswers
                    )
                  )
              )

            case c: CompleteMixedUsePropertyDetailsAnswers                             =>
              Ok(
                singleMixedUseCheckYourAnswersPage(c)
              )

          }

      }
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request)((_, _) => Redirect(returnsRoutes.TaskListController.taskList()))
    }
}

object MixedUsePropertyDetailsController {

  val disposalPriceForm: Form[BigDecimal] =
    Form(
      mapping(
        "singleMixedUseDisposalsDisposalPrice" -> of(
          MoneyUtils.amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

  val acquisitionPriceForm: Form[BigDecimal] =
    Form(
      mapping(
        "singleMixedUseDisposalsAcquisitionPrice" -> of(
          MoneyUtils.amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

}
