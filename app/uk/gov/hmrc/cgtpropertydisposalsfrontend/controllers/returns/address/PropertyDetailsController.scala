/*
 * Copyright 2022 HM Revenue & Customs
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

import java.time.LocalDate

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.order._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, of, optional, text}
import play.api.data.validation.{Constraint, Invalid, Valid}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => addressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{StartingToAmendToFillingOutReturnBehaviour, routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.{FillingOutReturn, StartingToAmendReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress, addressLineMapping}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.UUIDGenerator
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.{CompleteExamplePropertyDetailsAnswers, IncompleteExamplePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error, JourneyStatus, SessionData, TaxYear, TimeUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.FillingOutReturnAddressJourney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PropertyDetailsController @Inject() (
  val authenticatedAction: AuthenticatedAction,
  val sessionDataAction: SessionDataAction,
  val sessionStore: SessionStore,
  val ukAddressLookupService: UKAddressLookupService,
  returnsService: ReturnsService,
  uuidGenerator: UUIDGenerator,
  val errorHandler: ErrorHandler,
  cc: MessagesControllerComponents,
  val enterPostcodePage: views.html.address.enter_postcode,
  val selectAddressPage: views.html.address.select_address,
  val addressDisplay: views.html.components.address_display,
  val enterUkAddressPage: views.html.address.enter_uk_address,
  val enterNonUkAddressPage: views.html.address.enter_nonUk_address,
  val isUkPage: views.html.address.isUk,
  val exitPage: views.html.address.exit_page,
  multipleDisposalsGuidancePage: views.html.returns.address.multiple_disposals_guidance,
  multipleDisposalsDisposalDatePage: views.html.returns.address.disposal_date,
  multipleDisposalsDisposalPricePage: views.html.returns.address.disposal_price,
  multipleDisposalsAcquisitionPricePage: views.html.returns.address.acquisition_price,
  singleDisposalCheckYourAnswersPage: views.html.returns.address.single_disposal_check_your_answers,
  multipleDisposalsCheckYourAnswersPage: views.html.returns.address.multiple_disposals_check_your_answers,
  hasValidPostcodePage: views.html.returns.address.has_valid_postcode,
  enterUPRNPage: views.html.returns.address.enter_uprn
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with AddressController[FillingOutReturnAddressJourney]
    with StartingToAmendToFillingOutReturnBehaviour {

  import PropertyDetailsController._

  val toJourneyStatus: FillingOutReturnAddressJourney => JourneyStatus =
    _.journey

  def isATrust(journey: FillingOutReturnAddressJourney): Boolean =
    journey.journey.subscribedDetails.isATrust

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Future[Result], (SessionData, FillingOutReturnAddressJourney)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((_, s: StartingToAmendReturn)) =>
        implicit val r: RequestWithSessionData[_] = request
        Left(convertFromStartingAmendToFillingOutReturn(s, sessionStore, errorHandler, uuidGenerator))

      case Some((sessionData, r: FillingOutReturn)) =>
        val draftReturn = r.draftReturn.fold(
          m => Some(Left(m)),
          s => Some(Right(s)),
          _ => None,
          _ => None,
          _ => None
        )

        draftReturn
          .fold[Either[Future[Result], (SessionData, FillingOutReturnAddressJourney)]](
            Left(Redirect(controllers.routes.StartController.start()))
          ) { d =>
            Right(
              sessionData -> FillingOutReturnAddressJourney(
                r,
                d,
                d.fold(
                  e =>
                    e.triageAnswers
                      .fold(_.individualUserType, _.individualUserType),
                  e =>
                    e.triageAnswers
                      .fold(_.individualUserType, _.individualUserType)
                )
              )
            )
          }

      case _ => Left(Redirect(controllers.routes.StartController.start()))
    }

  private def withAssetTypes(
    journey: FillingOutReturnAddressJourney
  )(f: Either[List[AssetType], AssetType] => Future[Result]): Future[Result] =
    assetTypes(journey).fold[Future[Result]](
      Redirect(controllers.returns.routes.TaskListController.taskList())
    )(f)

  private def assetTypes(
    journey: FillingOutReturnAddressJourney
  ): Option[Either[List[AssetType], AssetType]] =
    journey.draftReturn.fold(
      _.triageAnswers.fold(_.assetTypes, c => Some(c.assetTypes)).map(Left(_)),
      _.triageAnswers.fold(_.assetType, c => Some(c.assetType)).map(Right(_))
    )

  private def shouldAskIfPostcodeExists(
    assetTypes: Either[List[AssetType], AssetType]
  ): Boolean =
    assetTypes.fold(
      _.forall(a => a === AssetType.NonResidential || a === AssetType.IndirectDisposal),
      _ === AssetType.NonResidential
    )

  override def updateAddress(
    journey: FillingOutReturnAddressJourney,
    address: Address,
    isManuallyEnteredAddress: Boolean
  )(implicit
    hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, JourneyStatus] =
    address match {
      case _: NonUkAddress =>
        EitherT.leftT[Future, JourneyStatus](
          Error("Got non uk address in returns journey but expected uk address")
        )
      case a: UkAddress    =>
        val isFurtherOrAmendReturn = journey.journey.isFurtherOrAmendReturn.contains(true)
        journey.draftReturn match {
          case Left(m: DraftMultipleDisposalsReturn) =>
            val answers = m.examplePropertyDetailsAnswers.getOrElse(
              IncompleteExamplePropertyDetailsAnswers.empty
            )
            if (answers.fold(_.address, c => Some(c.address)).contains(a))
              EitherT.pure(journey.journey)
            else {
              val updatedDraftReturn = m.copy(
                examplePropertyDetailsAnswers = Some(answers.unset(_.disposalDate).copy(address = Some(a))),
                exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else m.exemptionAndLossesAnswers,
                yearToDateLiabilityAnswers = if (isFurtherOrAmendReturn) None else m.yearToDateLiabilityAnswers
              )
              val updatedJourney     = journey.journey.copy(draftReturn = updatedDraftReturn)
              returnsService
                .storeDraftReturn(updatedJourney)
                .map(_ => updatedJourney)
            }

          case Right(d: DraftSingleDisposalReturn) =>
            if (d.propertyAddress.contains(a))
              EitherT.pure(journey.journey)
            else {
              val updatedDraftReturn = d.copy(
                propertyAddress = Some(a),
                exemptionAndLossesAnswers = if (isFurtherOrAmendReturn) None else d.exemptionAndLossesAnswers,
                yearToDateLiabilityAnswers = if (isFurtherOrAmendReturn) None else d.yearToDateLiabilityAnswers
              )
              val updatedJourney     = journey.journey.copy(draftReturn = updatedDraftReturn)
              returnsService
                .storeDraftReturn(updatedJourney)
                .map(_ => updatedJourney)
            }
        }

    }

  protected lazy val enterUkAddressCall: Call       =
    addressRoutes.PropertyDetailsController.enterUkAddress()
  protected lazy val enterUkAddressSubmitCall: Call =
    addressRoutes.PropertyDetailsController.enterUkAddressSubmit()

  protected lazy val enterPostcodeCall: Call       =
    addressRoutes.PropertyDetailsController.enterPostcode()
  protected lazy val enterPostcodeSubmitCall: Call =
    addressRoutes.PropertyDetailsController.enterPostcodeSubmit()
  protected lazy val selectAddressCall: Call       =
    addressRoutes.PropertyDetailsController.selectAddress()
  protected lazy val selectAddressSubmitCall: Call =
    addressRoutes.PropertyDetailsController.selectAddressSubmit()
  protected lazy val continueCall: Call            =
    addressRoutes.PropertyDetailsController.checkYourAnswers()

  override protected val enterPostcodePageBackLink: FillingOutReturnAddressJourney => Call = { fillingOutReturn =>
    if (assetTypes(fillingOutReturn).exists(shouldAskIfPostcodeExists))
      routes.PropertyDetailsController.singleDisposalHasUkPostcode()
    else
      fillingOutReturn.draftReturn.fold(
        _.examplePropertyDetailsAnswers
          .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)
          .fold(
            _ => routes.PropertyDetailsController.multipleDisposalsGuidance(),
            _ => routes.PropertyDetailsController.checkYourAnswers()
          ),
        _.propertyAddress.fold(
          returnsRoutes.TaskListController.taskList()
        )(_ => addressRoutes.PropertyDetailsController.checkYourAnswers())
      )
  }

  private def hasUkPostcodeBackLink(
    addressJourney: FillingOutReturnAddressJourney,
    isSingleDisposal: Boolean
  ): Call = {
    val isComplete = addressJourney.draftReturn
      .fold(
        _.examplePropertyDetailsAnswers.exists(_.fold(_ => false, _ => true)),
        _.propertyAddress.isDefined
      )
    if (isComplete) routes.PropertyDetailsController.checkYourAnswers()
    else if (isSingleDisposal)
      controllers.returns.routes.TaskListController.taskList()
    else routes.PropertyDetailsController.multipleDisposalsGuidance()
  }

  def singleDisposalHasUkPostcode(): Action[AnyContent] =
    authenticatedActionWithSessionData.async(implicit request => commonHasUkPostcodeBehaviour())

  def multipleDisposalsHasUkPostcode(): Action[AnyContent] =
    authenticatedActionWithSessionData.async(implicit request => commonHasUkPostcodeBehaviour())

  private def commonHasUkPostcodeBehaviour()(implicit
    request: RequestWithSessionData[_]
  ): Future[Result] =
    withValidJourney(request) { (_, fillingOutReturn) =>
      withAssetTypes(fillingOutReturn) { assetTypes =>
        if (shouldAskIfPostcodeExists(assetTypes)) {
          val isSingleDisposal = fillingOutReturn.draftReturn.fold(_ => false, _ => true)
          Ok(
            hasValidPostcodePage(
              hasValidPostcodeForm,
              hasUkPostcodeBackLink(fillingOutReturn, isSingleDisposal),
              isSingleDisposal,
              fillingOutReturn.journey.isAmendReturn
            )
          )
        } else
          Redirect(routes.PropertyDetailsController.checkYourAnswers())
      }
    }

  def singleDisposalHasUkPostcodeSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async(implicit request => commonHasUkPostcodeSubmitBehaviour())

  def multipleDisposalsHasUkPostcodeSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async(implicit request => commonHasUkPostcodeSubmitBehaviour())

  private def commonHasUkPostcodeSubmitBehaviour()(implicit
    request: RequestWithSessionData[_]
  ): Future[Result] =
    withValidJourney(request) { (_, fillingOutReturn) =>
      withAssetTypes(fillingOutReturn) { assetTypes =>
        if (shouldAskIfPostcodeExists(assetTypes)) {
          val isSingleDisposal = fillingOutReturn.draftReturn.fold(_ => false, _ => true)

          hasValidPostcodeForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  hasValidPostcodePage(
                    formWithErrors,
                    hasUkPostcodeBackLink(fillingOutReturn, isSingleDisposal),
                    isSingleDisposal,
                    fillingOutReturn.journey.isAmendReturn
                  )
                ),
              hasValidPostcode =>
                Redirect(
                  if (hasValidPostcode)
                    routes.PropertyDetailsController.enterPostcode()
                  else if (isSingleDisposal)
                    routes.PropertyDetailsController
                      .singleDisposalEnterLandUprn()
                  else
                    routes.PropertyDetailsController
                      .multipleDisposalsEnterLandUprn()
                )
            )
        } else
          Redirect(routes.PropertyDetailsController.checkYourAnswers())
      }
    }

  def singleDisposalEnterLandUprn(): Action[AnyContent] =
    authenticatedActionWithSessionData.async(implicit request => commonEnterLandUprnBehaviour())

  def multipleDisposalsEnterLandUprn(): Action[AnyContent] =
    authenticatedActionWithSessionData.async(implicit request => commonEnterLandUprnBehaviour())

  private def commonEnterLandUprnBehaviour()(implicit
    request: RequestWithSessionData[_]
  ): Future[Result] =
    withValidJourney(request) { (_, fillingOutReturn) =>
      withAssetTypes(fillingOutReturn) { assetTypes =>
        if (shouldAskIfPostcodeExists(assetTypes)) {
          val isSingleDisposal =
            fillingOutReturn.draftReturn.fold(_ => false, _ => true)
          Ok(
            enterUPRNPage(
              enterUPRNForm,
              routes.PropertyDetailsController.singleDisposalHasUkPostcode(),
              isSingleDisposal,
              fillingOutReturn.journey.isAmendReturn
            )
          )
        } else
          Redirect(routes.PropertyDetailsController.checkYourAnswers())
      }
    }

  def singleDisposalEnterLandUprnSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async(implicit request => commonEnterLandUprnSubmitBehaviour())

  def multipleDisposalsEnterLandUprnSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async(implicit request => commonEnterLandUprnSubmitBehaviour())

  private def commonEnterLandUprnSubmitBehaviour()(implicit
    request: RequestWithSessionData[_]
  ): Future[Result] =
    withValidJourney(request) { (_, fillingOutReturn) =>
      withAssetTypes(fillingOutReturn) { assetTypes =>
        if (shouldAskIfPostcodeExists(assetTypes)) {
          val isSingleDisposal =
            fillingOutReturn.draftReturn.fold(_ => false, _ => true)

          enterUPRNForm
            .bindFromRequest()
            .fold(
              formWithErrors =>
                BadRequest(
                  enterUPRNPage(
                    formWithErrors,
                    routes.PropertyDetailsController
                      .singleDisposalHasUkPostcode(),
                    isSingleDisposal,
                    fillingOutReturn.journey.isAmendReturn
                  )
                ),
              storeAddress(
                routes.PropertyDetailsController.checkYourAnswers(),
                fillingOutReturn,
                isManuallyEnteredAddress = true
              )
            )
        } else
          Redirect(routes.PropertyDetailsController.checkYourAnswers())
      }
    }

  def multipleDisposalsGuidance(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleDisposalReturn)   =>
            Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case Left(m: DraftMultipleDisposalsReturn) =>
            val backLink =
              m.examplePropertyDetailsAnswers
                .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)
                .fold(
                  _ => controllers.returns.routes.TaskListController.taskList(),
                  _ => routes.PropertyDetailsController.checkYourAnswers()
                )
            Ok(
              multipleDisposalsGuidancePage(
                backLink,
                r.journey.subscribedDetails.isATrust,
                extractIndividualUserType(r)
              )
            )
        }
      }
    }

  def multipleDisposalsGuidanceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        withAssetTypes(r) { assetTypes =>
          r.draftReturn match {
            case Right(_: DraftSingleDisposalReturn)   =>
              Redirect(routes.PropertyDetailsController.checkYourAnswers())
            case Left(m: DraftMultipleDisposalsReturn) =>
              val redirectTo =
                m.examplePropertyDetailsAnswers
                  .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)
                  .fold(
                    _ =>
                      if (shouldAskIfPostcodeExists(assetTypes))
                        routes.PropertyDetailsController
                          .singleDisposalHasUkPostcode()
                      else routes.PropertyDetailsController.enterPostcode(),
                    _ => routes.PropertyDetailsController.checkYourAnswers()
                  )

              Redirect(redirectTo)
          }
        }
      }
    }

  def disposalDate(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleDisposalReturn) =>
            Redirect(routes.PropertyDetailsController.checkYourAnswers())

          case Left(m: DraftMultipleDisposalsReturn) =>
            m.triageAnswers.fold(
              i => i.taxYear -> i.completionDate,
              c => Some(c.taxYear) -> Some(c.completionDate)
            ) match {
              case (Some(taxYear), Some(completionDate)) =>
                withPersonalRepresentativeDetails(m) { personalRepDetails =>
                  val answers = m.examplePropertyDetailsAnswers
                    .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)

                  val disposalDate =
                    answers.fold(_.disposalDate, c => Some(c.disposalDate))

                  val f    = getDisposalDateForm(taxYear, completionDate, personalRepDetails)
                  val form = disposalDate.fold(f)(c => f.fill(c.value))
                  Ok(
                    multipleDisposalsDisposalDatePage(
                      form,
                      r.journey.subscribedDetails.isATrust,
                      r.journey.isAmendReturn
                    )
                  )
                }

              case _ =>
                Redirect(
                  controllers.returns.routes.TaskListController.taskList()
                )
            }
        }
      }
    }

  def disposalDateSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleDisposalReturn)   =>
            Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case Left(m: DraftMultipleDisposalsReturn) =>
            m.triageAnswers.fold(
              i => i.taxYear -> i.completionDate,
              c => Some(c.taxYear) -> Some(c.completionDate)
            ) match {
              case (Some(taxYear), Some(completionDate)) =>
                withPersonalRepresentativeDetails(m) { personalRepDetails =>
                  val answers = m.examplePropertyDetailsAnswers
                    .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)

                  getDisposalDateForm(taxYear, completionDate, personalRepDetails)
                    .bindFromRequest()
                    .fold(
                      formWithErrors => {
                        val param1                = taxYear.startDateInclusive.getYear.toString
                        val param2                = taxYear.endDateExclusive.getYear.toString
                        val updatedFormWithErrors = formWithErrors.errors.map {
                          _.copy(args = Seq(param1, param2))
                        }

                        BadRequest(
                          multipleDisposalsDisposalDatePage(
                            formWithErrors.copy(errors = updatedFormWithErrors),
                            r.journey.subscribedDetails.isATrust,
                            r.journey.isAmendReturn
                          )
                        )
                      },
                      { date =>
                        val disposalDate = DisposalDate(date, taxYear)

                        if (
                          answers
                            .fold(_.disposalDate, c => Some(c.disposalDate))
                            .contains(disposalDate)
                        )
                          Redirect(
                            routes.PropertyDetailsController.checkYourAnswers()
                          )
                        else {
                          val isFurtherOrAmendReturn = r.journey.isFurtherOrAmendReturn.contains(true)
                          val updatedAnswers         =
                            answers
                              .fold(
                                _.copy(disposalDate = Some(disposalDate)),
                                _.copy(disposalDate = disposalDate)
                              )
                          val updatedDraftReturn     =
                            m.copy(
                              examplePropertyDetailsAnswers = Some(updatedAnswers),
                              gainOrLossAfterReliefs = if (isFurtherOrAmendReturn) None else m.gainOrLossAfterReliefs,
                              exemptionAndLossesAnswers =
                                if (isFurtherOrAmendReturn) None else m.exemptionAndLossesAnswers,
                              yearToDateLiabilityAnswers =
                                if (isFurtherOrAmendReturn) None else m.yearToDateLiabilityAnswers
                            )

                          val updatedJourney = r.journey
                            .copy(draftReturn = updatedDraftReturn)
                            .withForceDisplayGainOrLossAfterReliefsForAmends

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
                                routes.PropertyDetailsController
                                  .checkYourAnswers()
                              )
                          )

                        }

                      }
                    )
                }

              case _ =>
                Redirect(
                  controllers.returns.routes.TaskListController.taskList()
                )
            }
        }
      }
    }

  def disposalPrice(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleDisposalReturn)   =>
            Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case Left(m: DraftMultipleDisposalsReturn) =>
            val answers = m.examplePropertyDetailsAnswers
              .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)

            val backLink = disposalPriceBackLink(answers)

            val disposalPrice = answers
              .fold(_.disposalPrice, c => Some(c.disposalPrice))

            val form = disposalPrice.fold(disposalPriceForm)(c => disposalPriceForm.fill(c.inPounds))

            Ok(
              multipleDisposalsDisposalPricePage(
                form,
                backLink,
                r.journey.subscribedDetails.isATrust,
                extractIndividualUserType(r),
                r.journey.isAmendReturn
              )
            )
        }
      }
    }

  def disposalPriceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleDisposalReturn)   =>
            Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case Left(m: DraftMultipleDisposalsReturn) =>
            val answers  = m.examplePropertyDetailsAnswers
              .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)
            val backLink = disposalPriceBackLink(answers)

            disposalPriceForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    multipleDisposalsDisposalPricePage(
                      formWithErrors,
                      backLink,
                      r.journey.subscribedDetails.isATrust,
                      extractIndividualUserType(r),
                      r.journey.isAmendReturn
                    )
                  ),
                disposalPrice =>
                  if (
                    answers
                      .fold(_.disposalPrice, c => Some(c.disposalPrice))
                      .contains(AmountInPence.fromPounds(disposalPrice))
                  )
                    Redirect(
                      routes.PropertyDetailsController.checkYourAnswers()
                    )
                  else {
                    val updatedAnswers     =
                      answers
                        .fold(
                          _.copy(disposalPrice = Some(AmountInPence.fromPounds(disposalPrice))),
                          _.copy(disposalPrice = AmountInPence.fromPounds(disposalPrice))
                        )
                    val updatedDraftReturn = m.copy(
                      examplePropertyDetailsAnswers = Some(updatedAnswers),
                      yearToDateLiabilityAnswers = None,
                      gainOrLossAfterReliefs = None
                    )
                    val updatedJourney     =
                      r.journey.copy(draftReturn = updatedDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends
                    val result             = for {
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
                          routes.PropertyDetailsController.checkYourAnswers()
                        )
                    )
                  }
              )
        }
      }
    }

  def acquisitionPrice(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleDisposalReturn)   =>
            Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case Left(m: DraftMultipleDisposalsReturn) =>
            val answers = m.examplePropertyDetailsAnswers
              .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)

            val backLink = acquisitionPriceBackLink(answers)

            val acquisitionPrice = answers
              .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))

            val form = acquisitionPrice.fold(acquisitionPriceForm)(c => acquisitionPriceForm.fill(c.inPounds))

            Ok(
              multipleDisposalsAcquisitionPricePage(
                form,
                backLink,
                r.journey.subscribedDetails.isATrust,
                extractIndividualUserType(r),
                extractDateOfDeath(r),
                r.journey.isAmendReturn
              )
            )
        }
      }
    }

  def acquisitionPriceSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        r.draftReturn match {
          case Right(_: DraftSingleDisposalReturn)   =>
            Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case Left(m: DraftMultipleDisposalsReturn) =>
            val answers  = m.examplePropertyDetailsAnswers
              .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)
            val backLink = acquisitionPriceBackLink(answers)

            acquisitionPriceForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    multipleDisposalsAcquisitionPricePage(
                      formWithErrors,
                      backLink,
                      r.journey.subscribedDetails.isATrust,
                      extractIndividualUserType(r),
                      extractDateOfDeath(r),
                      r.journey.isAmendReturn
                    )
                  ),
                acquisitionPrice =>
                  if (
                    answers
                      .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))
                      .contains(AmountInPence.fromPounds(acquisitionPrice))
                  )
                    Redirect(
                      routes.PropertyDetailsController.checkYourAnswers()
                    )
                  else {
                    val updatedAnswers     =
                      answers
                        .fold(
                          _.copy(acquisitionPrice = Some(AmountInPence.fromPounds(acquisitionPrice))),
                          _.copy(acquisitionPrice = AmountInPence.fromPounds(acquisitionPrice))
                        )
                    val updatedDraftReturn = m.copy(
                      examplePropertyDetailsAnswers = Some(updatedAnswers),
                      yearToDateLiabilityAnswers = None,
                      gainOrLossAfterReliefs = None
                    )
                    val updatedJourney     =
                      r.journey.copy(draftReturn = updatedDraftReturn).withForceDisplayGainOrLossAfterReliefsForAmends
                    val result             = for {
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
                          routes.PropertyDetailsController.checkYourAnswers()
                        )
                    )
                  }
              )
        }
      }
    }

  def checkYourAnswers(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request) { (_, r) =>
        withAssetTypes(r) { assetTypes =>
          r.draftReturn match {
            case Left(m: DraftMultipleDisposalsReturn) =>
              m.examplePropertyDetailsAnswers.fold[Future[Result]](
                Redirect(
                  routes.PropertyDetailsController.multipleDisposalsGuidance()
                )
              ) {
                case IncompleteExamplePropertyDetailsAnswers(None, _, _, _) =>
                  Redirect(
                    routes.PropertyDetailsController.multipleDisposalsGuidance()
                  )

                case IncompleteExamplePropertyDetailsAnswers(_, None, _, _) =>
                  Redirect(routes.PropertyDetailsController.disposalDate())

                case IncompleteExamplePropertyDetailsAnswers(_, _, None, _) =>
                  Redirect(routes.PropertyDetailsController.disposalPrice())

                case IncompleteExamplePropertyDetailsAnswers(_, _, _, None) =>
                  Redirect(routes.PropertyDetailsController.acquisitionPrice())

                case IncompleteExamplePropertyDetailsAnswers(
                      Some(a),
                      Some(dd),
                      Some(dp),
                      Some(ap)
                    ) =>
                  val completeAnswers    =
                    CompleteExamplePropertyDetailsAnswers(a, dd, dp, ap)
                  val updatedDraftReturn = m
                    .copy(examplePropertyDetailsAnswers = Some(completeAnswers))
                  val updatedJourney     = r.journey.copy(draftReturn = updatedDraftReturn)
                  val result             = for {
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
                      Ok(
                        multipleDisposalsCheckYourAnswersPage(
                          completeAnswers,
                          shouldAskIfPostcodeExists(assetTypes),
                          r.journey.subscribedDetails.isATrust,
                          extractIndividualUserType(r),
                          extractDateOfDeath(r)
                        )
                      )
                  )

                case c: CompleteExamplePropertyDetailsAnswers =>
                  Ok(
                    multipleDisposalsCheckYourAnswersPage(
                      c,
                      shouldAskIfPostcodeExists(assetTypes),
                      r.journey.subscribedDetails.isATrust,
                      extractIndividualUserType(r),
                      extractDateOfDeath(r)
                    )
                  )

              }

            case Right(s: DraftSingleDisposalReturn) =>
              s.propertyAddress.fold(
                Redirect(
                  if (shouldAskIfPostcodeExists(assetTypes))
                    routes.PropertyDetailsController
                      .singleDisposalHasUkPostcode()
                  else
                    routes.PropertyDetailsController.enterPostcode()
                )
              )(address =>
                Ok(
                  singleDisposalCheckYourAnswersPage(
                    address,
                    shouldAskIfPostcodeExists(assetTypes)
                  )
                )
              )
          }
        }
      }
    }

  def checkYourAnswersSubmit(): Action[AnyContent] =
    authenticatedActionWithSessionData.async { implicit request =>
      withValidJourney(request)((_, _) => Redirect(returnsRoutes.TaskListController.taskList()))
    }

  private def getDisposalDateForm(
    taxYear: TaxYear,
    completionDate: CompletionDate,
    personalRepresentativeDetails: Option[PersonalRepresentativeDetails]
  ): Form[LocalDate] = {
    val startDateOfTaxYear = taxYear.startDateInclusive
    val endDateOfTaxYear   = taxYear.endDateExclusive

    val maximumDateInclusive =
      if (endDateOfTaxYear.isBefore(completionDate.value)) endDateOfTaxYear
      else completionDate.value

    disposalDateForm(
      maximumDateInclusive,
      startDateOfTaxYear,
      personalRepresentativeDetails
    )
  }

  private def extractIndividualUserType(
    f: FillingOutReturnAddressJourney
  ): Option[RepresentativeType] =
    f.individualUserType match {
      case Some(r: RepresentativeType) => Some(r)
      case _                           => None
    }

  private def extractDateOfDeath(
    f: FillingOutReturnAddressJourney
  ): Option[DateOfDeath] =
    f.draftReturn
      .fold(
        _.representeeAnswers.map(e => e.fold(_.dateOfDeath, _.dateOfDeath)),
        _.representeeAnswers.map(e => e.fold(_.dateOfDeath, _.dateOfDeath))
      )
      .flatten

  private def disposalPriceBackLink(
    answers: ExamplePropertyDetailsAnswers
  ): Call =
    answers
      .fold(
        _ => routes.PropertyDetailsController.disposalDate(),
        _ => routes.PropertyDetailsController.checkYourAnswers()
      )

  private def acquisitionPriceBackLink(
    answers: ExamplePropertyDetailsAnswers
  ): Call =
    answers
      .fold(
        _ => routes.PropertyDetailsController.disposalPrice(),
        _ => routes.PropertyDetailsController.checkYourAnswers()
      )

  private def withPersonalRepresentativeDetails(draftReturn: DraftMultipleDisposalsReturn)(
    f: Option[PersonalRepresentativeDetails] => Future[Result]
  )(implicit request: RequestWithSessionData[_]): Future[Result] =
    PersonalRepresentativeDetails
      .fromDraftReturn(draftReturn)
      .fold(
        { e =>
          logger.warn(s"Could not get personal representative details: $e")
          errorHandler.errorResult()
        },
        f
      )

  // the following aren't used for the returns journey - the returns journey only handles uk addresses
  protected lazy val isUkCall: Call                                       = enterPostcodeCall
  protected lazy val isUkSubmitCall: Call                                 = enterPostcodeCall
  protected lazy val enterNonUkAddressCall: Call                          = enterPostcodeCall
  protected lazy val enterNonUkAddressSubmitCall: Call                    = enterPostcodeCall
  protected lazy val backLinkCall: FillingOutReturnAddressJourney => Call = _ => enterPostcodeCall
  protected lazy val ukAddressNotAllowedExitPageCall: Option[Call]        = None
}

object PropertyDetailsController {

  val hasValidPostcodeForm: Form[Boolean] =
    Form(
      mapping(
        "hasValidPostcode" -> of(BooleanFormatter.formatter)
      )(identity)(Some(_))
    )

  val enterUPRNForm: Form[UkAddress] = {
    val uprnConstraint: Constraint[String] = Constraint(s =>
      if (s.isEmpty) Invalid("error.required")
      else if (s.exists(!_.isDigit)) Invalid("error.invalid")
      else if (s.length > 12) Invalid("error.tooLong")
      else Valid
    )

    Form(
      mapping(
        "enterUPRN-line1" -> text
          .transform[String](_.trim, _.trim)
          .verifying(uprnConstraint),
        "address-line2"   -> optional(addressLineMapping),
        "address-town"    -> optional(addressLineMapping),
        "address-county"  -> optional(addressLineMapping),
        "postcode"        -> Postcode.mapping
      )(UkAddress.apply)(UkAddress.unapply)
    )
  }

  def disposalDateForm(
    maximumDateInclusive: LocalDate,
    minimumDateInclusive: LocalDate,
    personalRepresentativeDetails: Option[PersonalRepresentativeDetails]
  ): Form[LocalDate] = {
    val key = "multipleDisposalsDisposalDate"
    Form(
      mapping(
        "" -> of(
          TimeUtils.dateFormatter(
            Some(maximumDateInclusive),
            Some(minimumDateInclusive),
            s"$key-day",
            s"$key-month",
            s"$key-year",
            key,
            None,
            Some(false),
            false,
            List(
              TimeUtils.personalRepresentativeDateValidation(
                personalRepresentativeDetails,
                key
              )
            )
          )
        )
      )(identity)(Some(_))
    )
  }

  val disposalPriceForm: Form[BigDecimal] =
    Form(
      mapping(
        "multipleDisposalsDisposalPrice" -> of(
          MoneyUtils
            .amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

  val acquisitionPriceForm: Form[BigDecimal] =
    Form(
      mapping(
        "multipleDisposalsAcquisitionPrice" -> of(
          MoneyUtils
            .amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

}
