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

import java.time.LocalDate

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, of, optional, text}
import play.api.data.validation.{Constraint, Invalid, Valid}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => addressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress, addressLineMapping}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.{CompleteExamplePropertyDetailsAnswers, IncompleteExamplePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{BooleanFormatter, Error, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.views.address.AddressJourneyType.Returns.FillingOutReturnAddressJourney
import uk.gov.hmrc.cgtpropertydisposalsfrontend.{controllers, views}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PropertyDetailsController @Inject() (
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
    with AddressController[FillingOutReturn] {

  import PropertyDetailsController._

  override val toAddressJourneyType: FillingOutReturn => FillingOutReturnAddressJourney =
    FillingOutReturnAddressJourney.apply

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, FillingOutReturn)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, r: FillingOutReturn)) => Right(sessionData -> r)
      case _                                        => Left(Redirect(controllers.routes.StartController.start()))
    }

  private def withAssetTypes(
    draftReturn: DraftReturn
  )(f: Either[List[AssetType], AssetType] => Future[Result]): Future[Result] =
    assetTypes(draftReturn).fold[Future[Result]](
      Redirect(controllers.returns.routes.TaskListController.taskList())
    )(f)

  private def assetTypes(draftReturn: DraftReturn): Option[Either[List[AssetType], AssetType]] =
    draftReturn.fold(
      _.triageAnswers.fold(_.assetTypes, c => Some(c.assetTypes)).map(Left(_)),
      _.triageAnswers.fold(_.assetType, c => Some(c.assetType)).map(Right(_))
    )

  private def hasNonResidentialProperty(assetTypes: Either[List[AssetType], AssetType]): Boolean =
    assetTypes.fold(_.contains(AssetType.NonResidential), _ === AssetType.NonResidential)

  def updateAddress(journey: FillingOutReturn, address: Address, isManuallyEnteredAddress: Boolean)(
    implicit hc: HeaderCarrier,
    request: Request[_]
  ): EitherT[Future, Error, FillingOutReturn] =
    address match {
      case _: NonUkAddress =>
        EitherT.leftT[Future, FillingOutReturn](Error("Got non uk address in returns journey but expected uk address"))
      case a: UkAddress =>
        journey.draftReturn match {
          case m: MultipleDisposalsDraftReturn =>
            val answers = m.examplePropertyDetailsAnswers.getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)
            if (answers.fold(_.address, c => Some(c.address)).contains(a))
              EitherT.pure(journey)
            else {
              val updatedDraftReturn = m.copy(
                examplePropertyDetailsAnswers = Some(
                  answers.fold(
                    _.copy(address = Some(a)),
                    _.copy(address = a)
                  )
                )
              )
              returnsService
                .storeDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )
                .map(_ => journey.copy(draftReturn = updatedDraftReturn))
            }

          case d: SingleDisposalDraftReturn =>
            if (d.propertyAddress.contains(a))
              EitherT.pure(journey)
            else {
              val updatedDraftReturn = d.copy(propertyAddress = Some(a))
              returnsService
                .storeDraftReturn(
                  updatedDraftReturn,
                  journey.subscribedDetails.cgtReference,
                  journey.agentReferenceNumber
                )
                .map(_ => journey.copy(draftReturn = updatedDraftReturn))
            }
        }

    }

  protected lazy val enterUkAddressCall: Call       = addressRoutes.PropertyDetailsController.enterUkAddress()
  protected lazy val enterUkAddressSubmitCall: Call = addressRoutes.PropertyDetailsController.enterUkAddressSubmit()

  protected lazy val enterPostcodeCall: Call       = addressRoutes.PropertyDetailsController.enterPostcode()
  protected lazy val enterPostcodeSubmitCall: Call = addressRoutes.PropertyDetailsController.enterPostcodeSubmit()
  protected lazy val selectAddressCall: Call       = addressRoutes.PropertyDetailsController.selectAddress()
  protected lazy val selectAddressSubmitCall: Call = addressRoutes.PropertyDetailsController.selectAddressSubmit()
  protected lazy val continueCall: Call            = addressRoutes.PropertyDetailsController.checkYourAnswers()

  override protected val enterPostcodePageBackLink: FillingOutReturn => Call = { fillingOutReturn =>
    val hasNonResidentialAssetTypes = assetTypes(fillingOutReturn.draftReturn).exists(hasNonResidentialProperty)
    if (hasNonResidentialAssetTypes)
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
    fillingOutReturn: FillingOutReturn,
    isSingleDisposal: Boolean
  ): Call = {
    val isComplete = fillingOutReturn.draftReturn
      .fold(_.examplePropertyDetailsAnswers.exists(_.fold(_ => false, _ => true)), _.propertyAddress.isDefined)
    if (isComplete) routes.PropertyDetailsController.checkYourAnswers()
    else if (isSingleDisposal) controllers.returns.routes.TaskListController.taskList()
    else routes.PropertyDetailsController.multipleDisposalsGuidance()
  }

  def singleDisposalHasUkPostcode(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    commonHasUkPostcodeBehaviour()
  }

  def multipleDisposalsHasUkPostcode(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      commonHasUkPostcodeBehaviour()
  }

  private def commonHasUkPostcodeBehaviour()(implicit request: RequestWithSessionData[_]): Future[Result] =
    withValidJourney(request) {
      case (_, fillingOutReturn) =>
        withAssetTypes(fillingOutReturn.draftReturn) { assetTypes =>
          if (hasNonResidentialProperty(assetTypes)) {
            val isSingleDisposal = fillingOutReturn.draftReturn.fold(_ => false, _ => true)
            Ok(
              hasValidPostcodePage(
                hasValidPostcodeForm,
                hasUkPostcodeBackLink(fillingOutReturn, isSingleDisposal),
                isSingleDisposal
              )
            )
          } else {
            Redirect(routes.PropertyDetailsController.checkYourAnswers())
          }
        }
    }

  def singleDisposalHasUkPostcodeSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      commonHasUkPostcodeSubmitBehaviour()
  }

  def multipleDisposalsHasUkPostcodeSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      commonHasUkPostcodeSubmitBehaviour()
  }

  private def commonHasUkPostcodeSubmitBehaviour()(implicit request: RequestWithSessionData[_]): Future[Result] =
    withValidJourney(request) {
      case (_, fillingOutReturn) =>
        withAssetTypes(fillingOutReturn.draftReturn) { assetTypes =>
          if (hasNonResidentialProperty(assetTypes)) {
            val isSingleDisposal = fillingOutReturn.draftReturn.fold(_ => false, _ => true)

            hasValidPostcodeForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    hasValidPostcodePage(
                      formWithErrors,
                      hasUkPostcodeBackLink(fillingOutReturn, isSingleDisposal),
                      isSingleDisposal
                    )
                  ), { hasValidPostcode =>
                  Redirect(
                    if (hasValidPostcode)
                      routes.PropertyDetailsController.enterPostcode()
                    else if (isSingleDisposal)
                      routes.PropertyDetailsController.singleDisposalEnterLandUprn()
                    else
                      routes.PropertyDetailsController.multipleDisposalsEnterLandUprn()
                  )
                }
              )
          } else {
            Redirect(routes.PropertyDetailsController.checkYourAnswers())
          }
        }
    }

  def singleDisposalEnterLandUprn(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    commonEnterLandUprnBehaviour()
  }

  def multipleDisposalsEnterLandUprn(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      commonEnterLandUprnBehaviour()
  }

  private def commonEnterLandUprnBehaviour()(implicit request: RequestWithSessionData[_]): Future[Result] =
    withValidJourney(request) {
      case (_, fillingOutReturn) =>
        withAssetTypes(fillingOutReturn.draftReturn) { assetTypes =>
          if (hasNonResidentialProperty(assetTypes)) {
            val isSingleDisposal = fillingOutReturn.draftReturn.fold(_ => false, _ => true)
            Ok(
              enterUPRNPage(
                enterUPRNForm,
                routes.PropertyDetailsController.singleDisposalHasUkPostcode(),
                isSingleDisposal
              )
            )
          } else {
            Redirect(routes.PropertyDetailsController.checkYourAnswers())
          }
        }
    }

  def singleDisposalEnterLandUprnSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      commonEnterLandUprnSubmitBehaviour()
  }

  def multipleDisposalsEnterLandUprnSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      commonEnterLandUprnSubmitBehaviour()
  }

  private def commonEnterLandUprnSubmitBehaviour()(implicit request: RequestWithSessionData[_]): Future[Result] =
    withValidJourney(request) {
      case (_, fillingOutReturn) =>
        withAssetTypes(fillingOutReturn.draftReturn) { assetTypes =>
          if (hasNonResidentialProperty(assetTypes)) {
            val isSingleDisposal = fillingOutReturn.draftReturn.fold(_ => false, _ => true)

            enterUPRNForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    enterUPRNPage(
                      formWithErrors,
                      routes.PropertyDetailsController.singleDisposalHasUkPostcode(),
                      isSingleDisposal
                    )
                  ),
                storeAddress(
                  routes.PropertyDetailsController.checkYourAnswers(),
                  fillingOutReturn,
                  isManuallyEnteredAddress = true
                )
              )
          } else {
            Redirect(routes.PropertyDetailsController.checkYourAnswers())
          }
        }
    }

  def multipleDisposalsGuidance(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, r) =>
        r.draftReturn match {
          case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case m: MultipleDisposalsDraftReturn =>
            val backLink =
              m.examplePropertyDetailsAnswers
                .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)
                .fold(
                  _ => controllers.returns.routes.TaskListController.taskList(),
                  _ => routes.PropertyDetailsController.checkYourAnswers()
                )
            Ok(multipleDisposalsGuidancePage(backLink))
        }
    }
  }

  def multipleDisposalsGuidanceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async {
    implicit request =>
      withValidJourney(request) {
        case (_, r) =>
          withAssetTypes(r.draftReturn) { assetTypes =>
            r.draftReturn match {
              case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
              case m: MultipleDisposalsDraftReturn =>
                val redirectTo =
                  m.examplePropertyDetailsAnswers
                    .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)
                    .fold(
                      _ =>
                        if (hasNonResidentialProperty(assetTypes))
                          routes.PropertyDetailsController.singleDisposalHasUkPostcode()
                        else routes.PropertyDetailsController.enterPostcode(),
                      _ => routes.PropertyDetailsController.checkYourAnswers()
                    )

                Redirect(redirectTo)
            }
          }
      }
  }

  def disposalDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, r) =>
        r.draftReturn match {
          case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case m: MultipleDisposalsDraftReturn =>
            m.triageAnswers.fold(_.taxYear, c => Some(c.taxYear)) match {
              case Some(taxYear) =>
                val answers = m.examplePropertyDetailsAnswers
                  .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)

                val disposalDate = answers.fold(_.disposalDate, c => Some(c.disposalDate))

                val completionDate = getCompletionDate(m.triageAnswers)

                val f    = getDisposalDateFrom(taxYear, completionDate)
                val form = disposalDate.fold(f)(c => f.fill(c.value))
                Ok(multipleDisposalsDisposalDatePage(form))

              case None => Redirect(controllers.returns.routes.TaskListController.taskList())
            }
        }
    }
  }

  def disposalDateSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, r) =>
        r.draftReturn match {
          case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case m: MultipleDisposalsDraftReturn =>
            m.triageAnswers.fold(_.taxYear, c => Some(c.taxYear)) match {
              case Some(taxYear) =>
                val answers = m.examplePropertyDetailsAnswers
                  .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)

                val completionDate = getCompletionDate(m.triageAnswers)

                getDisposalDateFrom(taxYear, completionDate)
                  .bindFromRequest()
                  .fold(
                    formWithErrors => {
                      val param1 = taxYear.startDateInclusive.getYear.toString
                      val param2 = taxYear.endDateExclusive.getYear.toString
                      val updatedFormWithErrors = formWithErrors.errors.map {
                        _.copy(args = Seq(param1, param2))
                      }

                      BadRequest(
                        multipleDisposalsDisposalDatePage(
                          formWithErrors.copy(errors = updatedFormWithErrors)
                        )
                      )
                    }, { date =>
                      val disposalDate = DisposalDate(date, taxYear)

                      if (answers
                            .fold(_.disposalDate, c => Some(c.disposalDate))
                            .contains(disposalDate)) {
                        Redirect(routes.PropertyDetailsController.checkYourAnswers())
                      } else {
                        val updatedAnswers =
                          answers
                            .fold(
                              _.copy(disposalDate = Some(disposalDate)),
                              _.copy(disposalDate = disposalDate)
                            )
                        val updatedDraftReturn = m.copy(examplePropertyDetailsAnswers = Some(updatedAnswers))
                        val result = for {
                          _ <- returnsService.storeDraftReturn(
                                updatedDraftReturn,
                                r.subscribedDetails.cgtReference,
                                r.agentReferenceNumber
                              )
                          _ <- EitherT(
                                updateSession(sessionStore, request)(
                                  _.copy(journeyStatus = Some(r.copy(draftReturn = updatedDraftReturn)))
                                )
                              )
                        } yield ()

                        result.fold(
                          { e =>
                            logger.warn("Could not update draft return", e)
                            errorHandler.errorResult()
                          },
                          _ => Redirect(routes.PropertyDetailsController.checkYourAnswers())
                        )

                      }

                    }
                  )

              case None => Redirect(controllers.returns.routes.TaskListController.taskList())
            }
        }
    }
  }

  def disposalPrice(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, r) =>
        r.draftReturn match {
          case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case m: MultipleDisposalsDraftReturn =>
            val answers = m.examplePropertyDetailsAnswers
              .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)

            val backLink = disposalPriceBackLink(answers)

            val disposalPrice = answers
              .fold(_.disposalPrice, c => Some(c.disposalPrice))

            val form = disposalPrice.fold(disposalPriceForm)(c => disposalPriceForm.fill(c.inPounds))

            Ok(multipleDisposalsDisposalPricePage(form, backLink))
        }
    }
  }

  def disposalPriceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, r) =>
        r.draftReturn match {
          case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case m: MultipleDisposalsDraftReturn =>
            val answers = m.examplePropertyDetailsAnswers
              .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)
            val backLink = disposalPriceBackLink(answers)

            disposalPriceForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    multipleDisposalsDisposalPricePage(formWithErrors, backLink)
                  ), { disposalPrice =>
                  if (answers
                        .fold(_.disposalPrice, c => Some(c.disposalPrice))
                        .contains(AmountInPence.fromPounds(disposalPrice))) {
                    Redirect(routes.PropertyDetailsController.checkYourAnswers())
                  } else {
                    val updatedAnswers =
                      answers
                        .fold(
                          _.copy(disposalPrice = Some(AmountInPence.fromPounds(disposalPrice))),
                          _.copy(disposalPrice = AmountInPence.fromPounds(disposalPrice))
                        )
                    val updatedDraftReturn = m.copy(examplePropertyDetailsAnswers = Some(updatedAnswers))
                    val result = for {
                      _ <- returnsService.storeDraftReturn(
                            updatedDraftReturn,
                            r.subscribedDetails.cgtReference,
                            r.agentReferenceNumber
                          )
                      _ <- EitherT(
                            updateSession(sessionStore, request)(
                              _.copy(journeyStatus = Some(r.copy(draftReturn = updatedDraftReturn)))
                            )
                          )
                    } yield ()

                    result.fold(
                      { e =>
                        logger.warn("Could not update draft return", e)
                        errorHandler.errorResult()
                      },
                      _ => Redirect(routes.PropertyDetailsController.checkYourAnswers())
                    )
                  }
                }
              )
        }
    }
  }

  def acquisitionPrice(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, r) =>
        r.draftReturn match {
          case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case m: MultipleDisposalsDraftReturn =>
            val answers = m.examplePropertyDetailsAnswers
              .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)

            val backLink = acquisitionPriceBackLink(answers)

            val acquisitionPrice = answers
              .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))

            val form = acquisitionPrice.fold(acquisitionPriceForm)(c => acquisitionPriceForm.fill(c.inPounds))

            Ok(multipleDisposalsAcquisitionPricePage(form, backLink))
        }
    }
  }

  def acquisitionPriceSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, r) =>
        r.draftReturn match {
          case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case m: MultipleDisposalsDraftReturn =>
            val answers = m.examplePropertyDetailsAnswers
              .getOrElse(IncompleteExamplePropertyDetailsAnswers.empty)
            val backLink = acquisitionPriceBackLink(answers)

            acquisitionPriceForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    multipleDisposalsAcquisitionPricePage(formWithErrors, backLink)
                  ), { acquisitionPrice =>
                  if (answers
                        .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))
                        .contains(AmountInPence.fromPounds(acquisitionPrice))) {
                    Redirect(routes.PropertyDetailsController.checkYourAnswers())
                  } else {
                    val updatedAnswers =
                      answers
                        .fold(
                          _.copy(acquisitionPrice = Some(AmountInPence.fromPounds(acquisitionPrice))),
                          _.copy(acquisitionPrice = AmountInPence.fromPounds(acquisitionPrice))
                        )
                    val updatedDraftReturn = m.copy(examplePropertyDetailsAnswers = Some(updatedAnswers))
                    val result = for {
                      _ <- returnsService.storeDraftReturn(
                            updatedDraftReturn,
                            r.subscribedDetails.cgtReference,
                            r.agentReferenceNumber
                          )
                      _ <- EitherT(
                            updateSession(sessionStore, request)(
                              _.copy(journeyStatus = Some(r.copy(draftReturn = updatedDraftReturn)))
                            )
                          )
                    } yield ()

                    result.fold(
                      { e =>
                        logger.warn("Could not update draft return", e)
                        errorHandler.errorResult()
                      },
                      _ => Redirect(routes.PropertyDetailsController.checkYourAnswers())
                    )
                  }
                }
              )
        }
    }
  }

  def checkYourAnswers(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, r) =>
        withAssetTypes(r.draftReturn) { assetTypes =>
          val hasNonResidentialAssetType = hasNonResidentialProperty(assetTypes)

          r.draftReturn match {
            case m: MultipleDisposalsDraftReturn =>
              m.examplePropertyDetailsAnswers.fold[Future[Result]](
                Redirect(routes.PropertyDetailsController.multipleDisposalsGuidance())
              ) {
                case IncompleteExamplePropertyDetailsAnswers(None, _, _, _) =>
                  Redirect(routes.PropertyDetailsController.multipleDisposalsGuidance())

                case IncompleteExamplePropertyDetailsAnswers(_, None, _, _) =>
                  Redirect(routes.PropertyDetailsController.disposalDate())

                case IncompleteExamplePropertyDetailsAnswers(_, _, None, _) =>
                  Redirect(routes.PropertyDetailsController.disposalPrice())

                case IncompleteExamplePropertyDetailsAnswers(_, _, _, None) =>
                  Redirect(routes.PropertyDetailsController.acquisitionPrice())

                case IncompleteExamplePropertyDetailsAnswers(Some(a), Some(dd), Some(dp), Some(ap)) =>
                  val completeAnswers    = CompleteExamplePropertyDetailsAnswers(a, dd, dp, ap)
                  val updatedDraftReturn = m.copy(examplePropertyDetailsAnswers = Some(completeAnswers))
                  val result = for {
                    _ <- returnsService.storeDraftReturn(
                          updatedDraftReturn,
                          r.subscribedDetails.cgtReference,
                          r.agentReferenceNumber
                        )
                    _ <- EitherT(
                          updateSession(sessionStore, request)(
                            _.copy(journeyStatus = Some(r.copy(draftReturn = updatedDraftReturn)))
                          )
                        )
                  } yield ()

                  result.fold(
                    { e =>
                      logger.warn("Could not update draft return", e)
                      errorHandler.errorResult()
                    },
                    _ => Ok(multipleDisposalsCheckYourAnswersPage(completeAnswers, hasNonResidentialAssetType))
                  )

                case c: CompleteExamplePropertyDetailsAnswers =>
                  Ok(multipleDisposalsCheckYourAnswersPage(c, hasNonResidentialAssetType))

              }

            case s: SingleDisposalDraftReturn =>
              s.propertyAddress.fold(
                Redirect(
                  if (hasNonResidentialProperty(assetTypes))
                    routes.PropertyDetailsController.singleDisposalHasUkPostcode()
                  else
                    routes.PropertyDetailsController.enterPostcode()
                )
              )(address => Ok(singleDisposalCheckYourAnswersPage(address, hasNonResidentialAssetType)))
          }
        }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, _) =>
        Redirect(returnsRoutes.TaskListController.taskList())
    }
  }

  private def getDisposalDateFrom(taxYear: TaxYear, completionDate: CompletionDate): Form[LocalDate] = {
    val startDateOfTaxYear = taxYear.startDateInclusive
    val endDateOfTaxYear   = taxYear.endDateExclusive

    val maximumDateInclusive =
      if (endDateOfTaxYear.isBefore(completionDate.value)) endDateOfTaxYear
      else completionDate.value

    disposalDateForm(maximumDateInclusive, startDateOfTaxYear)
  }

  private def disposalPriceBackLink(answers: ExamplePropertyDetailsAnswers): Call =
    answers
      .fold(
        _ => routes.PropertyDetailsController.disposalDate(),
        _ => routes.PropertyDetailsController.checkYourAnswers()
      )

  private def acquisitionPriceBackLink(answers: ExamplePropertyDetailsAnswers): Call =
    answers
      .fold(
        _ => routes.PropertyDetailsController.disposalPrice(),
        _ => routes.PropertyDetailsController.checkYourAnswers()
      )

  private def getCompletionDate(answers: MultipleDisposalsTriageAnswers): CompletionDate =
    answers
      .fold(_.completionDate, c => Some(c.completionDate))
      .getOrElse(CompletionDate(LocalDateUtils.today()))

  // the following aren't used for the returns journey - the returns journey only handles uk addresses
  protected lazy val isUkCall: Call                    = enterPostcodeCall
  protected lazy val isUkSubmitCall: Call              = enterPostcodeCall
  protected lazy val enterNonUkAddressCall: Call       = enterPostcodeCall
  protected lazy val enterNonUkAddressSubmitCall: Call = enterPostcodeCall
  protected lazy val backLinkCall: Call                = enterPostcodeCall

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
        "enterUPRN-line1" -> text.transform[String](_.trim, _.trim).verifying(uprnConstraint),
        "address-line2"   -> optional(addressLineMapping),
        "address-town"    -> optional(addressLineMapping),
        "address-county"  -> optional(addressLineMapping),
        "postcode"        -> Postcode.mapping
      )(UkAddress.apply)(UkAddress.unapply)
    )
  }

  def disposalDateForm(maximumDateInclusive: LocalDate, minimumDateInclusive: LocalDate): Form[LocalDate] = {
    val key = "multipleDisposalsDisposalDate"

    Form(
      mapping(
        "" -> of(
          LocalDateUtils.dateFormatter(
            Some(maximumDateInclusive),
            Some(minimumDateInclusive),
            s"$key-day",
            s"$key-month",
            s"$key-year",
            key
          )
        )
      )(identity)(Some(_))
    )
  }

  val disposalPriceForm: Form[BigDecimal] =
    Form(
      mapping(
        "multipleDisposalsDisposalPrice" -> of(
          MoneyUtils.amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

  val acquisitionPriceForm: Form[BigDecimal] =
    Form(
      mapping(
        "multipleDisposalsAcquisitionPrice" -> of(
          MoneyUtils.amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds)
        )
      )(identity)(Some(_))
    )

}
