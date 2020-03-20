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
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.mvc._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.{ErrorHandler, ViewConfig}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions.{AuthenticatedAction, RequestWithSessionData, SessionDataAction, WithAuthAndSessionDataAction}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.{routes => addressRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.address.PropertyDetailsController.{disposalDateForm, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.returns.{routes => returnsRoutes}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.{AddressController, SessionUpdates}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{NonUkAddress, UkAddress}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance.{AmountInPence, MoneyUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsExamplePropertyDetailsAnswers.{CompleteMultipleDisposalsExamplePropertyDetailsAnswers, IncompleteMultipleDisposalsExamplePropertyDetailsAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalDate, MultipleDisposalsDraftReturn, MultipleDisposalsExamplePropertyDetailsAnswers, SingleDisposalDraftReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, LocalDateUtils, SessionData, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.UKAddressLookupService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.{Logging, toFuture}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.Logging._
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
  multipleDisposalsCheckYourAnswersPage: views.html.returns.address.multiple_disposals_check_your_answers
)(implicit val viewConfig: ViewConfig, val ec: ExecutionContext)
    extends FrontendController(cc)
    with WithAuthAndSessionDataAction
    with Logging
    with SessionUpdates
    with AddressController[FillingOutReturn] {

  override val toAddressJourneyType: FillingOutReturn => FillingOutReturnAddressJourney =
    FillingOutReturnAddressJourney.apply

  def validJourney(
    request: RequestWithSessionData[_]
  ): Either[Result, (SessionData, FillingOutReturn)] =
    request.sessionData.flatMap(s => s.journeyStatus.map(s -> _)) match {
      case Some((sessionData, r: FillingOutReturn)) => Right(sessionData -> r)
      case _                                        => Left(Redirect(controllers.routes.StartController.start()))
    }

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
            if (m.examplePropertyDetailsAnswers.flatMap(_.fold(_.address, c => Some(c.address))).contains(a))
              EitherT.pure(journey)
            else {
              val updatedDraftReturn = m.copy(
                examplePropertyDetailsAnswers = Some(
                  IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(
                    Some(a),
                    None,
                    None,
                    None
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

  override protected val enterPostcodePageBackLink: FillingOutReturn => Call =
    _.draftReturn.fold(
      _.examplePropertyDetailsAnswers.fold(
        routes.PropertyDetailsController.multipleDisposalsGuidance()
      )(
        _.fold(
          _ => routes.PropertyDetailsController.multipleDisposalsGuidance(),
          _ => routes.PropertyDetailsController.checkYourAnswers()
        )
      ),
      _.propertyAddress.fold(
        returnsRoutes.TaskListController.taskList()
      )(_ => addressRoutes.PropertyDetailsController.checkYourAnswers())
    )

  def multipleDisposalsGuidance(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, r) =>
        r.draftReturn match {
          case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case m: MultipleDisposalsDraftReturn =>
            val backLink =
              m.examplePropertyDetailsAnswers
                .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
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
          r.draftReturn match {
            case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
            case m: MultipleDisposalsDraftReturn =>
              val redirectTo =
                m.examplePropertyDetailsAnswers
                  .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
                  .fold(
                    _ => routes.PropertyDetailsController.enterPostcode(),
                    _ => routes.PropertyDetailsController.checkYourAnswers()
                  )

              Redirect(redirectTo)
          }
      }
  }

  def disposalDate(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, r) =>
        r.draftReturn match {
          case _: SingleDisposalDraftReturn => Redirect(routes.PropertyDetailsController.checkYourAnswers())
          case m: MultipleDisposalsDraftReturn =>
            val answers  = m.examplePropertyDetailsAnswers
            val backLink = disposalDateBackLink(answers)
            val disposalDate = answers
              .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
              .fold(_.disposalDate, c => Some(c.disposalDate))

            m.triageAnswers.fold(_.taxYear, c => Some(c.taxYear)) match {
              case Some(taxYear) =>
                val form =
                  disposalDate.fold(getDisposalDateFrom(taxYear))(c => getDisposalDateFrom(taxYear).fill(c.value))
                Ok(multipleDisposalsDisposalDatePage(form, backLink, false))

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
            val answers  = m.examplePropertyDetailsAnswers
            val backLink = disposalDateBackLink(answers)
            m.triageAnswers.fold(_.taxYear, c => Some(c.taxYear)) match {
              case Some(taxYear) =>
                getDisposalDateFrom(taxYear)
                  .bindFromRequest()
                  .fold(
                    formWithErrors =>
                      BadRequest(
                        multipleDisposalsDisposalDatePage(formWithErrors, backLink, false)
                      ), { date =>
                      val disposalDate = DisposalDate(date, taxYear)

                      if (answers
                            .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
                            .fold(_.disposalDate, c => Some(c.disposalDate))
                            .contains(disposalDate)) {
                        Redirect(routes.PropertyDetailsController.checkYourAnswers())
                      } else {
                        val updatedAnswers =
                          answers
                            .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
                            .fold(
                              _.copy(disposalDate = Some(disposalDate)),
                              _.copy(disposalDate = disposalDate)
                            )
                        val updatedDraftReturn = m.copy(examplePropertyDetailsAnswers = Some(updatedAnswers))
                        val result = for {
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
            val answers  = m.examplePropertyDetailsAnswers
            val backLink = disposalPriceBackLink(answers)

            val disposalPrice = answers
              .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
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
            val answers  = m.examplePropertyDetailsAnswers
            val backLink = disposalPriceBackLink(answers)

            disposalPriceForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    multipleDisposalsDisposalPricePage(formWithErrors, backLink)
                  ), { disposalPrice =>
                  if (answers
                        .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
                        .fold(_.disposalPrice, c => Some(c.disposalPrice))
                        .contains(AmountInPence.fromPounds(disposalPrice))) {
                    Redirect(routes.PropertyDetailsController.checkYourAnswers())
                  } else {
                    val updatedAnswers =
                      answers
                        .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
                        .fold(
                          _.copy(disposalPrice = Some(AmountInPence.fromPounds(disposalPrice))),
                          _.copy(disposalPrice = AmountInPence.fromPounds(disposalPrice))
                        )
                    val updatedDraftReturn = m.copy(examplePropertyDetailsAnswers = Some(updatedAnswers))
                    val result = for {
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
            val answers  = m.examplePropertyDetailsAnswers
            val backLink = acquisitionPriceBackLink(answers)

            val acquisitionPrice = answers
              .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
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
            val answers  = m.examplePropertyDetailsAnswers
            val backLink = acquisitionPriceBackLink(answers)

            acquisitionPriceForm
              .bindFromRequest()
              .fold(
                formWithErrors =>
                  BadRequest(
                    multipleDisposalsDisposalPricePage(formWithErrors, backLink)
                  ), { acquisitionPrice =>
                  if (answers
                        .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
                        .fold(_.acquisitionPrice, c => Some(c.acquisitionPrice))
                        .contains(AmountInPence.fromPounds(acquisitionPrice))) {
                    Redirect(routes.PropertyDetailsController.checkYourAnswers())
                  } else {
                    val updatedAnswers =
                      answers
                        .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
                        .fold(
                          _.copy(acquisitionPrice = Some(AmountInPence.fromPounds(acquisitionPrice))),
                          _.copy(acquisitionPrice = AmountInPence.fromPounds(acquisitionPrice))
                        )
                    val updatedDraftReturn = m.copy(examplePropertyDetailsAnswers = Some(updatedAnswers))
                    val result = for {
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
        r.draftReturn match {
          case m: MultipleDisposalsDraftReturn =>
            m.examplePropertyDetailsAnswers.fold[Future[Result]](
              Redirect(routes.PropertyDetailsController.multipleDisposalsGuidance())
            ) {
              case IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(None, _, _, _) =>
                Redirect(routes.PropertyDetailsController.multipleDisposalsGuidance())

              case IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(_, None, _, _) =>
                Redirect(routes.PropertyDetailsController.disposalDate())

              case IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(_, _, None, _) =>
                Redirect(routes.PropertyDetailsController.disposalPrice())

              case IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(_, _, _, None) =>
                Redirect(routes.PropertyDetailsController.acquisitionPrice())

              case IncompleteMultipleDisposalsExamplePropertyDetailsAnswers(Some(a), Some(dd), Some(dp), Some(ap)) =>
                val completeAnswers    = CompleteMultipleDisposalsExamplePropertyDetailsAnswers(a, dd, dp, ap)
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
                  _ => Ok(multipleDisposalsCheckYourAnswersPage(completeAnswers))
                )

              case c: CompleteMultipleDisposalsExamplePropertyDetailsAnswers =>
                Ok(multipleDisposalsCheckYourAnswersPage(c))

            }

          case s: SingleDisposalDraftReturn =>
            s.propertyAddress.fold(
              Redirect(routes.PropertyDetailsController.enterPostcode())
            )(address => Ok(singleDisposalCheckYourAnswersPage(address)))
        }
    }
  }

  def checkYourAnswersSubmit(): Action[AnyContent] = authenticatedActionWithSessionData.async { implicit request =>
    withValidJourney(request) {
      case (_, _) =>
        Redirect(returnsRoutes.TaskListController.taskList())
    }
  }

  private def getDisposalDateFrom(taxYear: TaxYear): Form[LocalDate] = {
    val today              = LocalDateUtils.today()
    val startDateOfTaxYear = taxYear.startDateInclusive
    val endDateOfTaxYear   = taxYear.endDateExclusive

    val maximumDateInclusive = if (endDateOfTaxYear.isAfter(today)) endDateOfTaxYear else today

    disposalDateForm(maximumDateInclusive, startDateOfTaxYear)
  }

  private def disposalDateBackLink(answers: Option[MultipleDisposalsExamplePropertyDetailsAnswers]): Call =
    answers
      .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty)
      .fold(
        _ => routes.PropertyDetailsController.enterUkAddress(),
        _ => routes.PropertyDetailsController.checkYourAnswers()
      )

  private def disposalPriceBackLink(answers: Option[MultipleDisposalsExamplePropertyDetailsAnswers]): Call =
    answers
      .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty) // TODO: required?
      .fold(
        _ => routes.PropertyDetailsController.disposalDate(),
        _ => routes.PropertyDetailsController.checkYourAnswers()
      )

  private def acquisitionPriceBackLink(answers: Option[MultipleDisposalsExamplePropertyDetailsAnswers]): Call =
    answers
      .getOrElse(IncompleteMultipleDisposalsExamplePropertyDetailsAnswers.empty) // TODO: required?
      .fold(
        _ => routes.PropertyDetailsController.disposalPrice(),
        _ => routes.PropertyDetailsController.checkYourAnswers()
      )

  // the following aren't used for the returns journey - the returns journey only handles uk addresses
  protected lazy val isUkCall: Call                    = enterPostcodeCall
  protected lazy val isUkSubmitCall: Call              = enterPostcodeCall
  protected lazy val enterNonUkAddressCall: Call       = enterPostcodeCall
  protected lazy val enterNonUkAddressSubmitCall: Call = enterPostcodeCall
  protected lazy val backLinkCall: Call                = enterPostcodeCall

}

object PropertyDetailsController {

  def disposalDateForm(maximumDateInclusive: LocalDate, minimumDateInclusive: LocalDate): Form[LocalDate] = {
    val key = "multipleDisposalsDisposalDate"

    Form(
      mapping(
        "" -> of(
          LocalDateUtils.dateFormatter(
            Some(maximumDateInclusive), //TODO: earliest of [Today, End of the tax year]
            Some(minimumDateInclusive), //TODO: start of the tax year
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
        "disposalPrice" -> of(MoneyUtils.amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

  val acquisitionPriceForm: Form[BigDecimal] =
    Form(
      mapping(
        "acquisitionPrice" -> of(MoneyUtils.amountInPoundsFormatter(_ <= 0, _ > MoneyUtils.maxAmountOfPounds))
      )(identity)(Some(_))
    )

}
