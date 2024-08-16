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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.Lang
import play.api.libs.json.{JsNumber, JsString, Json}
import play.api.mvc.Request
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ViewConfig
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.JourneyStatus.FillingOutReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.AddressGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.CompleteReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.DraftReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExampleCompanyDetailsAnswersGen.completeExampleCompanyDetailsAnswersGen
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ExamplePropertyDetailsAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.JourneyStatusGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnAPIGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.ReturnGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SingleMixedUseDetailsAnswersGen.completeMixedUsePropertyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TaxYearGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.TriageQuestionsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.{CompleteMultipleDisposalsReturn, CompleteMultipleIndirectDisposalReturn, CompleteSingleDisposalReturn, CompleteSingleIndirectDisposalReturn, CompleteSingleMixedUseDisposalReturn}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExampleCompanyDetailsAnswers.CompleteExampleCompanyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.ExamplePropertyDetailsAnswers.CompleteExamplePropertyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MixedUsePropertyDetailsAnswers.CompleteMixedUsePropertyDetailsAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.{CompleteMultipleDisposalsTriageAnswers, IncompleteMultipleDisposalsTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SubmitReturnResponse.ReturnCharge
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TaxYear, TimeUtils}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.{GetDraftReturnResponse, ListReturnsResponse}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ReturnsServiceImplSpec extends AnyWordSpec with Matchers with MockFactory with GuiceOneAppPerSuite {

  private val mockConnector = mock[ReturnsConnector]
  private val config        = app.injector.instanceOf[ViewConfig]
  private val language      = Lang("en")

  private def mockStoreDraftReturn(
    draftReturn: DraftReturn,
    cgtReference: CgtReference
  )(
    response: Either[Error, Unit]
  ) =
    (mockConnector
      .storeDraftReturn(_: DraftReturn, _: CgtReference)(_: HeaderCarrier))
      .expects(draftReturn, cgtReference, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockGetDraftReturns(
    cgtReference: CgtReference
  )(response: Either[Error, GetDraftReturnResponse]) =
    (mockConnector
      .getDraftReturns(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockSubmitReturn(
    submitReturnRequest: SubmitReturnRequest,
    lang: Lang
  )(response: Either[Error, SubmitReturnResponse]) =
    (mockConnector
      .submitReturn(_: SubmitReturnRequest, _: Lang)(_: HeaderCarrier))
      .expects(submitReturnRequest, lang, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockListReturn(
    cgtReference: CgtReference,
    fromDate: LocalDate,
    toDate: LocalDate
  )(
    response: Either[Error, ListReturnsResponse]
  ) =
    (mockConnector
      .listReturns(_: CgtReference, _: LocalDate, _: LocalDate)(
        _: HeaderCarrier
      ))
      .expects(cgtReference, fromDate, toDate, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockDisplayReturn(cgtReference: CgtReference, submissionId: String)(
    response: Either[Error, DisplayReturn]
  ) =
    (mockConnector
      .displayReturn(_: CgtReference, _: String)(_: HeaderCarrier))
      .expects(cgtReference, submissionId, *)
      .returning(EitherT.fromEither[Future](response))

  private def mockDeleteDraftReturns(
    draftReturnIds: List[UUID]
  )(response: Either[Error, Unit]) =
    (mockConnector
      .deleteDraftReturns(_: List[UUID])(_: HeaderCarrier))
      .expects(draftReturnIds, *)
      .returning(EitherT.fromEither[Future](response))

  val service = new ReturnsServiceImpl(mockConnector, stub[AuditService], config)

  implicit val hc: HeaderCarrier   = HeaderCarrier()
  implicit val request: Request[_] = FakeRequest()
  private val emptyJsonBody        = "{}"

  "ReturnsServiceImpl" when {

    "handling requests to store draft returns" must {

      val fillingOutReturn = sample[FillingOutReturn].copy(amendReturnData = None)
      val draftReturn      = fillingOutReturn.draftReturn
      val cgtReference     = fillingOutReturn.subscribedDetails.cgtReference

      "return an error" when {

        "the connector fails for any reason" in {
          mockStoreDraftReturn(draftReturn, cgtReference)(Left(Error("some error")))

          val result = await(service.storeDraftReturn(fillingOutReturn).value)

          result shouldBe Left(Error("some error"))
        }
      }

      "return an ok response" when {

        "the http call came back with a 200" in {
          mockStoreDraftReturn(draftReturn, cgtReference)(Right(()))

          await(
            service.storeDraftReturn(fillingOutReturn).value
          ) shouldBe Right(())
        }

      }

      "immediately return successfully without doing anything" when {

        "the return is an amend return" in {
          val result = service.storeDraftReturn(
            sample[FillingOutReturn].copy(amendReturnData = Some(sample[AmendReturnData]))
          )
          await(result.value) shouldBe Right(())
        }

      }

    }

    "handling requests to get draft returns for single disposal journey" must {

      val cgtReference = sample[CgtReference]

      val sentReturns = List(sample[ReturnSummary])

      "return an error" when {
        "the connetor returns an error" in {
          mockGetDraftReturns(cgtReference)(Left(Error("some error")))

          val result = await(service.getDraftReturns(cgtReference, sentReturns).value)

          result shouldBe Left(Error("some error"))
        }
      }

      "return an ok response" when {

        "the http call came back with a 200 and the body can be parsed" in {
          val draftReturnsResponse =
            GetDraftReturnResponse(
              List(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = IncompleteSingleDisposalTriageAnswers.empty
                )
              )
            )

          mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))

          await(
            service.getDraftReturns(cgtReference, sentReturns).value
          ) shouldBe Right(
            draftReturnsResponse.draftReturns
          )
        }

        "there are draft returns which are found to have been already sent and" when {

          "the draft return is a single disposal draft return and" when {
            val cgtReference  = sample[CgtReference]
            val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(individualUserType = None)
            val address       = sample[UkAddress]
            val draftReturnId = UUID.randomUUID()

            val draftReturn          =
              sample[DraftSingleDisposalReturn].copy(
                triageAnswers = triageAnswers,
                propertyAddress = Some(address.copy(postcode = Postcode(" z Z 01 Zz  "))),
                id = draftReturnId
              )
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))

            val sentReturn  = sample[ReturnSummary].copy(
              taxYear = triageAnswers.disposalDate.taxYear.startDateInclusive.getYear.toString,
              completionDate = triageAnswers.completionDate.value,
              propertyAddress = address.copy(postcode = Postcode("ZZ01ZZ"))
            )
            val sentReturns = List(sentReturn)

            "the draft returns are successfully deleted" in {
              inSequence {
                mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
                mockDeleteDraftReturns(List(draftReturnId))(Right(()))
              }

              await(
                service.getDraftReturns(cgtReference, sentReturns).value
              ) shouldBe Right(List.empty)
            }

            "the draft returns cannot be deleted" in {
              inSequence {
                mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
                mockDeleteDraftReturns(List(draftReturnId))(Right(()))
              }

              await(
                service.getDraftReturns(cgtReference, sentReturns).value
              ) shouldBe Right(List.empty)
            }

          }

        }

        "there are draft returns which have disposal dates or completion dates incompatible with dates of death" when {

          def testDraftReturnIsDeleted(draftReturn: DraftReturn): Unit = {
            val cgtReference         = sample[CgtReference]
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))

            inSequence {
              mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
              mockDeleteDraftReturns(List(draftReturn.id))(Right(()))
            }

            await(
              service.getDraftReturns(cgtReference, List.empty).value
            ) shouldBe Right(List.empty)
          }

          "the user is a period of admin personal rep and" when {

            val dateOfDeath = LocalDate.now()

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val singleDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                disposalDate = DisposalDate(dateOfDeath, sample[TaxYear])
              )

            val examplePropertyDetailsAnswers =
              sample[CompleteExamplePropertyDetailsAnswers].copy(
                disposalDate = DisposalDate(dateOfDeath, sample[TaxYear])
              )

            "the user is on a single disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a multiple disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                    individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
                  ),
                  examplePropertyDetailsAnswers = Some(examplePropertyDetailsAnswers),
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a single indirect disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a single mixed use journey" in {
              testDraftReturnIsDeleted(
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }
          }

          "the user is a non-period of admin personal rep and" when {

            val dateOfDeath = LocalDate.now()

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val singleDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentative),
                disposalDate = DisposalDate(dateOfDeath.plusDays(1L), sample[TaxYear])
              )

            val examplePropertyDetailsAnswers =
              sample[CompleteExamplePropertyDetailsAnswers].copy(
                disposalDate = DisposalDate(dateOfDeath.plusDays(1L), sample[TaxYear])
              )

            "the user is on a single disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a multiple disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                    individualUserType = Some(PersonalRepresentative)
                  ),
                  examplePropertyDetailsAnswers = Some(examplePropertyDetailsAnswers),
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a single indirect disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a single mixed use journey" in {
              testDraftReturnIsDeleted(
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }
          }

        }

        "there are draft returns which have disposal dates or completion dates compatible with dates of death" when {

          def testDraftReturnIsNotDeleted(draftReturn: DraftReturn): Unit = {
            val cgtReference         = sample[CgtReference]
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))
            inSequence {
              mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
            }

            await(
              service.getDraftReturns(cgtReference, List.empty).value
            ) shouldBe Right(List(draftReturn))
          }

          val dateOfDeath = LocalDate.of(2020, 4, 20)

          val taxYearExchanged = TimeUtils.getTaxYearExchangedOfADate(dateOfDeath)

          val taxYear = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(dateOfDeath.getYear, 4, 6),
            endDateExclusive = LocalDate.of(dateOfDeath.getYear + 1, 4, 6)
          )

          val alreadySentSA = taxYearExchanged.year match {
            case 2020 | 2021 => Some(false)
            case _           => None
          }

          "the user is a period of admin personal rep and" when {

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val singleDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                disposalDate = DisposalDate(dateOfDeath.plusDays(1L), taxYear),
                alreadySentSelfAssessment = alreadySentSA,
                completionDate = CompletionDate(dateOfDeath.plusDays(1L))
              )

            "the user is on a single disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a single indirect disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a single mixed use journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

          "the user is a non-period of admin personal rep and" when {

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val singleDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentative),
                disposalDate = DisposalDate(dateOfDeath.minusDays(2), taxYear),
                alreadySentSelfAssessment = alreadySentSA
              )

            "the user is on a single disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a single indirect disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a single mixed use journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

        }

      }

    }

    "handling requests to get draft returns for single indirect disposal journey" must {

      val cgtReference = sample[CgtReference]

      val sentReturns = List(sample[ReturnSummary])

      "return an ok response" when {

        "the http call came back with a 200 and the body can be parsed" in {
          val draftReturnsResponse =
            GetDraftReturnResponse(
              List(
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = IncompleteSingleDisposalTriageAnswers.empty.copy(
                    assetType = Some(AssetType.IndirectDisposal)
                  )
                )
              )
            )

          mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))

          await(
            service.getDraftReturns(cgtReference, sentReturns).value
          ) shouldBe Right(
            draftReturnsResponse.draftReturns
          )
        }

        "there are draft returns which are found to have been already sent and" when {

          "the draft return is a single disposal draft return and" when {
            val cgtReference  = sample[CgtReference]
            val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = None,
              assetType = AssetType.IndirectDisposal
            )
            val address       = sample[UkAddress]
            val draftReturnId = UUID.randomUUID()

            val draftReturn          =
              sample[DraftSingleIndirectDisposalReturn].copy(
                triageAnswers = triageAnswers,
                companyAddress = Some(address.copy(postcode = Postcode(" z Z 01 Zz  "))),
                id = draftReturnId
              )
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))

            val sentReturn  = sample[ReturnSummary].copy(
              taxYear = triageAnswers.disposalDate.taxYear.startDateInclusive.getYear.toString,
              completionDate = triageAnswers.completionDate.value,
              propertyAddress = address.copy(postcode = Postcode("ZZ01ZZ"))
            )
            val sentReturns = List(sentReturn)

            "the draft returns are successfully deleted" in {
              inSequence {
                mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
                mockDeleteDraftReturns(List(draftReturnId))(Right(HttpResponse(OK, emptyJsonBody)))
              }

              await(
                service.getDraftReturns(cgtReference, sentReturns).value
              ) shouldBe Right(List.empty)
            }

            "the draft returns cannot be deleted" in {
              inSequence {
                mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
                mockDeleteDraftReturns(List(draftReturnId))(Left(Error("")))
              }

              await(
                service.getDraftReturns(cgtReference, sentReturns).value
              ) shouldBe Right(List.empty)
            }

          }

        }

        "there are draft returns which have disposal dates or completion dates incompatible with dates of death" when {

          def testDraftReturnIsDeleted(draftReturn: DraftReturn): Unit = {
            val cgtReference         = sample[CgtReference]
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))

            inSequence {
              mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
              mockDeleteDraftReturns(List(draftReturn.id))(
                Right(HttpResponse(OK, emptyJsonBody))
              )
            }

            await(
              service.getDraftReturns(cgtReference, List.empty).value
            ) shouldBe Right(List.empty)
          }

          "the user is a period of admin personal rep and" when {

            val dateOfDeath = LocalDate.now()

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val singleIndirectDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                disposalDate = DisposalDate(dateOfDeath, sample[TaxYear]),
                assetType = AssetType.IndirectDisposal
              )

            "the user is on a single indirect disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = singleIndirectDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

          "the user is a non-period of admin personal rep and" when {

            val dateOfDeath = LocalDate.now()

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val singleIndirectDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentative),
                disposalDate = DisposalDate(dateOfDeath.plusDays(1L), sample[TaxYear]),
                assetType = AssetType.IndirectDisposal
              )

            "the user is on a single indirect disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = singleIndirectDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

        }

        "there are draft returns which have disposal dates or completion dates compatible with dates of death" when {

          def testDraftReturnIsNotDeleted(draftReturn: DraftReturn): Unit = {
            val cgtReference         = sample[CgtReference]
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))
            mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))

            await(
              service.getDraftReturns(cgtReference, List.empty).value
            ) shouldBe Right(List(draftReturn))
          }

          val dateOfDeath = LocalDate.of(2020, 4, 20)

          val taxYearExchanged = TimeUtils.getTaxYearExchangedOfADate(dateOfDeath)

          val taxYear = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(dateOfDeath.getYear, 4, 6),
            endDateExclusive = LocalDate.of(dateOfDeath.getYear + 1, 4, 6)
          )

          val alreadySentSA = taxYearExchanged.year match {
            case 2020 | 2021 => Some(false)
            case _           => None
          }

          "the user is a period of admin personal rep and" when {

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val singleIndirectDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                disposalDate = DisposalDate(dateOfDeath.plusDays(1L), taxYear),
                alreadySentSelfAssessment = alreadySentSA,
                completionDate = CompletionDate(dateOfDeath.plusDays(1L)),
                assetType = AssetType.IndirectDisposal
              )

            "the user is on a single indirect disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = singleIndirectDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

          "the user is a non-period of admin personal rep and" when {

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val singleIndirectDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentative),
                disposalDate = DisposalDate(dateOfDeath.minusDays(2), taxYear),
                alreadySentSelfAssessment = alreadySentSA,
                assetType = AssetType.IndirectDisposal
              )

            "the user is on a single indirect disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = singleIndirectDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

        }

      }

    }

    "handling requests to get draft returns for single mixeduse disposal journey" must {

      val cgtReference = sample[CgtReference]

      val sentReturns = List(sample[ReturnSummary])

      "return an ok response" when {

        "the http call came back with a 200 and the body can be parsed" in {
          val draftReturnsResponse =
            GetDraftReturnResponse(
              List(
                sample[DraftSingleIndirectDisposalReturn].copy(
                  triageAnswers = IncompleteSingleDisposalTriageAnswers.empty.copy(
                    assetType = Some(AssetType.MixedUse)
                  )
                )
              )
            )

          mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))

          await(
            service.getDraftReturns(cgtReference, sentReturns).value
          ) shouldBe Right(
            draftReturnsResponse.draftReturns
          )
        }

        "there are draft returns which are found to have been already sent and" when {

          "the draft return is a single disposal draft return and" when {
            val cgtReference  = sample[CgtReference]
            val triageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
              individualUserType = None,
              assetType = AssetType.MixedUse
            )
            val address       = sample[UkAddress]
            val draftReturnId = UUID.randomUUID()

            val mixedUsePropertyDetailsAnswers = sample[CompleteMixedUsePropertyDetailsAnswers].copy(
              address = address.copy(postcode = Postcode(" z Z 01 Zz  "))
            )

            val draftReturn          =
              sample[DraftSingleMixedUseDisposalReturn].copy(
                triageAnswers = triageAnswers,
                mixedUsePropertyDetailsAnswers = Some(mixedUsePropertyDetailsAnswers),
                id = draftReturnId
              )
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))

            val sentReturn  = sample[ReturnSummary].copy(
              taxYear = triageAnswers.disposalDate.taxYear.startDateInclusive.getYear.toString,
              completionDate = triageAnswers.completionDate.value,
              propertyAddress = address.copy(postcode = Postcode("ZZ01ZZ"))
            )
            val sentReturns = List(sentReturn)

            "the draft returns are successfully deleted" in {
              inSequence {
                mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
                mockDeleteDraftReturns(List(draftReturnId))(
                  Right(HttpResponse(OK, emptyJsonBody))
                )
              }

              await(
                service.getDraftReturns(cgtReference, sentReturns).value
              ) shouldBe Right(List.empty)
            }

            "the draft returns cannot be deleted" in {
              inSequence {
                mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
                mockDeleteDraftReturns(List(draftReturnId))(Left(Error("")))
              }

              await(
                service.getDraftReturns(cgtReference, sentReturns).value
              ) shouldBe Right(List.empty)
            }

          }

        }

        "there are draft returns which have disposal dates or completion dates incompatible with dates of death" when {

          def testDraftReturnIsDeleted(draftReturn: DraftReturn): Unit = {
            val cgtReference         = sample[CgtReference]
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))

            inSequence {
              mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
              mockDeleteDraftReturns(List(draftReturn.id))(
                Right(HttpResponse(OK, emptyJsonBody))
              )
            }

            await(
              service.getDraftReturns(cgtReference, List.empty).value
            ) shouldBe Right(List.empty)
          }

          "the user is a period of admin personal rep and" when {

            val dateOfDeath = LocalDate.now()

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val singleMixedUseDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                disposalDate = DisposalDate(dateOfDeath, sample[TaxYear]),
                assetType = AssetType.MixedUse
              )

            "the user is on a single indirect disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = singleMixedUseDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

          "the user is a non-period of admin personal rep and" when {

            val dateOfDeath = LocalDate.now()

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val singleMixedUseDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentative),
                disposalDate = DisposalDate(dateOfDeath.plusDays(1L), sample[TaxYear]),
                assetType = AssetType.MixedUse
              )

            "the user is on a single indirect disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = singleMixedUseDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

        }

        "there are draft returns which have disposal dates or completion dates compatible with dates of death" when {

          def testDraftReturnIsNotDeleted(draftReturn: DraftReturn): Unit = {
            val cgtReference         = sample[CgtReference]
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))
            mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))

            await(
              service.getDraftReturns(cgtReference, List.empty).value
            ) shouldBe Right(List(draftReturn))
          }

          val dateOfDeath = LocalDate.of(2020, 4, 20)

          val taxYearExchanged = TimeUtils.getTaxYearExchangedOfADate(dateOfDeath)

          val taxYear = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(dateOfDeath.getYear, 4, 6),
            endDateExclusive = LocalDate.of(dateOfDeath.getYear + 1, 4, 6)
          )

          val alreadySentSA = taxYearExchanged.year match {
            case 2020 | 2021 => Some(false)
            case _           => None
          }

          "the user is a period of admin personal rep and" when {

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val singleMixedUseDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                disposalDate = DisposalDate(dateOfDeath.plusDays(1L), taxYear),
                alreadySentSelfAssessment = alreadySentSA,
                completionDate = CompletionDate(dateOfDeath.plusDays(1L)),
                assetType = AssetType.MixedUse
              )

            "the user is on a single mixeduse disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = singleMixedUseDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

          "the user is a non-period of admin personal rep and" when {

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val singleMixedUseDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentative),
                disposalDate = DisposalDate(dateOfDeath.minusDays(2), taxYear),
                alreadySentSelfAssessment = alreadySentSA,
                assetType = AssetType.IndirectDisposal
              )

            "the user is on a single mixeduse disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftSingleMixedUseDisposalReturn].copy(
                  triageAnswers = singleMixedUseDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

        }

      }

    }

    "handling requests to get draft returns for multiple disposals journey" must {

      val cgtReference = sample[CgtReference]

      val sentReturns = List(sample[ReturnSummary])

      "return an ok response" when {

        "the http call came back with a 200 and the body can be parsed" in {
          val draftReturnsResponse =
            GetDraftReturnResponse(
              List(
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = IncompleteMultipleDisposalsTriageAnswers.empty
                )
              )
            )

          mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))

          await(
            service.getDraftReturns(cgtReference, sentReturns).value
          ) shouldBe Right(
            draftReturnsResponse.draftReturns
          )
        }

        "there are draft returns which are found to have been already sent and" when {

          "the draft return is a single disposal draft return and" when {
            val cgtReference  = sample[CgtReference]
            val address       = sample[UkAddress]
            val draftReturnId = UUID.randomUUID()

            val triageAnswers                 = sample[CompleteMultipleDisposalsTriageAnswers].copy(individualUserType = None)
            val examplePropertyDetailsAnswers = sample[CompleteExamplePropertyDetailsAnswers].copy(
              address = address.copy(postcode = Postcode(" z Z 01 Zz  "))
            )

            val draftReturn          =
              sample[DraftMultipleDisposalsReturn].copy(
                triageAnswers = triageAnswers,
                examplePropertyDetailsAnswers = Some(examplePropertyDetailsAnswers),
                id = draftReturnId
              )
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))

            val sentReturn  = sample[ReturnSummary].copy(
              taxYear = triageAnswers.taxYear.startDateInclusive.getYear.toString,
              completionDate = triageAnswers.completionDate.value,
              propertyAddress = address.copy(postcode = Postcode("ZZ01ZZ"))
            )
            val sentReturns = List(sentReturn)

            "the draft returns are successfully deleted" in {
              inSequence {
                mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
                mockDeleteDraftReturns(List(draftReturnId))(
                  Right(HttpResponse(OK, emptyJsonBody))
                )
              }

              await(
                service.getDraftReturns(cgtReference, sentReturns).value
              ) shouldBe Right(List.empty)
            }

            "the draft returns cannot be deleted" in {
              inSequence {
                mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
                mockDeleteDraftReturns(List(draftReturnId))(Left(Error("")))
              }

              await(
                service.getDraftReturns(cgtReference, sentReturns).value
              ) shouldBe Right(List.empty)
            }

          }

        }

        "there are draft returns which have disposal dates or completion dates incompatible with dates of death" when {

          def testDraftReturnIsDeleted(draftReturn: DraftReturn): Unit = {
            val cgtReference         = sample[CgtReference]
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))

            inSequence {
              mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
              mockDeleteDraftReturns(List(draftReturn.id))(
                Right(HttpResponse(OK, emptyJsonBody))
              )
            }

            await(
              service.getDraftReturns(cgtReference, List.empty).value
            ) shouldBe Right(List.empty)
          }

          "the user is a period of admin personal rep and" when {

            val dateOfDeath = LocalDate.now()

            val representeeAnswers =
              sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(dateOfDeath)))

            val multipleDisposalsTriageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                completionDate = CompletionDate(dateOfDeath)
              )

            val examplePropertyDetailsAnswers =
              sample[CompleteExamplePropertyDetailsAnswers].copy(
                disposalDate = DisposalDate(dateOfDeath, sample[TaxYear])
              )

            "the user is on a multiple disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                    individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin)
                  ),
                  examplePropertyDetailsAnswers = Some(examplePropertyDetailsAnswers),
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a multiple indirect disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

          "the user is a non-period of admin personal rep and" when {

            val dateOfDeath = LocalDate.now()

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val multipleDisposalsTriageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(
                  individualUserType = Some(PersonalRepresentative),
                  completionDate = CompletionDate(dateOfDeath.plusDays(1L))
                )

            val examplePropertyDetailsAnswers =
              sample[CompleteExamplePropertyDetailsAnswers].copy(
                disposalDate = DisposalDate(dateOfDeath.plusDays(1L), sample[TaxYear])
              )

            "the user is on a multiple disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
                    individualUserType = Some(PersonalRepresentative)
                  ),
                  examplePropertyDetailsAnswers = Some(examplePropertyDetailsAnswers),
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a multiple indirect disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

        }

        "there are draft returns which have disposal dates or completion dates compatible with dates of death" when {

          def testDraftReturnIsNotDeleted(draftReturn: DraftReturn): Unit = {
            val cgtReference         = sample[CgtReference]
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))
            mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))

            await(
              service.getDraftReturns(cgtReference, List.empty).value
            ) shouldBe Right(List(draftReturn))
          }

          val dateOfDeath = LocalDate.of(2020, 4, 20)

          val taxYearExchanged = TimeUtils.getTaxYearExchangedOfADate(dateOfDeath)

          val taxYear = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(dateOfDeath.getYear, 4, 6),
            endDateExclusive = LocalDate.of(dateOfDeath.getYear + 1, 4, 6)
          )

          val alreadySentSA = taxYearExchanged.year match {
            case 2020 | 2021 => Some(false)
            case _           => None
          }

          "the user is a period of admin personal rep and" when {

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val multipleDisposalsTriageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                completionDate = CompletionDate(dateOfDeath.plusDays(1L)),
                alreadySentSelfAssessment = alreadySentSA,
                taxYearExchanged = Some(taxYearExchanged),
                taxYear = taxYear
              )

            val examplePropertyDetailsAnswers =
              sample[CompleteExamplePropertyDetailsAnswers].copy(
                disposalDate = DisposalDate(dateOfDeath.plusDays(1L), taxYear)
              )

            "the user is on a multiple disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
                  examplePropertyDetailsAnswers = Some(examplePropertyDetailsAnswers),
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a multiple indirect disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers.copy(
                    assetTypes = List(AssetType.IndirectDisposal)
                  ),
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

          "the user is a non-period of admin personal rep and" when {

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val multipleDisposalsTriageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentative),
                completionDate = CompletionDate(dateOfDeath.minusDays(2)),
                taxYearExchanged = Some(taxYearExchanged),
                taxYear = taxYear,
                alreadySentSelfAssessment = alreadySentSA
              )

            val examplePropertyDetailsAnswers =
              sample[CompleteExamplePropertyDetailsAnswers].copy(
                disposalDate = DisposalDate(dateOfDeath.minusDays(2), taxYear)
              )

            "the user is on a multiple disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
                  examplePropertyDetailsAnswers = Some(examplePropertyDetailsAnswers),
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a multiple indirect disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers.copy(
                    assetTypes = List(AssetType.IndirectDisposal)
                  ),
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

        }

      }

    }

    "handling requests to get draft returns for multiple indirect disposals journey" must {

      val cgtReference = sample[CgtReference]

      val sentReturns = List(sample[ReturnSummary])

      "return an ok response" when {

        "the http call came back with a 200 and the body can be parsed" in {
          val draftReturnsResponse =
            GetDraftReturnResponse(
              List(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = IncompleteMultipleDisposalsTriageAnswers.empty.copy(
                    assetTypes = Some(List(AssetType.MixedUse))
                  )
                )
              )
            )

          mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))

          await(
            service.getDraftReturns(cgtReference, sentReturns).value
          ) shouldBe Right(
            draftReturnsResponse.draftReturns
          )
        }

        "there are draft returns which are found to have been already sent and" when {

          "the draft return is a single disposal draft return and" when {
            val cgtReference  = sample[CgtReference]
            val address       = sample[UkAddress]
            val draftReturnId = UUID.randomUUID()

            val triageAnswers                = sample[CompleteMultipleDisposalsTriageAnswers].copy(
              individualUserType = None,
              assetTypes = List(AssetType.MixedUse)
            )
            val exampleCompanyDetailsAnswers = sample[CompleteExampleCompanyDetailsAnswers].copy(
              address = address.copy(postcode = Postcode(" z Z 01 Zz  "))
            )

            val draftReturn          =
              sample[DraftMultipleIndirectDisposalsReturn].copy(
                triageAnswers = triageAnswers,
                exampleCompanyDetailsAnswers = Some(exampleCompanyDetailsAnswers),
                id = draftReturnId
              )
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))

            val sentReturn  = sample[ReturnSummary].copy(
              taxYear = triageAnswers.taxYear.startDateInclusive.getYear.toString,
              completionDate = triageAnswers.completionDate.value,
              propertyAddress = address.copy(postcode = Postcode("ZZ01ZZ"))
            )
            val sentReturns = List(sentReturn)

            "the draft returns are successfully deleted" in {
              inSequence {
                mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
                mockDeleteDraftReturns(List(draftReturnId))(
                  Right(HttpResponse(OK, emptyJsonBody))
                )
              }

              await(
                service.getDraftReturns(cgtReference, sentReturns).value
              ) shouldBe Right(List.empty)
            }

            "the draft returns cannot be deleted" in {
              inSequence {
                mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
                mockDeleteDraftReturns(List(draftReturnId))(Left(Error("")))
              }

              await(
                service.getDraftReturns(cgtReference, sentReturns).value
              ) shouldBe Right(List.empty)
            }

          }

        }

        "there are draft returns which have disposal dates or completion dates incompatible with dates of death" when {

          def testDraftReturnIsDeleted(draftReturn: DraftReturn): Unit = {
            val cgtReference         = sample[CgtReference]
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))

            inSequence {
              mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))
              mockDeleteDraftReturns(List(draftReturn.id))(
                Right(HttpResponse(OK, emptyJsonBody))
              )
            }

            await(
              service.getDraftReturns(cgtReference, List.empty).value
            ) shouldBe Right(List.empty)
          }

          "the user is a period of admin personal rep and" when {

            val dateOfDeath = LocalDate.now()

            val representeeAnswers =
              sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(dateOfDeath)))

            val multipleDisposalsTriageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                completionDate = CompletionDate(dateOfDeath),
                assetTypes = List(AssetType.IndirectDisposal)
              )

            "the user is on a multiple indirect disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

          "the user is a non-period of admin personal rep and" when {

            val dateOfDeath = LocalDate.now()

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val multipleDisposalsTriageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(
                  individualUserType = Some(PersonalRepresentative),
                  completionDate = CompletionDate(dateOfDeath.plusDays(1L)),
                  assetTypes = List(AssetType.IndirectDisposal)
                )

            "the user is on a multiple indirect disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

        }

        "there are draft returns which have disposal dates or completion dates compatible with dates of death" when {

          def testDraftReturnIsNotDeleted(draftReturn: DraftReturn): Unit = {
            val cgtReference         = sample[CgtReference]
            val draftReturnsResponse = GetDraftReturnResponse(List(draftReturn))
            mockGetDraftReturns(cgtReference)(Right(draftReturnsResponse))

            await(
              service.getDraftReturns(cgtReference, List.empty).value
            ) shouldBe Right(List(draftReturn))
          }

          val dateOfDeath = LocalDate.of(2020, 4, 20)

          val taxYearExchanged = TimeUtils.getTaxYearExchangedOfADate(dateOfDeath)

          val taxYear = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(dateOfDeath.getYear, 4, 6),
            endDateExclusive = LocalDate.of(dateOfDeath.getYear + 1, 4, 6)
          )

          val alreadySentSA = taxYearExchanged.year match {
            case 2020 | 2021 => Some(false)
            case _           => None
          }

          "the user is a period of admin personal rep and" when {

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val multipleDisposalsTriageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                completionDate = CompletionDate(dateOfDeath.plusDays(1L)),
                alreadySentSelfAssessment = alreadySentSA,
                taxYearExchanged = Some(taxYearExchanged),
                taxYear = taxYear,
                assetTypes = List(AssetType.IndirectDisposal)
              )

            "the user is on a multiple indirect disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

          "the user is a non-period of admin personal rep and" when {

            val representeeAnswers = sample[CompleteRepresenteeAnswers].copy(
              dateOfDeath = Some(DateOfDeath(dateOfDeath))
            )

            val multipleDisposalsTriageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentative),
                completionDate = CompletionDate(dateOfDeath.minusDays(2)),
                taxYearExchanged = Some(taxYearExchanged),
                taxYear = taxYear,
                alreadySentSelfAssessment = alreadySentSA,
                assetTypes = List(AssetType.IndirectDisposal)
              )

            "the user is on a multiple indirect disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

          }

        }

      }

    }

    "handling requests to submit a return" must {

      val submitReturnRequest = sample[SubmitReturnRequest]

      "return an error" when {
        "the connector returns an error" in {
          mockSubmitReturn(submitReturnRequest, Lang("en"))(Left(Error("some error")))

          val response = await(service.submitReturn(submitReturnRequest, language).value)

          response shouldBe Left(Error("some error"))
        }
      }

      "return an ok response" when {

        "the http call came back with a 200 and the JSON is valid and contains a charge " in {
          val response =
            SubmitReturnResponse(
              "bundleId",
              LocalDateTime.of(2000, 1, 1, 1, 1),
              Some(
                ReturnCharge(
                  "charge",
                  AmountInPence(123L),
                  LocalDate.of(2000, 1, 1)
                )
              ),
              None
            )

          mockSubmitReturn(submitReturnRequest, Lang("en"))(
            Right(
              SubmitReturnResponse(
                formBundleId = "bundleId",
                processingDate = LocalDateTime.parse("2000-01-01T01:01"),
                charge = Some(
                  ReturnCharge(
                    chargeReference = "charge",
                    amount = AmountInPence(123),
                    dueDate = LocalDate.parse("2000-01-01")
                  )
                ),
                deltaCharge = None
              )
            )
          )

          await(service.submitReturn(submitReturnRequest, language).value) shouldBe Right(response)
        }

        "the http call came back with a 200 and the JSON is valid and does not contain a charge " in {
          val response =
            SubmitReturnResponse(
              "bundleId",
              LocalDateTime.of(LocalDate.of(2000, 1, 1), LocalTime.of(1, 1)),
              None,
              None
            )

          mockSubmitReturn(submitReturnRequest, Lang("en"))(
            Right(
              SubmitReturnResponse(
                formBundleId = "bundleId",
                processingDate = LocalDateTime.parse("2000-01-01T01:01"),
                charge = None,
                deltaCharge = None
              )
            )
          )

          await(service.submitReturn(submitReturnRequest, language).value) shouldBe Right(
            response
          )
        }

      }

    }

    "handling requests to list returns" must {

      val cgtReference     = sample[CgtReference]
      val today: LocalDate = LocalDate.now()
      val currentTaxYear   = TaxYear.thisTaxYearStartDate().getYear
      val fromDate         = TimeUtils.getTaxYearStartDate(currentTaxYear - 4)
      val toDate           = TimeUtils.getTaxYearEndDateInclusive(today)

      "return an error " when {
        "the connector returns an error for any reason" in {
          mockListReturn(cgtReference, fromDate, toDate)(Left(Error("some error")))

          val result = await(service.listReturns(cgtReference).value)

          result shouldBe Left(Error("some error"))
        }
      }

      "return a list of returns" when {

        "the response body can be parsed and converted" in {
          val returnSummary = sample[ReturnSummary]
          val response      = ListReturnsResponse(List(returnSummary))

          mockListReturn(cgtReference, fromDate, toDate)(Right(response))

          await(service.listReturns(cgtReference).value) shouldBe Right(
            List(returnSummary)
          )
        }

      }

    }

    "handling requests to display a return" must {

      val cgtReference = sample[CgtReference]
      val submissionId = "id"

      "return an error " when {
        "the connector fails for any reason" in {
          mockDisplayReturn(cgtReference, submissionId)(Left(Error("some error")))

          val response = await(service.displayReturn(cgtReference, submissionId).value)

          response shouldBe Left(Error("some error"))
        }
      }

      "return a list of returns" when {
        "the response body can be parsed and converted" in {
          val completeReturn: CompleteReturn = sample[CompleteSingleDisposalReturn]
          val displayReturn                  = DisplayReturn(completeReturn, ReturnType.FurtherReturn)

          mockDisplayReturn(cgtReference, submissionId)(Right(displayReturn))

          await(
            service.displayReturn(cgtReference, submissionId).value
          ) shouldBe Right(displayReturn)
        }
      }
    }

    "handling requests to unsetUnwantedSectionsToDraftReturn" must {

      "unset unwanted sections to the draft multiple disposals return" in {
        val draftReturn = sample[DraftMultipleDisposalsReturn]

        val result = service.unsetUnwantedSectionsToDraftReturn(draftReturn)

        result.gainOrLossAfterReliefs     shouldBe None
        result.exemptionAndLossesAnswers  shouldBe None
        result.yearToDateLiabilityAnswers shouldBe None
        result.fold(
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers
        )                                 shouldBe None

      }

      "unset unwanted sections to the draft single disposal return" in {
        val draftReturn = sample[DraftSingleDisposalReturn]

        val result = service.unsetUnwantedSectionsToDraftReturn(draftReturn)

        result.gainOrLossAfterReliefs     shouldBe None
        result.exemptionAndLossesAnswers  shouldBe None
        result.yearToDateLiabilityAnswers shouldBe None
        result.fold(
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers
        )                                 shouldBe None

      }

      "unset unwanted sections to the draft multiple indirect disposals return" in {
        val draftReturn = sample[DraftMultipleIndirectDisposalsReturn]

        val result = service.unsetUnwantedSectionsToDraftReturn(draftReturn)

        result.gainOrLossAfterReliefs     shouldBe None
        result.exemptionAndLossesAnswers  shouldBe None
        result.yearToDateLiabilityAnswers shouldBe None
        result.fold(
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers
        )                                 shouldBe None

      }

      "unset unwanted sections to the draft single indirect disposal return" in {
        val draftReturn = sample[DraftSingleIndirectDisposalReturn]

        val result = service.unsetUnwantedSectionsToDraftReturn(draftReturn)

        result.gainOrLossAfterReliefs     shouldBe None
        result.exemptionAndLossesAnswers  shouldBe None
        result.yearToDateLiabilityAnswers shouldBe None
        result.fold(
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers
        )                                 shouldBe None

      }

      "unset unwanted sections to the draft single mixeduse disposal return" in {
        val draftReturn = sample[DraftSingleMixedUseDisposalReturn]

        val result = service.unsetUnwantedSectionsToDraftReturn(draftReturn)

        result.gainOrLossAfterReliefs     shouldBe None
        result.exemptionAndLossesAnswers  shouldBe None
        result.yearToDateLiabilityAnswers shouldBe None
        result.fold(
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers,
          _.supportingEvidenceAnswers
        )                                 shouldBe None

      }

    }

    "handling requests to updateCorrectTaxYearToSentReturns" must {

      "single disposal journey" when {

        "change invalid taxyear of ReturnSummary to match the disposal date's taxyear" in {

          val cgtReference                = sample[CgtReference]
          val submissionId                = "id"
          val taxYear                     = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2020, 4, 6),
            endDateExclusive = LocalDate.of(2021, 4, 6)
          )
          val disposalDate                = DisposalDate(LocalDate.of(2021, 1, 1), taxYear)
          val singleDisposalTriageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            disposalDate = disposalDate
          )
          val completeReturn              = sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = singleDisposalTriageAnswers
          )
          val displayReturn               = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val inputReturnSummary = List(
            sample[ReturnSummary].copy(
              submissionId = submissionId,
              taxYear = "2021"
            )
          )

          val expectedReturnSummary = inputReturnSummary.map(_.copy(taxYear = "2020"))

          mockDisplayReturn(cgtReference, submissionId)(Right(displayReturn))

          await(
            service.updateCorrectTaxYearToSentReturns(cgtReference, inputReturnSummary).value
          ) shouldBe Right((true, expectedReturnSummary))
        }

        "not change valid taxyear of ReturnSummary to match the disposal date's taxyear" in {

          val cgtReference                = sample[CgtReference]
          val submissionId                = "id"
          val taxYear                     = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2021, 4, 6),
            endDateExclusive = LocalDate.of(2022, 4, 6)
          )
          val disposalDate                = DisposalDate(LocalDate.of(2021, 7, 1), taxYear)
          val singleDisposalTriageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            disposalDate = disposalDate
          )
          val completeReturn              = sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = singleDisposalTriageAnswers
          )
          val displayReturn               = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val inputReturnSummary = List(
            sample[ReturnSummary].copy(
              submissionId = submissionId,
              taxYear = "2021"
            )
          )

          val expectedReturnSummary = inputReturnSummary

          mockDisplayReturn(cgtReference, submissionId)(Right(displayReturn))

          await(
            service.updateCorrectTaxYearToSentReturns(cgtReference, inputReturnSummary).value
          ) shouldBe Right((false, expectedReturnSummary))
        }

      }

      "multiple disposals journey" when {

        "change invalid taxyear of ReturnSummary to match the disposal date's taxyear" in {

          val cgtReference                   = sample[CgtReference]
          val submissionId                   = "id"
          val taxYear                        = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2020, 4, 6),
            endDateExclusive = LocalDate.of(2021, 4, 6)
          )
          val multipleDisposalsTriageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            taxYearExchanged = Some(TaxYearExchanged(2020)),
            taxYear = taxYear
          )
          val completeReturn                 = sample[CompleteMultipleDisposalsReturn].copy(
            triageAnswers = multipleDisposalsTriageAnswers
          )
          val displayReturn                  = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val inputReturnSummary = List(
            sample[ReturnSummary].copy(
              submissionId = submissionId,
              taxYear = "2021"
            )
          )

          val expectedReturnSummary = inputReturnSummary.map(_.copy(taxYear = "2020"))

          mockDisplayReturn(cgtReference, submissionId)(Right(displayReturn))

          await(
            service.updateCorrectTaxYearToSentReturns(cgtReference, inputReturnSummary).value
          ) shouldBe Right((true, expectedReturnSummary))
        }

        "not change valid taxyear of ReturnSummary to match the disposal date's taxyear" in {

          val cgtReference                   = sample[CgtReference]
          val submissionId                   = "id"
          val taxYear                        = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2021, 4, 6),
            endDateExclusive = LocalDate.of(2022, 4, 6)
          )
          val multipleDisposalsTriageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            taxYearExchanged = Some(TaxYearExchanged(2020)),
            taxYear = taxYear
          )
          val completeReturn                 = sample[CompleteMultipleDisposalsReturn].copy(
            triageAnswers = multipleDisposalsTriageAnswers
          )
          val displayReturn                  = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val inputReturnSummary = List(
            sample[ReturnSummary].copy(
              submissionId = submissionId,
              taxYear = "2021"
            )
          )

          val expectedReturnSummary = inputReturnSummary

          mockDisplayReturn(cgtReference, submissionId)(Right(displayReturn))

          await(
            service.updateCorrectTaxYearToSentReturns(cgtReference, inputReturnSummary).value
          ) shouldBe Right((false, expectedReturnSummary))
        }

      }

    }

    "handling requests to updateSAStatusToSentReturn" must {

      "multiple disposals journey" when {

        "update SA status for previous tax year" in {
          val submissionDate                 = LocalDate.of(2021, 4, 10)
          val taxYear                        = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2020, 4, 6),
            endDateExclusive = LocalDate.of(2021, 4, 6)
          )
          val multipleDisposalsTriageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            taxYearExchanged = Some(TaxYearExchanged(2020)),
            taxYear = taxYear,
            alreadySentSelfAssessment = None
          )
          val completeReturn                 = sample[CompleteMultipleDisposalsReturn].copy(
            triageAnswers = multipleDisposalsTriageAnswers
          )
          val displayReturn                  = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val result = service.updateSAStatusToSentReturn(submissionDate, displayReturn)

          result.completeReturn.fold(
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment
          ) shouldBe Some(false)
        }

        "not update SA status for current tax year" in {
          val submissionDate = LocalDate.of(2021, 8, 6)
          val taxYear        = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2021, 4, 6),
            endDateExclusive = LocalDate.of(2022, 4, 6)
          )

          val multipleDisposalsTriageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            taxYearExchanged = Some(TaxYearExchanged(2021)),
            taxYear = taxYear,
            alreadySentSelfAssessment = None
          )
          val completeReturn                 = sample[CompleteMultipleDisposalsReturn].copy(
            triageAnswers = multipleDisposalsTriageAnswers
          )
          val displayReturn                  = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val result = service.updateSAStatusToSentReturn(submissionDate, displayReturn)

          result.completeReturn.fold(
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment
          ) shouldBe None
        }

      }

      "single disposal journey" when {

        "update SA status for previous tax year" in {
          val submissionDate              = LocalDate.of(2021, 4, 10)
          val taxYear                     = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2020, 4, 6),
            endDateExclusive = LocalDate.of(2021, 4, 6)
          )
          val disposalDate                = DisposalDate(LocalDate.of(2021, 4, 1), taxYear)
          val singleDisposalTriageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            disposalDate = disposalDate,
            alreadySentSelfAssessment = None
          )
          val completeReturn              = sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = singleDisposalTriageAnswers
          )
          val displayReturn               = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val result = service.updateSAStatusToSentReturn(submissionDate, displayReturn)

          result.completeReturn.fold(
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment
          ) shouldBe Some(false)
        }

        "not update SA status for current tax year" in {
          val submissionDate              = LocalDate.of(2021, 8, 6)
          val taxYear                     = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2021, 4, 6),
            endDateExclusive = LocalDate.of(2022, 4, 6)
          )
          val disposalDate                = DisposalDate(LocalDate.of(2021, 6, 21), taxYear)
          val completionDate              = CompletionDate(submissionDate.minusDays(10))
          val singleDisposalTriageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            disposalDate = disposalDate,
            completionDate = completionDate,
            alreadySentSelfAssessment = None
          )
          val completeReturn              = sample[CompleteSingleDisposalReturn].copy(
            triageAnswers = singleDisposalTriageAnswers
          )
          val displayReturn               = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val result = service.updateSAStatusToSentReturn(submissionDate, displayReturn)

          result.completeReturn.fold(
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment
          ) shouldBe None
        }

      }

      "single indirect disposal journey" when {

        "update SA status for previous tax year" in {
          val submissionDate              = LocalDate.of(2021, 4, 10)
          val taxYear                     = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2020, 4, 6),
            endDateExclusive = LocalDate.of(2021, 4, 6)
          )
          val disposalDate                = DisposalDate(LocalDate.of(2021, 4, 1), taxYear)
          val singleDisposalTriageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            assetType = AssetType.IndirectDisposal,
            disposalDate = disposalDate,
            alreadySentSelfAssessment = None
          )
          val completeReturn              = sample[CompleteSingleIndirectDisposalReturn].copy(
            triageAnswers = singleDisposalTriageAnswers
          )
          val displayReturn               = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val result = service.updateSAStatusToSentReturn(submissionDate, displayReturn)

          result.completeReturn.fold(
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment
          ) shouldBe Some(false)
        }

        "not update SA status for current tax year" in {
          val submissionDate              = LocalDate.of(2021, 8, 6)
          val taxYear                     = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2021, 4, 6),
            endDateExclusive = LocalDate.of(2022, 4, 6)
          )
          val disposalDate                = DisposalDate(LocalDate.of(2021, 6, 21), taxYear)
          val completionDate              = CompletionDate(submissionDate.minusDays(10))
          val singleDisposalTriageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            assetType = AssetType.IndirectDisposal,
            disposalDate = disposalDate,
            completionDate = completionDate,
            alreadySentSelfAssessment = None
          )
          val completeReturn              = sample[CompleteSingleIndirectDisposalReturn].copy(
            triageAnswers = singleDisposalTriageAnswers
          )
          val displayReturn               = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val result = service.updateSAStatusToSentReturn(submissionDate, displayReturn)

          result.completeReturn.fold(
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment
          ) shouldBe None
        }

      }

      "multiple indirect disposals journey" when {

        "update SA status for previous tax year" in {
          val submissionDate                 = LocalDate.of(2021, 4, 10)
          val taxYear                        = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2020, 4, 6),
            endDateExclusive = LocalDate.of(2021, 4, 6)
          )
          val multipleDisposalsTriageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            assetTypes = List(AssetType.IndirectDisposal),
            taxYearExchanged = Some(TaxYearExchanged(2020)),
            taxYear = taxYear,
            alreadySentSelfAssessment = None
          )
          val completeReturn                 = sample[CompleteMultipleIndirectDisposalReturn].copy(
            triageAnswers = multipleDisposalsTriageAnswers
          )
          val displayReturn                  = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val result = service.updateSAStatusToSentReturn(submissionDate, displayReturn)

          result.completeReturn.fold(
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment
          ) shouldBe Some(false)
        }

        "not update SA status for current tax year" in {
          val submissionDate = LocalDate.of(2021, 8, 6)
          val taxYear        = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2021, 4, 6),
            endDateExclusive = LocalDate.of(2022, 4, 6)
          )

          val multipleDisposalsTriageAnswers = sample[CompleteMultipleDisposalsTriageAnswers].copy(
            assetTypes = List(AssetType.IndirectDisposal),
            taxYearExchanged = Some(TaxYearExchanged(2021)),
            taxYear = taxYear,
            alreadySentSelfAssessment = None
          )
          val completeReturn                 = sample[CompleteMultipleIndirectDisposalReturn].copy(
            triageAnswers = multipleDisposalsTriageAnswers
          )
          val displayReturn                  = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val result = service.updateSAStatusToSentReturn(submissionDate, displayReturn)

          result.completeReturn.fold(
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment
          ) shouldBe None
        }

      }

      "single mixeduse disposal journey" when {

        "update SA status for previous tax year" in {
          val submissionDate              = LocalDate.of(2021, 4, 10)
          val taxYear                     = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2020, 4, 6),
            endDateExclusive = LocalDate.of(2021, 4, 6)
          )
          val disposalDate                = DisposalDate(LocalDate.of(2021, 4, 1), taxYear)
          val singleDisposalTriageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            assetType = AssetType.MixedUse,
            disposalDate = disposalDate,
            alreadySentSelfAssessment = None
          )
          val completeReturn              = sample[CompleteSingleMixedUseDisposalReturn].copy(
            triageAnswers = singleDisposalTriageAnswers
          )
          val displayReturn               = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val result = service.updateSAStatusToSentReturn(submissionDate, displayReturn)

          result.completeReturn.fold(
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment
          ) shouldBe Some(false)
        }

        "not update SA status for current tax year" in {
          val submissionDate              = LocalDate.of(2021, 8, 6)
          val taxYear                     = sample[TaxYear].copy(
            startDateInclusive = LocalDate.of(2021, 4, 6),
            endDateExclusive = LocalDate.of(2022, 4, 6)
          )
          val disposalDate                = DisposalDate(LocalDate.of(2021, 6, 21), taxYear)
          val completionDate              = CompletionDate(submissionDate.minusDays(10))
          val singleDisposalTriageAnswers = sample[CompleteSingleDisposalTriageAnswers].copy(
            assetType = AssetType.MixedUse,
            disposalDate = disposalDate,
            completionDate = completionDate,
            alreadySentSelfAssessment = None
          )
          val completeReturn              = sample[CompleteSingleMixedUseDisposalReturn].copy(
            triageAnswers = singleDisposalTriageAnswers
          )
          val displayReturn               = DisplayReturn(completeReturn, ReturnType.FirstReturn)

          val result = service.updateSAStatusToSentReturn(submissionDate, displayReturn)

          result.completeReturn.fold(
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment,
            _.triageAnswers.alreadySentSelfAssessment
          ) shouldBe None
        }

      }

    }

  }

}
