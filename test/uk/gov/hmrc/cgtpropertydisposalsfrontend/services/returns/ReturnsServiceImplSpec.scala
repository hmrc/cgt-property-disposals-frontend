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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns

import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util.UUID

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsNumber, JsString, Json}
import play.api.mvc.Request
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.connectors.returns.ReturnsConnector
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.Generators._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.UkAddress
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Postcode
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.finance._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.CgtReference
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.CompleteReturn.CompleteSingleDisposalReturn
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.IndividualUserType.{PersonalRepresentative, PersonalRepresentativeInPeriodOfAdmin}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.MultipleDisposalsTriageAnswers.CompleteMultipleDisposalsTriageAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeAnswers.CompleteRepresenteeAnswers
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SingleDisposalTriageAnswers.{CompleteSingleDisposalTriageAnswers, IncompleteSingleDisposalTriageAnswers}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.SubmitReturnResponse.ReturnCharge
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, TaxYear}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.AuditService
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.returns.ReturnsServiceImpl.{GetDraftReturnResponse, ListReturnsResponse}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ReturnsServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val mockConnector = mock[ReturnsConnector]

  def mockStoreDraftReturn(
    draftReturn: DraftReturn,
    cgtReference: CgtReference
  )(
    response: Either[Error, HttpResponse]
  ) =
    (mockConnector
      .storeDraftReturn(_: DraftReturn, _: CgtReference)(_: HeaderCarrier))
      .expects(draftReturn, cgtReference, *)
      .returning(EitherT.fromEither[Future](response))

  def mockGetDraftReturns(
    cgtReference: CgtReference
  )(response: Either[Error, HttpResponse]) =
    (mockConnector
      .getDraftReturns(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(EitherT.fromEither[Future](response))

  def mockSubmitReturn(
    submitReturnRequest: SubmitReturnRequest
  )(response: Either[Error, HttpResponse]) =
    (mockConnector
      .submitReturn(_: SubmitReturnRequest)(_: HeaderCarrier))
      .expects(submitReturnRequest, *)
      .returning(EitherT.fromEither[Future](response))

  def mockListReturn(
    cgtReference: CgtReference,
    fromDate: LocalDate,
    toDate: LocalDate
  )(
    response: Either[Error, HttpResponse]
  ) =
    (mockConnector
      .listReturns(_: CgtReference, _: LocalDate, _: LocalDate)(
        _: HeaderCarrier
      ))
      .expects(cgtReference, fromDate, toDate, *)
      .returning(EitherT.fromEither[Future](response))

  def mockDisplayReturn(cgtReference: CgtReference, submissionId: String)(
    response: Either[Error, HttpResponse]
  ) =
    (mockConnector
      .displayReturn(_: CgtReference, _: String)(_: HeaderCarrier))
      .expects(cgtReference, submissionId, *)
      .returning(EitherT.fromEither[Future](response))

  def mockDeleteDraftReturns(
    draftReturnIds: List[UUID]
  )(response: Either[Error, HttpResponse]) =
    (mockConnector
      .deleteDraftReturns(_: List[UUID])(_: HeaderCarrier))
      .expects(draftReturnIds, *)
      .returning(EitherT.fromEither[Future](response))

  val service = new ReturnsServiceImpl(mockConnector, stub[AuditService])

  implicit val hc: HeaderCarrier   = HeaderCarrier()
  implicit val request: Request[_] = FakeRequest()
  private val emptyJsonBody        = "{}"

  "ReturnsServiceImpl" when {

    "handling requests to store draft returns" must {

      val draftReturn  = sample[DraftSingleDisposalReturn]
      val cgtReference = sample[CgtReference]

      "return an error" when {

        "the http call fails" in {
          mockStoreDraftReturn(draftReturn, cgtReference)(Left(Error("")))

          await(
            service.storeDraftReturn(draftReturn, cgtReference, None).value
          ).isLeft shouldBe true
        }

        "the http call came back with a status other than 200" in {
          mockStoreDraftReturn(draftReturn, cgtReference)(
            Right(HttpResponse(INTERNAL_SERVER_ERROR, emptyJsonBody))
          )

          await(
            service.storeDraftReturn(draftReturn, cgtReference, None).value
          ).isLeft shouldBe true
        }

      }

      "return an ok response" when {

        "the http call came back with a 200" in {
          mockStoreDraftReturn(draftReturn, cgtReference)(
            Right(HttpResponse(OK, emptyJsonBody))
          )

          await(
            service.storeDraftReturn(draftReturn, cgtReference, None).value
          ) shouldBe Right(())
        }

      }

    }

    "handling requests to get draft returns" must {

      val cgtReference = sample[CgtReference]

      val sentReturns = List(sample[ReturnSummary])

      "return an error" when {

        "the http response does not come back with status 200" in {
          mockGetDraftReturns(cgtReference)(
            Right(HttpResponse(INTERNAL_SERVER_ERROR, emptyJsonBody))
          )

          await(
            service.getDraftReturns(cgtReference, sentReturns).value
          ).isLeft shouldBe true
        }

        "the http response comes back with status 200 but the body cannot be parsed" in {
          mockGetDraftReturns(cgtReference)(Left(Error(emptyJsonBody)))

          await(
            service.getDraftReturns(cgtReference, sentReturns).value
          ).isLeft shouldBe true
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

          mockGetDraftReturns(cgtReference)(
            Right(HttpResponse(OK, Json.toJson(draftReturnsResponse), Map[String, Seq[String]]().empty))
          )

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
                mockGetDraftReturns(cgtReference)(
                  Right(
                    HttpResponse(OK, Json.toJson(draftReturnsResponse), Map[String, Seq[String]]().empty)
                  )
                )
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
                mockGetDraftReturns(cgtReference)(
                  Right(
                    HttpResponse(OK, Json.toJson(draftReturnsResponse), Map[String, Seq[String]]().empty)
                  )
                )
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
              mockGetDraftReturns(cgtReference)(
                Right(
                  HttpResponse(OK, Json.toJson(draftReturnsResponse), Map[String, Seq[String]]().empty)
                )
              )
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

            val singleDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                disposalDate = DisposalDate(dateOfDeath, sample[TaxYear])
              )

            val multipleDisposalsTriageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                completionDate = CompletionDate(dateOfDeath)
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
                  triageAnswers = multipleDisposalsTriageAnswers,
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

            "the user is on a multiple indirect disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
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

            val representeeAnswers =
              sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(dateOfDeath)))

            val singleDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentative),
                disposalDate = DisposalDate(dateOfDeath.plusDays(1L), sample[TaxYear])
              )

            val multipleDisposalsTriageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(
                  individualUserType = Some(PersonalRepresentative),
                  completionDate = CompletionDate(dateOfDeath.plusDays(1L))
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
                  triageAnswers = multipleDisposalsTriageAnswers,
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

            "the user is on a multiple indirect disposal journey" in {
              testDraftReturnIsDeleted(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
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
              mockGetDraftReturns(cgtReference)(
                Right(
                  HttpResponse(OK, Json.toJson(draftReturnsResponse), Map[String, Seq[String]]().empty)
                )
              )
            }

            await(
              service.getDraftReturns(cgtReference, List.empty).value
            ) shouldBe Right(List(draftReturn))
          }

          "the user is a period of admin personal rep and" when {

            val dateOfDeath = LocalDate.now()

            val representeeAnswers =
              sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(dateOfDeath)))

            val singleDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
                disposalDate = DisposalDate(dateOfDeath.plusDays(1L), sample[TaxYear])
              )

            val multipleDisposalsTriageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentativeInPeriodOfAdmin),
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

            "the user is on a multiple disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
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

            "the user is on a multiple indirect disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
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

            val dateOfDeath = LocalDate.now()

            val representeeAnswers =
              sample[CompleteRepresenteeAnswers].copy(dateOfDeath = Some(DateOfDeath(dateOfDeath)))

            val singleDisposalTriageAnswers =
              sample[CompleteSingleDisposalTriageAnswers].copy(
                individualUserType = Some(PersonalRepresentative),
                disposalDate = DisposalDate(dateOfDeath, sample[TaxYear])
              )

            val multipleDisposalsTriageAnswers =
              sample[CompleteMultipleDisposalsTriageAnswers]
                .copy(individualUserType = Some(PersonalRepresentative), completionDate = CompletionDate(dateOfDeath))

            "the user is on a single disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftSingleDisposalReturn].copy(
                  triageAnswers = singleDisposalTriageAnswers,
                  representeeAnswers = Some(representeeAnswers)
                )
              )
            }

            "the user is on a multiple disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftMultipleDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
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

            "the user is on a multiple indirect disposal journey" in {
              testDraftReturnIsNotDeleted(
                sample[DraftMultipleIndirectDisposalsReturn].copy(
                  triageAnswers = multipleDisposalsTriageAnswers,
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

    "handling requests to submit a return" must {

      val submitReturnRequest = sample[SubmitReturnRequest]

      "return an error" when {

        def test(response: Either[Error, HttpResponse]) = {
          mockSubmitReturn(submitReturnRequest)(response)

          await(
            service.submitReturn(submitReturnRequest).value
          ).isLeft shouldBe true
        }

        "the http call fails" in {
          test(Left(Error("")))
        }

        "the http call came back with a status other than 200" in {
          test(Right(HttpResponse(INTERNAL_SERVER_ERROR, emptyJsonBody)))
        }

        "the http call came back with status 200 but there is no JSON body" in {
          test(Right(HttpResponse(OK, emptyJsonBody)))
        }

        "the http call came back with status 200 and JSON body, but the JSON cannot be parsed" in {
          test(Right(HttpResponse(OK, JsNumber(1), Map[String, Seq[String]]().empty)))
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
              )
            )

          mockSubmitReturn(submitReturnRequest)(
            Right(
              HttpResponse(
                OK,
                Json.parse("""
                |{
                |  "formBundleId": "bundleId",
                |  "processingDate": "2000-01-01T01:01",
                |  "charge" : {
                |    "chargeReference": "charge",
                |    "amount": 123,
                |    "dueDate": "2000-01-01"
                |  }
                |}
                |""".stripMargin),
                Map[String, Seq[String]]().empty
              )
            )
          )

          await(service.submitReturn(submitReturnRequest).value) shouldBe Right(
            response
          )
        }

        "the http call came back with a 200 and the JSON is valid and does not contain a charge " in {
          val response =
            SubmitReturnResponse(
              "bundleId",
              LocalDateTime.of(LocalDate.of(2000, 1, 1), LocalTime.of(1, 1)),
              None
            )

          mockSubmitReturn(submitReturnRequest)(
            Right(
              HttpResponse(
                OK,
                Json.parse("""{
                |  "formBundleId": "bundleId",
                |  "processingDate": "2000-01-01T01:01"
                |}""".stripMargin),
                Map[String, Seq[String]]().empty
              )
            )
          )

          await(service.submitReturn(submitReturnRequest).value) shouldBe Right(
            response
          )
        }

      }

    }

    "handling requests to list returns" must {

      val cgtReference       = sample[CgtReference]
      val (fromDate, toDate) = TaxYear
        .thisTaxYearStartDate() -> TaxYear
        .thisTaxYearStartDate()
        .plusYears(1L)
        .minusDays(1L)

      "return an error " when {

        "the http call fails" in {
          mockListReturn(cgtReference, fromDate, toDate)(Left(Error("")))

          await(service.listReturns(cgtReference).value).isLeft shouldBe true
        }

        "the http call returns with a status which is not 200" in {
          mockListReturn(cgtReference, fromDate, toDate)(
            Right(HttpResponse(500, emptyJsonBody))
          )

          await(service.listReturns(cgtReference).value).isLeft shouldBe true
        }

        "the response body cannot be parsed" in {
          mockListReturn(cgtReference, fromDate, toDate)(
            Right(HttpResponse(200, JsString("Hi!"), Map[String, Seq[String]]().empty))
          )

          await(service.listReturns(cgtReference).value).isLeft shouldBe true
        }

      }

      "return a list of returns" when {

        "the response body can be parsed and converted" in {
          val returnSummary = sample[ReturnSummary]
          val response      = ListReturnsResponse(List(returnSummary))

          mockListReturn(cgtReference, fromDate, toDate)(
            Right(HttpResponse(200, Json.toJson(response), Map[String, Seq[String]]().empty))
          )

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

        "the http call fails" in {
          mockDisplayReturn(cgtReference, submissionId)(Left(Error("")))

          await(
            service.displayReturn(cgtReference, submissionId).value
          ).isLeft shouldBe true
        }

        "the http call returns with a status which is not 200" in {
          mockDisplayReturn(cgtReference, submissionId)(
            Right(HttpResponse(500, emptyJsonBody))
          )

          await(
            service.displayReturn(cgtReference, submissionId).value
          ).isLeft shouldBe true
        }

        "there is no response body" in {
          mockDisplayReturn(cgtReference, submissionId)(
            Right(HttpResponse(200, emptyJsonBody))
          )

          await(
            service.displayReturn(cgtReference, submissionId).value
          ).isLeft shouldBe true
        }

      }

      "return a list of returns" when {

        "the response body can be parsed and converted" in {
          val completeReturn: CompleteReturn =
            sample[CompleteSingleDisposalReturn]
          mockDisplayReturn(cgtReference, submissionId)(
            Right(HttpResponse(200, Json.toJson(completeReturn), Map[String, Seq[String]]().empty))
          )

          await(
            service.displayReturn(cgtReference, submissionId).value
          ) shouldBe Right(completeReturn)
        }

      }

    }

  }

}
