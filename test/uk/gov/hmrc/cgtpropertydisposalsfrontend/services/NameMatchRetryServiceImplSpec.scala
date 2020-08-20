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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import cats.data.EitherT
import cats.instances.future._
import com.typesafe.config.ConfigFactory
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.Configuration
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Request
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, NameMatchServiceError, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.BusinessPartnerRecordGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameMatchGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.BusinessPartnerRecordNameMatchAuditDetails.{IndividualNameWithNinoAuditDetails, IndividualNameWithSaUtrAuditDetails, TrustNameWithTrnAuditDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{BusinessPartnerRecordNameMatchAttemptEvent, BusinessPartnerRecordNameMatchAuditDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest.{IndividualBusinessPartnerRecordRequest, TrustBusinessPartnerRecordRequest}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails.{IndividualRepresenteeNameMatchDetails, IndividualSautrNameMatchDetails, TrustNameMatchDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{NoReferenceId, RepresenteeCgtReference, RepresenteeNino, RepresenteeSautr}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.audit.CgtAccountNameMatchAttemptEvent
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.NameMatchRetryStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.{BusinessPartnerRecordService, SubscriptionService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect._

class NameMatchRetryServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val bprService: BusinessPartnerRecordService =
    mock[BusinessPartnerRecordService]

  val retryStore: NameMatchRetryStore = mock[NameMatchRetryStore]

  val mockAuditService: AuditService = mock[AuditService]

  val mockSubscriptionService: SubscriptionService = mock[SubscriptionService]

  val maxRetries: Int = 3

  val config = Configuration(
    ConfigFactory.parseString(
      s"""
        |bpr-name-match.max-retries = $maxRetries
        |""".stripMargin
    )
  )

  val service = new NameMatchRetryServiceImpl(
    bprService,
    retryStore,
    mockSubscriptionService,
    mockAuditService,
    config
  )

  def mockSendBprNameMatchAttemptEvent(
    attemptsMade: Int,
    maxAttemptsMade: Int,
    nameMatchDetails: BusinessPartnerRecordNameMatchAuditDetails
  )(result: Unit) =
    (
      mockAuditService
        .sendEvent(
          _: String,
          _: BusinessPartnerRecordNameMatchAttemptEvent,
          _: String
        )(
          _: ExecutionContext,
          _: HeaderCarrier,
          _: Writes[BusinessPartnerRecordNameMatchAttemptEvent],
          _: Request[_]
        )
      )
      .expects(
        "businessPartnerRecordNameMatchAttempt",
        BusinessPartnerRecordNameMatchAttemptEvent(
          attemptsMade,
          maxAttemptsMade,
          nameMatchDetails
        ),
        "business-partner-record-name-match-attempt",
        *,
        *,
        *,
        *
      )
      .returning(result)

  def mockSendCgtAccoutnNameMatchAttemptEvent(
    attemptsMade: Int,
    maxAttemptsMade: Int,
    name: IndividualName,
    cgtReference: CgtReference
  ) =
    (
      mockAuditService
        .sendEvent(_: String, _: CgtAccountNameMatchAttemptEvent, _: String)(
          _: ExecutionContext,
          _: HeaderCarrier,
          _: Writes[CgtAccountNameMatchAttemptEvent],
          _: Request[_]
        )
      )
      .expects(
        "cgtAccountNameMatchAttempt",
        CgtAccountNameMatchAttemptEvent(
          attemptsMade,
          maxAttemptsMade,
          name.firstName,
          name.lastName,
          cgtReference.value
        ),
        "cgt-account-name-match-attempt",
        *,
        *,
        *,
        *
      )
      .returning(())

  def mockGetNumberOfUnsccessfulAttempts[A <: NameMatchDetails](
    expectedGGCredID: GGCredId
  )(result: Either[Error, Option[UnsuccessfulNameMatchAttempts[A]]]) =
    (retryStore
      .get[A](_: GGCredId)(_: Reads[A]))
      .expects(expectedGGCredID, *)
      .returning(Future.successful(result))

  def mockStoreNumberOfUnsccessfulAttempts[A <: NameMatchDetails](
    expectedGGCredID: GGCredId,
    unsuccessfulNameMatchAttempts: UnsuccessfulNameMatchAttempts[A]
  )(result: Either[Error, Unit]) =
    (retryStore
      .store[A](_: GGCredId, _: UnsuccessfulNameMatchAttempts[A])(_: Writes[A]))
      .expects(expectedGGCredID, unsuccessfulNameMatchAttempts, *)
      .returning(Future.successful(result))

  def mockGetIndividualBpr(
    expectedId: Either[SAUTR, NINO],
    expectedName: IndividualName
  )(
    result: Either[Error, BusinessPartnerRecordResponse]
  ) =
    (bprService
      .getBusinessPartnerRecord(_: BusinessPartnerRecordRequest)(
        _: HeaderCarrier
      ))
      .expects(
        IndividualBusinessPartnerRecordRequest(expectedId, Some(expectedName)),
        *
      )
      .returning(EitherT.fromEither[Future](result))

  def mockGetTrustBpr(expectedTrn: TRN, expectedName: TrustName)(
    result: Either[Error, BusinessPartnerRecordResponse]
  ) =
    (bprService
      .getBusinessPartnerRecord(_: BusinessPartnerRecordRequest)(
        _: HeaderCarrier
      ))
      .expects(
        TrustBusinessPartnerRecordRequest(
          Left(expectedTrn),
          Some(expectedName)
        ),
        *
      )
      .returning(EitherT.fromEither[Future](result))

  def mockGetSubscriptionDetails(
    cgtReference: CgtReference,
    expectedSubscribedDetails: Either[Error, Option[SubscribedDetails]]
  ): Unit =
    (mockSubscriptionService
      .getSubscribedDetails(_: CgtReference)(_: HeaderCarrier))
      .expects(cgtReference, *)
      .returning(
        EitherT.fromEither[Future](expectedSubscribedDetails)
      )

  "NameMatchRetryServiceImpl" when {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    implicit val request: Request[_] = FakeRequest()

    "getting the number of previous unsuccessful attempts" must {

      val ggCredId = sample[GGCredId]

      "return an error" when {

        "there is an error reading the value" in {
          mockGetNumberOfUnsccessfulAttempts[IndividualSautrNameMatchDetails](
            ggCredId
          )(Left(Error("")))

          testIsErrorOfType[
            IndividualSautrNameMatchDetails,
            NameMatchServiceError.BackendError
          ](
            service
              .getNumberOfUnsuccessfulAttempts[IndividualSautrNameMatchDetails](
                ggCredId
              )
          )
        }

      }

      "return the number of attempts" when {

        "a number can be found and it's less than the configured maximum" in {
          val nameMatchDetails = sample[IndividualSautrNameMatchDetails]

          mockGetNumberOfUnsccessfulAttempts(ggCredId)(
            Right(
              Some(
                UnsuccessfulNameMatchAttempts(1, maxRetries, nameMatchDetails)
              )
            )
          )

          await(
            service.getNumberOfUnsuccessfulAttempts(ggCredId).value
          ) shouldBe Right(
            Some(UnsuccessfulNameMatchAttempts(1, maxRetries, nameMatchDetails))
          )

        }

      }

      "return None" when {

        "there is no record of attempts for the given gg cred id" in {
          mockGetNumberOfUnsccessfulAttempts(ggCredId)(Right(None))

          await(
            service.getNumberOfUnsuccessfulAttempts(ggCredId).value
          ) shouldBe Right(
            None
          )
        }

      }

      "indicate when too many unsuccessful attempts have been made" when {

        "a number can be found and it's equal to the configured maximum" in {
          val nameMatchDetails = sample[TrustNameMatchDetails]

          mockGetNumberOfUnsccessfulAttempts(ggCredId)(
            Right(
              Some(
                UnsuccessfulNameMatchAttempts(
                  maxRetries,
                  maxRetries,
                  nameMatchDetails
                )
              )
            )
          )

          testIsErrorOfType[
            TrustNameMatchDetails,
            NameMatchServiceError.TooManyUnsuccessfulAttempts
          ](
            service.getNumberOfUnsuccessfulAttempts(ggCredId)
          )
        }

        "a number can be found and it's greater than the configured maximum" in {
          val nameMatchDetails = sample[IndividualRepresenteeNameMatchDetails]

          mockGetNumberOfUnsccessfulAttempts(ggCredId)(
            Right(
              Some(
                UnsuccessfulNameMatchAttempts(
                  maxRetries + 1,
                  maxRetries,
                  nameMatchDetails
                )
              )
            )
          )

          testIsErrorOfType[
            IndividualRepresenteeNameMatchDetails,
            NameMatchServiceError.TooManyUnsuccessfulAttempts
          ](
            service.getNumberOfUnsuccessfulAttempts(ggCredId)
          )
        }

      }

    }

    "attempting a name match for an individual with an SA UTR" must {

      val nameMatchDetails = sample[IndividualSautrNameMatchDetails]

      behave like commonBprNameMatchBehaviour[
        IndividualSautrNameMatchDetails,
        (BusinessPartnerRecord, Option[CgtReference])
      ](
        nameMatchDetails,
        IndividualNameWithSaUtrAuditDetails(
          nameMatchDetails.name.firstName,
          nameMatchDetails.name.lastName,
          nameMatchDetails.sautr.value
        ),
        mockGetIndividualBpr(
          Left(nameMatchDetails.sautr),
          nameMatchDetails.name
        )(_),
        service.attemptBusinessPartnerRecordNameMatch(_, _, _),
        _ -> _
      )

    }

    "attempting a name match for a trust" must {

      val nameMatchDetails = sample[TrustNameMatchDetails]

      behave like commonBprNameMatchBehaviour[
        TrustNameMatchDetails,
        (BusinessPartnerRecord, Option[CgtReference])
      ](
        nameMatchDetails,
        TrustNameWithTrnAuditDetails(
          nameMatchDetails.name.value,
          nameMatchDetails.trn.value
        ),
        mockGetTrustBpr(nameMatchDetails.trn, nameMatchDetails.name)(_),
        service.attemptBusinessPartnerRecordNameMatch(_, _, _),
        _ -> _
      )

    }

    "attempting a name match for an individual representee" when {

      "passed no reference id" must {

        "return a successful response" in {
          val result = service.attemptNameMatch(
            IndividualRepresenteeNameMatchDetails(
              sample[IndividualName],
              NoReferenceId
            ),
            sample[GGCredId],
            None
          )

          await(result.value) shouldBe Right(NoReferenceId)

        }

      }

      "passed a nino" must {

        val representeeNino  = sample[RepresenteeNino]
        val nameMatchDetails = IndividualRepresenteeNameMatchDetails(
          sample[IndividualName],
          representeeNino
        )

        behave like commonBprNameMatchBehaviour[
          IndividualRepresenteeNameMatchDetails,
          RepresenteeReferenceId
        ](
          nameMatchDetails,
          IndividualNameWithNinoAuditDetails(
            nameMatchDetails.name.firstName,
            nameMatchDetails.name.lastName,
            representeeNino.value.value
          ),
          mockGetIndividualBpr(
            Right(representeeNino.value),
            nameMatchDetails.name
          )(_),
          service.attemptNameMatch(_, _, _),
          (_, _) => representeeNino
        )

      }

      "passed an sautr" must {

        val representeeSautr = sample[RepresenteeSautr]
        val nameMatchDetails = IndividualRepresenteeNameMatchDetails(
          sample[IndividualName],
          representeeSautr
        )

        behave like commonBprNameMatchBehaviour[
          IndividualRepresenteeNameMatchDetails,
          RepresenteeReferenceId
        ](
          nameMatchDetails,
          IndividualNameWithSaUtrAuditDetails(
            nameMatchDetails.name.firstName,
            nameMatchDetails.name.lastName,
            representeeSautr.value.value
          ),
          mockGetIndividualBpr(
            Left(representeeSautr.value),
            nameMatchDetails.name
          )(_),
          service.attemptNameMatch(_, _, _),
          (_, _) => representeeSautr
        )

      }

      "passed a cgt reference" must {

        val ggCredId                = sample[GGCredId]
        val cgtReference            = sample[CgtReference]
        val representeeCgtReference = RepresenteeCgtReference(cgtReference)
        val nameMatchDetails        = IndividualRepresenteeNameMatchDetails(
          sample[IndividualName],
          representeeCgtReference
        )

        "return TooManyUnsuccessfulAttempts" when {

          "the number of previous attempts passed in equals the configured maximum" in {
            mockSendCgtAccoutnNameMatchAttemptEvent(
              maxRetries,
              maxRetries,
              nameMatchDetails.name,
              cgtReference
            )

            testIsErrorOfType[
              IndividualRepresenteeNameMatchDetails,
              NameMatchServiceError.TooManyUnsuccessfulAttempts
            ](
              service.attemptNameMatch(
                nameMatchDetails,
                ggCredId,
                Some(
                  UnsuccessfulNameMatchAttempts(
                    maxRetries,
                    maxRetries,
                    sample[IndividualRepresenteeNameMatchDetails]
                  )
                )
              )
            )
          }

          "the number of previous attempts passed in exceeds the configured maximum" in {
            mockSendCgtAccoutnNameMatchAttemptEvent(
              maxRetries + 1,
              maxRetries,
              nameMatchDetails.name,
              cgtReference
            )

            testIsErrorOfType[
              IndividualRepresenteeNameMatchDetails,
              NameMatchServiceError.TooManyUnsuccessfulAttempts
            ](
              service.attemptNameMatch(
                nameMatchDetails,
                ggCredId,
                Some(
                  UnsuccessfulNameMatchAttempts(
                    maxRetries + 1,
                    maxRetries,
                    sample[IndividualRepresenteeNameMatchDetails]
                  )
                )
              )
            )
          }

          "the user has now exceed the configured maximum number of unsuccessful attempts" when {
            def test(subscribedDetails: Option[SubscribedDetails]): Unit = {
              inSequence {
                mockSendCgtAccoutnNameMatchAttemptEvent(
                  maxRetries,
                  maxRetries,
                  nameMatchDetails.name,
                  cgtReference
                )
                mockGetSubscriptionDetails(
                  cgtReference,
                  Right(subscribedDetails)
                )
                mockStoreNumberOfUnsccessfulAttempts(
                  ggCredId,
                  UnsuccessfulNameMatchAttempts(
                    maxRetries,
                    maxRetries,
                    nameMatchDetails
                  )
                )(Right(()))
              }

              testIsErrorOfType[
                IndividualRepresenteeNameMatchDetails,
                NameMatchServiceError.TooManyUnsuccessfulAttempts
              ](
                service.attemptNameMatch(
                  nameMatchDetails,
                  ggCredId,
                  Some(
                    UnsuccessfulNameMatchAttempts(
                      maxRetries - 1,
                      maxRetries,
                      sample[IndividualRepresenteeNameMatchDetails]
                    )
                  )
                )
              )
            }

            "an account was not found" in {
              test(None)
            }

            "an account was found but the name didn't match" in {
              test(
                Some(
                  sample[SubscribedDetails].copy(
                    name = Right(
                      sample[IndividualName].copy(
                        firstName = s"${nameMatchDetails.name.firstName}abc"
                      )
                    )
                  )
                )
              )
            }

            "an account was found but the account name was for a trust" in {
              test(
                Some(
                  sample[SubscribedDetails].copy(name = Left(sample[TrustName]))
                )
              )
            }
          }

        }

        "return a BackendError" when {

          "there is an error getting the subscription details" in {
            inSequence {
              mockSendCgtAccoutnNameMatchAttemptEvent(
                1,
                maxRetries,
                nameMatchDetails.name,
                cgtReference
              )
              mockGetSubscriptionDetails(cgtReference, Left(Error("")))
            }

            testIsErrorOfType[
              IndividualRepresenteeNameMatchDetails,
              NameMatchServiceError.BackendError
            ](
              service.attemptNameMatch(
                nameMatchDetails,
                ggCredId,
                None
              )
            )
          }

          "there is an error updating the number of unsuccessful attempts in the retry store" in {
            inSequence {
              mockSendCgtAccoutnNameMatchAttemptEvent(
                1,
                maxRetries,
                nameMatchDetails.name,
                cgtReference
              )
              mockGetSubscriptionDetails(cgtReference, Right(None))
              mockStoreNumberOfUnsccessfulAttempts(
                ggCredId,
                UnsuccessfulNameMatchAttempts(1, maxRetries, nameMatchDetails)
              )(
                Left(Error(""))
              )
            }

            testIsErrorOfType[
              IndividualRepresenteeNameMatchDetails,
              NameMatchServiceError.BackendError
            ](
              service.attemptNameMatch(
                nameMatchDetails,
                ggCredId,
                None
              )
            )
          }

        }

        "return NameMatchFailed" when {

          def test(subscribedDetails: Option[SubscribedDetails]): Unit = {
            inSequence {
              mockSendCgtAccoutnNameMatchAttemptEvent(
                2,
                maxRetries,
                nameMatchDetails.name,
                cgtReference
              )
              mockGetSubscriptionDetails(cgtReference, Right(subscribedDetails))
              mockStoreNumberOfUnsccessfulAttempts(
                ggCredId,
                UnsuccessfulNameMatchAttempts(2, maxRetries, nameMatchDetails)
              )(
                Right(())
              )
            }

            testIsErrorOfType[
              IndividualRepresenteeNameMatchDetails,
              NameMatchServiceError.NameMatchFailed[
                IndividualRepresenteeNameMatchDetails
              ]
            ](
              service.attemptNameMatch(
                nameMatchDetails,
                ggCredId,
                Some(
                  UnsuccessfulNameMatchAttempts(
                    1,
                    maxRetries,
                    sample[IndividualRepresenteeNameMatchDetails]
                  )
                )
              )
            )
          }

          "the user has not exceeded the configured maximum number of unsuccessful attempts" when {

            "a cgt account was not found" in {
              test(None)
            }

            "an account was found but the name didn't match" in {
              test(
                Some(
                  sample[SubscribedDetails].copy(
                    name = Right(
                      sample[IndividualName].copy(
                        firstName = s"${nameMatchDetails.name.firstName}abc"
                      )
                    )
                  )
                )
              )
            }

            "an account was found but the account name was for a trust" in {
              test(
                Some(
                  sample[SubscribedDetails].copy(name = Left(sample[TrustName]))
                )
              )
            }
          }

          "the name match details passed in are the same as the previous attempt" in {
            mockSendCgtAccoutnNameMatchAttemptEvent(
              1,
              maxRetries,
              nameMatchDetails.name,
              cgtReference
            )

            testIsErrorOfType[
              IndividualRepresenteeNameMatchDetails,
              NameMatchServiceError.NameMatchFailed[
                IndividualRepresenteeNameMatchDetails
              ]
            ](
              service.attemptNameMatch(
                nameMatchDetails,
                ggCredId,
                Some(
                  UnsuccessfulNameMatchAttempts(1, maxRetries, nameMatchDetails)
                )
              )
            )

          }

        }

        "return a successful response" when {

          "the subscription details are found and the name matches" in {
            val subscribedDetails = sample[SubscribedDetails]
              .copy(name = Right(nameMatchDetails.name))

            inSequence {
              mockSendCgtAccoutnNameMatchAttemptEvent(
                1,
                maxRetries,
                nameMatchDetails.name,
                cgtReference
              )
              mockGetSubscriptionDetails(
                cgtReference,
                Right(Some(subscribedDetails))
              )
            }

            val result =
              service.attemptNameMatch(nameMatchDetails, ggCredId, None)
            await(result.value) shouldBe Right(representeeCgtReference)
          }

        }

      }

    }

  }

  private def testIsErrorOfType[
    A <: NameMatchDetails,
    E <: NameMatchServiceError[A] : ClassTag
  ](
    result: EitherT[Future, NameMatchServiceError[A], _]
  ) =
    await(result.value) match {
      case Left(_: E) => ()
      case other      =>
        fail(s"expected error of type ${classTag[E]} but found $other")
    }

  private def commonBprNameMatchBehaviour[A <: NameMatchDetails : Gen, B](
    sampleNameMatchDetails: A,
    auditEvent: BusinessPartnerRecordNameMatchAuditDetails,
    mockGetBpr: Either[Error, BusinessPartnerRecordResponse] => Unit,
    toResult: (
      A,
      GGCredId,
      Option[UnsuccessfulNameMatchAttempts[A]]
    ) => EitherT[Future, NameMatchServiceError[A], B],
    toSuccessfulResponse: (BusinessPartnerRecord, Option[CgtReference]) => B
  ) = {
    val ggCredId = sample[GGCredId]

    "return TooManyUnsuccessfulAttempts" when {

      "the number of previous attempts passed in equals the configured maximum" in {
        inSequence {
          mockSendBprNameMatchAttemptEvent(
            maxRetries,
            maxRetries,
            auditEvent
          )(())
        }
        testIsErrorOfType[A, NameMatchServiceError.TooManyUnsuccessfulAttempts](
          toResult(
            sampleNameMatchDetails,
            ggCredId,
            Some(
              UnsuccessfulNameMatchAttempts(maxRetries, maxRetries, sample[A])
            )
          )
        )
      }

      "the number of previous attempts passed in exceeds the configured maximum" in {
        inSequence {
          mockSendBprNameMatchAttemptEvent(
            maxRetries,
            maxRetries,
            auditEvent
          )(())
        }

        testIsErrorOfType[A, NameMatchServiceError.TooManyUnsuccessfulAttempts](
          toResult(
            sampleNameMatchDetails,
            ggCredId,
            Some(
              UnsuccessfulNameMatchAttempts(maxRetries, maxRetries, sample[A])
            )
          )
        )
      }

      "a BPR was not found and the user has now exceeded the configured maximum number of " +
        "unsuccessful attempts" in {
        inSequence {
          mockSendBprNameMatchAttemptEvent(
            maxRetries,
            maxRetries,
            auditEvent
          )(())
          mockGetBpr(Right(BusinessPartnerRecordResponse(None, None)))
          mockStoreNumberOfUnsccessfulAttempts(
            ggCredId,
            UnsuccessfulNameMatchAttempts(
              maxRetries,
              maxRetries,
              sampleNameMatchDetails
            )
          )(Right(()))
        }

        testIsErrorOfType[A, NameMatchServiceError.TooManyUnsuccessfulAttempts](
          toResult(
            sampleNameMatchDetails,
            ggCredId,
            Some(
              UnsuccessfulNameMatchAttempts(
                maxRetries - 1,
                maxRetries,
                sample[A]
              )
            )
          )
        )
      }

    }

    "return a BackendError" when {

      "there is an error getting the BPR" in {
        inSequence {
          mockSendBprNameMatchAttemptEvent(
            1,
            maxRetries,
            auditEvent
          )(())
          mockGetBpr(Left(Error("")))
        }

        testIsErrorOfType[A, NameMatchServiceError.BackendError](
          toResult(
            sampleNameMatchDetails,
            ggCredId,
            None
          )
        )
      }

      "there is an error updating the number of unsuccessful attempts in the retry store" in {
        inSequence {
          mockSendBprNameMatchAttemptEvent(
            1,
            maxRetries,
            auditEvent
          )(())
          mockGetBpr(Right(BusinessPartnerRecordResponse(None, None)))
          mockStoreNumberOfUnsccessfulAttempts(
            ggCredId,
            UnsuccessfulNameMatchAttempts(1, maxRetries, sampleNameMatchDetails)
          )(
            Left(Error(""))
          )
        }

        testIsErrorOfType[A, NameMatchServiceError.BackendError](
          toResult(
            sampleNameMatchDetails,
            ggCredId,
            None
          )
        )
      }

    }

    "return NameMatchFailed" when {

      "a BPR was not found and the user has not exceeded the configured maximum number of " +
        "unsuccessful attempts" in {
        inSequence {
          mockSendBprNameMatchAttemptEvent(
            2,
            maxRetries,
            auditEvent
          )(())

          mockGetBpr(Right(BusinessPartnerRecordResponse(None, None)))

          mockStoreNumberOfUnsccessfulAttempts(
            ggCredId,
            UnsuccessfulNameMatchAttempts(2, maxRetries, sampleNameMatchDetails)
          )(
            Right(())
          )
        }

        testIsErrorOfType[A, NameMatchServiceError.NameMatchFailed[A]](
          toResult(
            sampleNameMatchDetails,
            ggCredId,
            Some(
              UnsuccessfulNameMatchAttempts(
                1,
                maxRetries,
                sample[A]
              )
            )
          )
        )
      }

      "the name and sautr passed in are the same as the previous attempt" in {
        mockSendBprNameMatchAttemptEvent(1, maxRetries, auditEvent)(())

        testIsErrorOfType[A, NameMatchServiceError.NameMatchFailed[A]](
          toResult(
            sampleNameMatchDetails,
            ggCredId,
            Some(
              UnsuccessfulNameMatchAttempts(
                1,
                maxRetries,
                sampleNameMatchDetails
              )
            )
          )
        )

      }

    }

    "return a successful response" when {

      "the name match succeeded and a BPR was found" in {
        val bpr = sample[BusinessPartnerRecord]

        inSequence {
          mockSendBprNameMatchAttemptEvent(
            1,
            maxRetries,
            auditEvent
          )(())
          mockGetBpr(Right(BusinessPartnerRecordResponse(Some(bpr), None)))
        }

        val result = toResult(
          sampleNameMatchDetails,
          ggCredId,
          None
        )

        await(result.value) shouldBe Right(toSuccessfulResponse(bpr, None))
      }

      "the name match succeeded and a BPR was found and an existing cgt reference" in {
        val bpr          = sample[BusinessPartnerRecord]
        val cgtReference = sample[CgtReference]

        inSequence {
          mockSendBprNameMatchAttemptEvent(
            1,
            maxRetries,
            auditEvent
          )(())
          mockGetBpr(
            Right(BusinessPartnerRecordResponse(Some(bpr), Some(cgtReference)))
          )
        }

        val result = toResult(
          sampleNameMatchDetails,
          ggCredId,
          None
        )

        await(result.value) shouldBe Right(
          toSuccessfulResponse(bpr, Some(cgtReference))
        )
      }

    }

  }

}
