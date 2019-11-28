/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding

import cats.data.EitherT
import cats.instances.future._
import com.typesafe.config.ConfigFactory
import org.scalacheck.ScalacheckShapeless._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.Configuration
import play.api.libs.json.{Reads, Writes}
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{GGCredId, SAUTR, TRN}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest.{IndividualBusinessPartnerRecordRequest, TrustBusinessPartnerRecordRequest}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.UnsuccessfulNameMatchAttempts.NameMatchDetails.{IndividualNameMatchDetails, TrustNameMatchDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.onboarding.BusinessPartnerRecordNameMatchRetryStore
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect._

class BusinessPartnerRecordNameMatchRetryServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val bprService: BusinessPartnerRecordService = mock[BusinessPartnerRecordService]

  val retryStore: BusinessPartnerRecordNameMatchRetryStore = mock[BusinessPartnerRecordNameMatchRetryStore]

  val mockAuditService: OnboardingAuditService = mock[OnboardingAuditService]

  val maxRetries: Int = 3

  val config = Configuration(
    ConfigFactory.parseString(
      s"""
        |bpr-name-match.max-retries = $maxRetries
        |""".stripMargin
    )
  )

  val service = new BusinessPartnerRecordNameMatchRetryServiceImpl(bprService, retryStore, mockAuditService, config)

  def mockSendBprNameMatchAttemptEvent[A <: NameMatchDetails](
    attemptsMade: Int,
    maxAttemptsMade: Int,
    nameMatchDetails: A
  )(result: Unit) =
    (mockAuditService
      .sendBusinessPartnerRecordNameMatchAttemptEvent(_: Int, _: Int, _: A)(
        _: HeaderCarrier,
        _: ExecutionContext
      ))
      .expects(attemptsMade, maxAttemptsMade, nameMatchDetails, *, *)
      .returning(result)

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

  def mockGetBpr(expectedSautr: SAUTR, expectedName: IndividualName)(
    result: Either[Error, BusinessPartnerRecordResponse]
  ) =
    (bprService
      .getBusinessPartnerRecord(_: BusinessPartnerRecordRequest)(_: HeaderCarrier))
      .expects(IndividualBusinessPartnerRecordRequest(Left(expectedSautr), Some(expectedName)), *)
      .returning(EitherT.fromEither[Future](result))

  def mockGetBpr(expectedTrn: TRN, expectedName: TrustName)(
    result: Either[Error, BusinessPartnerRecordResponse]
  ) =
    (bprService
      .getBusinessPartnerRecord(_: BusinessPartnerRecordRequest)(_: HeaderCarrier))
      .expects(TrustBusinessPartnerRecordRequest(Left(expectedTrn), Some(expectedName)), *)
      .returning(EitherT.fromEither[Future](result))

  "BusinessPartnerRecordNameMatchRetryServiceImpl" when {

    val ggCredId                   = sample[GGCredId]
    val individualNameMatchDetails = sample[IndividualNameMatchDetails]

    def testIsErrorOfType[A <: NameMatchDetails, E <: NameMatchError[A]: ClassTag](
      result: EitherT[Future, NameMatchError[A], _]
    ) =
      await(result.value) match {
        case Left(_: E) => ()
        case other      => fail(s"expected error of type ${classTag[E]} but found $other")
      }

    "getting the number of previous unsuccessful attempts" must {

      "return an error" when {

        "there is an error reading the value" in {
          mockGetNumberOfUnsccessfulAttempts[IndividualNameMatchDetails](ggCredId)(Left(Error("")))

          testIsErrorOfType[IndividualNameMatchDetails, NameMatchError.BackendError](
            service.getNumberOfUnsuccessfulAttempts[IndividualNameMatchDetails](ggCredId)
          )
        }

      }

      "return the number of attempts" when {

        "a number can be found and it's less than the configured maximum" in {
          mockGetNumberOfUnsccessfulAttempts(ggCredId)(
            Right(
              Some(
                UnsuccessfulNameMatchAttempts(1, maxRetries, individualNameMatchDetails)
              )
            )
          )

          await(service.getNumberOfUnsuccessfulAttempts(ggCredId).value) shouldBe Right(
            Some(UnsuccessfulNameMatchAttempts(1, maxRetries, individualNameMatchDetails))
          )

        }

      }

      "return None" when {

        "there is no record of attempts for the given gg cred id" in {
          mockGetNumberOfUnsccessfulAttempts(ggCredId)(Right(None))

          await(service.getNumberOfUnsuccessfulAttempts(ggCredId).value) shouldBe Right(
            None
          )
        }

      }

      "indicate when too many unsuccessful attempts have been made" when {

        "a number can be found and it's equal to the configured maximum" in {
          mockGetNumberOfUnsccessfulAttempts(ggCredId)(
            Right(
              Some(
                UnsuccessfulNameMatchAttempts(maxRetries, maxRetries, individualNameMatchDetails)
              )
            )
          )

          testIsErrorOfType[IndividualNameMatchDetails, NameMatchError.TooManyUnsuccessfulAttempts](
            service.getNumberOfUnsuccessfulAttempts(ggCredId)
          )
        }

        "a number can be found and it's greater than the configured maximum" in {
          mockGetNumberOfUnsccessfulAttempts(ggCredId)(
            Right(
              Some(
                UnsuccessfulNameMatchAttempts(maxRetries + 1, maxRetries, individualNameMatchDetails)
              )
            )
          )

          testIsErrorOfType[IndividualNameMatchDetails, NameMatchError.TooManyUnsuccessfulAttempts](
            service.getNumberOfUnsuccessfulAttempts(ggCredId)
          )
        }

      }

    }

    "attempting a name match when getting a BPR" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      "return TooManyUnsuccessfulAttempts" when {

        "the number of previous attempts passed in equals the configured maximum" in {
          testIsErrorOfType[IndividualNameMatchDetails, NameMatchError.TooManyUnsuccessfulAttempts](
            service.attemptBusinessPartnerRecordNameMatch(
              individualNameMatchDetails,
              ggCredId,
              Some(UnsuccessfulNameMatchAttempts(maxRetries, maxRetries, sample[IndividualNameMatchDetails]))
            )
          )
        }

        "the number of previous attempts passed in exceeds the configured maximum" in {
          testIsErrorOfType[IndividualNameMatchDetails, NameMatchError.TooManyUnsuccessfulAttempts](
            service.attemptBusinessPartnerRecordNameMatch(
              individualNameMatchDetails,
              ggCredId,
              Some(UnsuccessfulNameMatchAttempts(maxRetries + 1, maxRetries, sample[IndividualNameMatchDetails]))
            )
          )
        }

        "a BPR was not found and the user has now exceeded the configured maximum number of " +
          "unsuccessful attempts" in {
          inSequence {
            mockGetBpr(individualNameMatchDetails.sautr, individualNameMatchDetails.name)(
              Right(BusinessPartnerRecordResponse(None))
            )
            mockStoreNumberOfUnsccessfulAttempts(
              ggCredId,
              UnsuccessfulNameMatchAttempts(maxRetries, maxRetries, individualNameMatchDetails)
            )(Right(()))
          }

          testIsErrorOfType[IndividualNameMatchDetails, NameMatchError.TooManyUnsuccessfulAttempts](
            service.attemptBusinessPartnerRecordNameMatch(
              individualNameMatchDetails,
              ggCredId,
              Some(UnsuccessfulNameMatchAttempts(maxRetries - 1, maxRetries, sample[IndividualNameMatchDetails]))
            )
          )
        }

      }

      "return a BackendError" when {

        "there is an error getting the BPR" in {
          val trustNameMatchDetails = sample[TrustNameMatchDetails]

          inSequence {
            mockGetBpr(trustNameMatchDetails.trn, trustNameMatchDetails.name)(Left(Error("")))
          }

          testIsErrorOfType[TrustNameMatchDetails, NameMatchError.BackendError](
            service.attemptBusinessPartnerRecordNameMatch(
              trustNameMatchDetails,
              ggCredId,
              None
            )
          )
        }

        "there is an error updating the number of unsuccessful attempts in the retry store" in {
          inSequence {
            mockGetBpr(individualNameMatchDetails.sautr, individualNameMatchDetails.name)(
              Right(BusinessPartnerRecordResponse(None))
            )
            mockStoreNumberOfUnsccessfulAttempts(
              ggCredId,
              UnsuccessfulNameMatchAttempts(1, maxRetries, individualNameMatchDetails)
            )(
              Left(Error(""))
            )
          }

          testIsErrorOfType[IndividualNameMatchDetails, NameMatchError.BackendError](
            service.attemptBusinessPartnerRecordNameMatch(
              individualNameMatchDetails,
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
            mockGetBpr(individualNameMatchDetails.sautr, individualNameMatchDetails.name)(
              Right(BusinessPartnerRecordResponse(None))
            )
            mockStoreNumberOfUnsccessfulAttempts(
              ggCredId,
              UnsuccessfulNameMatchAttempts(1, maxRetries, individualNameMatchDetails)
            )(
              Right(())
            )
          }

          testIsErrorOfType[IndividualNameMatchDetails, NameMatchError.NameMatchFailed[IndividualNameMatchDetails]](
            service.attemptBusinessPartnerRecordNameMatch(
              individualNameMatchDetails,
              ggCredId,
              None
            )
          )
        }

        "the name and sautr passed in are the same as the previous attempt" in {
          mockSendBprNameMatchAttemptEvent(1, maxRetries, individualNameMatchDetails)(())
          testIsErrorOfType[IndividualNameMatchDetails, NameMatchError.NameMatchFailed[IndividualNameMatchDetails]](
            service.attemptBusinessPartnerRecordNameMatch(
              individualNameMatchDetails,
              ggCredId,
              Some(UnsuccessfulNameMatchAttempts(1, maxRetries, individualNameMatchDetails))
            )
          )

        }

      }

      "return a BPR" when {

        "the name match succeeded and a BPR was found for an individual" in {
          val bpr = sample[BusinessPartnerRecord]

          mockGetBpr(individualNameMatchDetails.sautr, individualNameMatchDetails.name)(
            Right(BusinessPartnerRecordResponse(Some(bpr)))
          )

          val result = service.attemptBusinessPartnerRecordNameMatch(
            individualNameMatchDetails,
            ggCredId,
            None
          )

          await(result.value) shouldBe Right(bpr)
        }

        "the name match succeeded and a BPR was found for a trust" in {
          val bpr                   = sample[BusinessPartnerRecord]
          val trustNameMatchDetails = sample[TrustNameMatchDetails]

          mockGetBpr(trustNameMatchDetails.trn, trustNameMatchDetails.name)(
            Right(BusinessPartnerRecordResponse(Some(bpr)))
          )

          val result = service.attemptBusinessPartnerRecordNameMatch(
            trustNameMatchDetails,
            ggCredId,
            None
          )

          await(result.value) shouldBe Right(bpr)
        }

      }

    }

  }

}
