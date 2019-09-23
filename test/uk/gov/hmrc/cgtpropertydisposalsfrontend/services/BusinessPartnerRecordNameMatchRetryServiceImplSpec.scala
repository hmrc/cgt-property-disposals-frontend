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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import cats.data.EitherT
import cats.instances.future._
import com.typesafe.config.ConfigFactory
import org.scalacheck.ScalacheckShapeless._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import play.api.Configuration
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.{BusinessPartnerRecord, BusinessPartnerRecordRequest, BusinessPartnerRecordResponse, NameMatchError, NumberOfUnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.bpr.BusinessPartnerRecordRequest.IndividualBusinessPartnerRecordRequest
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids.{GGCredId, SAUTR}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, Name, sample}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.BusinessPartnerRecordNameMatchRetryStore
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect._

class BusinessPartnerRecordNameMatchRetryServiceImplSpec extends WordSpec with Matchers with MockFactory {

  val bprService: BusinessPartnerRecordService = mock[BusinessPartnerRecordService]

  val retryStore: BusinessPartnerRecordNameMatchRetryStore = mock[BusinessPartnerRecordNameMatchRetryStore]

  val maxRetries: Int = 3

  val config = Configuration(
    ConfigFactory.parseString(
      s"""
        |bpr-name-match.max-retries = $maxRetries
        |""".stripMargin
    )
  )

  val service = new BusinessPartnerRecordNameMatchRetryServiceImpl(bprService, retryStore, config)

  def mockGetNumberOfUnsccessfulAttempts(expectedGGCredID: GGCredId)(result: Either[Error, Option[Int]]) =
    (retryStore
      .get(_: GGCredId))
      .expects(expectedGGCredID)
      .returning(Future.successful(result))

  def mockStoreNumberOfUnsccessfulAttempts(expectedGGCredID: GGCredId,
                                          expectedNumberOfUnsuccessfulAttempts: Int)(result: Either[Error, Unit]) =
    (retryStore
      .store(_: GGCredId, _: Int))
      .expects(expectedGGCredID, expectedNumberOfUnsuccessfulAttempts)
      .returning(Future.successful(result))


  def mockGetBpr(expectedSautr: SAUTR, expectedName: Name)(result: Either[Error, BusinessPartnerRecordResponse]) =
    (bprService
      .getBusinessPartnerRecord(_: BusinessPartnerRecordRequest)(_: HeaderCarrier))
      .expects(IndividualBusinessPartnerRecordRequest(Left(expectedSautr), Some(expectedName)), *)
      .returning(EitherT.fromEither[Future](result))

  "BusinessPartnerRecordNameMatchRetryServiceImpl" when {

    val ggCredId = sample[GGCredId]
    val sautr    = sample[SAUTR]
    val name     = sample[Name]

    def testIsErrorOfType[E <: NameMatchError: ClassTag](
                                                          result: EitherT[Future, NameMatchError, _]
                                                        ) =
      await(result.value) match {
        case Left(_: E) => ()
        case other      => fail(s"expected error of type ${classTag[E]} but found $other")
      }

    "getting the number of previous unsuccessful attempts" must {

      "return an error" when {

        "there is an error reading the value" in {
          mockGetNumberOfUnsccessfulAttempts(ggCredId)(Left(Error("")))

          testIsErrorOfType[NameMatchError.BackendError](
            service.getNumberOfUnsuccessfulAttempts(ggCredId)
          )
        }

      }

      "return the number of attempts" when {

        "a number can be found and it's less than the configured maximum" in {
          mockGetNumberOfUnsccessfulAttempts(ggCredId)(Right(Some(1)))

          await(service.getNumberOfUnsuccessfulAttempts(ggCredId).value) shouldBe Right(
            NumberOfUnsuccessfulNameMatchAttempts(1, maxRetries)
          )

        }

      }

      "return zero" when {

        "there is no record of attempts for the given gg cred id" in {
          mockGetNumberOfUnsccessfulAttempts(ggCredId)(Right(None))

          await(service.getNumberOfUnsuccessfulAttempts(ggCredId).value) shouldBe Right(
            NumberOfUnsuccessfulNameMatchAttempts(0, maxRetries)
          )
        }

      }

      "indicate when too many unsuccessful attempts have been made" when {

        "a number can be found and it's equal to the configured maximum" in {
          mockGetNumberOfUnsccessfulAttempts(ggCredId)(Right(Some(maxRetries)))

          testIsErrorOfType[NameMatchError.TooManyUnsuccessfulAttempts](
            service.getNumberOfUnsuccessfulAttempts(ggCredId)
          )
        }

        "a number can be found and it's greater than the configured maximum" in {
          mockGetNumberOfUnsccessfulAttempts(ggCredId)(Right(Some(maxRetries + 1)))

          testIsErrorOfType[NameMatchError.TooManyUnsuccessfulAttempts](
            service.getNumberOfUnsuccessfulAttempts(ggCredId)
          )
        }

      }

    }

    "attempting a name match when getting a BPR" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      "return TooManyUnsuccessfulAttempts" when {

        "the number of previous attempts passed in equals the configured maximum" in {
          testIsErrorOfType[NameMatchError.TooManyUnsuccessfulAttempts](
            service.attemptBusinessPartnerRecordNameMatch(
              sautr,
              name,
              ggCredId,
              maxRetries
            )
          )
        }

        "the number of previous attempts passed in exceeds the configured maximum" in {
          testIsErrorOfType[NameMatchError.TooManyUnsuccessfulAttempts](
            service.attemptBusinessPartnerRecordNameMatch(
              sautr,
              name,
              ggCredId,
              maxRetries + 1
            )
          )
        }

        "a BPR was not found and the user has now exceeded the configured maximum number of " +
          "unsuccessful attempts" in {
          inSequence {
            mockGetBpr(sautr, name)(Right(BusinessPartnerRecordResponse(None)))
            mockStoreNumberOfUnsccessfulAttempts(ggCredId, maxRetries)(Right(()))
          }

          testIsErrorOfType[NameMatchError.TooManyUnsuccessfulAttempts](
            service.attemptBusinessPartnerRecordNameMatch(
              sautr,
              name,
              ggCredId,
              maxRetries - 1
            )
          )
        }

      }

      "return a BackendError" when {

        "there is an error getting the BPR" in {
          inSequence {
            mockGetBpr(sautr, name)(Left(Error("")))
          }

          testIsErrorOfType[NameMatchError.BackendError](
            service.attemptBusinessPartnerRecordNameMatch(
              sautr,
              name,
              ggCredId,
              0
            )
          )
        }

        "there is an error updating the number of unsuccessful attempts in the retry store" in {
          inSequence {
            mockGetBpr(sautr, name)(Right(BusinessPartnerRecordResponse(None)))
            mockStoreNumberOfUnsccessfulAttempts(ggCredId, 1)(Left(Error("")))
          }

          testIsErrorOfType[NameMatchError.BackendError](
            service.attemptBusinessPartnerRecordNameMatch(
              sautr,
              name,
              ggCredId,
              0
            )
          )
        }

      }

      "return NameMatchFailed" when {

        "a BPR was not found and the user has not exceeded the configured maximum number of " +
          "unsuccessful attempts" in {
          inSequence {
            mockGetBpr(sautr, name)(Right(BusinessPartnerRecordResponse(None)))
            mockStoreNumberOfUnsccessfulAttempts(ggCredId, 1)(Right(()))
          }

          testIsErrorOfType[NameMatchError.NameMatchFailed](
            service.attemptBusinessPartnerRecordNameMatch(
              sautr,
              name,
              ggCredId,
              0
            )
          )
        }

      }

      "return a BPR" when {

        "the name match succeeded and a BPR was found" in {
          val bpr = sample[BusinessPartnerRecord]

          mockGetBpr(sautr, name)(Right(BusinessPartnerRecordResponse(Some(bpr))))

           val result = service.attemptBusinessPartnerRecordNameMatch(
              sautr,
              name,
              ggCredId,
              0
            )

          await(result.value) shouldBe Right(bpr)
        }

      }

    }

  }

}
