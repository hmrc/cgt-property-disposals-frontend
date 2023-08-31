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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.services

import cats.data.EitherT
import cats.instances.future._
import com.typesafe.config.ConfigFactory
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.i18n.Lang
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Request
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.UnsuccessfulNameMatchAttempts.NameMatchDetails.{IndividualRepresenteeNameMatchDetails, IndividualSautrNameMatchDetails, TrustNameMatchDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.BusinessPartnerRecordGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.Generators.sample
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.IdGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.NameMatchGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.RepresenteeAnswersGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.generators.SubscribedDetailsGen._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.ids._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.name.{IndividualName, TrustName}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.SubscribedDetails
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.BusinessPartnerRecordNameMatchAuditDetails.{IndividualNameWithNinoAuditDetails, IndividualNameWithSaUtrAuditDetails, TrustNameWithTrnAuditDetails}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.audit.{BusinessPartnerRecordNameMatchAttemptEvent, BusinessPartnerRecordNameMatchAuditDetails, NameMatchAccountLocked}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr.BusinessPartnerRecordRequest.{IndividualBusinessPartnerRecordRequest, TrustBusinessPartnerRecordRequest}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.onboarding.bpr._
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.RepresenteeReferenceId.{NoReferenceId, RepresenteeCgtReference, RepresenteeNino, RepresenteeSautr}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.audit.CgtAccountNameMatchAttemptEvent
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{Error, NameMatchServiceError, UnsuccessfulNameMatchAttempts}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.NameMatchRetryStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.services.onboarding.{BusinessPartnerRecordService, SubscriptionService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect._

class NameMatchRetryServiceImplSpec extends AnyWordSpec with Matchers with MockFactory {

  val bprService: BusinessPartnerRecordService =
    mock[BusinessPartnerRecordService]

  val retryStore: NameMatchRetryStore = mock[NameMatchRetryStore]

  val mockAuditService: AuditService = mock[AuditService]

  val mockSubscriptionService: SubscriptionService = mock[SubscriptionService]

  val maxRetries: Int = 3

  private val config = Configuration(
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

  private val lang = Lang.defaultLang

  def mockSendNameMatchAccountLockedEvent(
    event: NameMatchAccountLocked
  ): Unit =
    (
      mockAuditService
        .sendEvent(
          _: String,
          _: NameMatchAccountLocked,
          _: String
        )(
          _: ExecutionContext,
          _: HeaderCarrier,
          _: Writes[NameMatchAccountLocked],
          _: Request[_]
        )
      )
      .expects("NameMatchAccountLocked", event, "name-match-account-locked", *, *, *, *)
      .returning(())

  private def mockSendBprNameMatchAttemptEvent(
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

  private def mockSendCgtAccountNameMatchAttemptEvent(
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

  private def mockGetNumberOfUnsuccessfulAttempts[A <: NameMatchDetails](
    expectedGGCredID: GGCredId
  )(result: Either[Error, Option[UnsuccessfulNameMatchAttempts[A]]]) =
    (retryStore
      .get[A](_: GGCredId)(_: Reads[A]))
      .expects(expectedGGCredID, *)
      .returning(Future.successful(result))

  private def mockStoreNumberOfUnsuccessfulAttempts[A <: NameMatchDetails](
    expectedGGCredID: GGCredId,
    unsuccessfulNameMatchAttempts: UnsuccessfulNameMatchAttempts[A]
  )(result: Either[Error, Unit]) =
    (retryStore
      .store[A](_: GGCredId, _: UnsuccessfulNameMatchAttempts[A])(_: Writes[A]))
      .expects(expectedGGCredID, unsuccessfulNameMatchAttempts, *)
      .returning(Future.successful(result))

  private def mockGetIndividualBpr(
    expectedId: Either[SAUTR, NINO],
    expectedName: IndividualName,
    ggCredId: GGCredId,
    createNewEnrolmentIfMissing: Boolean,
    lang: Lang
  )(
    result: Either[Error, BusinessPartnerRecordResponse]
  ) =
    (bprService
      .getBusinessPartnerRecord(_: BusinessPartnerRecordRequest, _: Lang)(
        _: HeaderCarrier
      ))
      .expects(
        IndividualBusinessPartnerRecordRequest(
          expectedId,
          Some(expectedName),
          ggCredId.value,
          createNewEnrolmentIfMissing
        ),
        lang,
        *
      )
      .returning(EitherT.fromEither[Future](result))

  private def mockGetTrustBpr(
    expectedTrn: TRN,
    expectedName: TrustName,
    ggCredId: GGCredId,
    createNewEnrolmentIfMissing: Boolean,
    lang: Lang
  )(
    result: Either[Error, BusinessPartnerRecordResponse]
  ) =
    (bprService
      .getBusinessPartnerRecord(_: BusinessPartnerRecordRequest, _: Lang)(
        _: HeaderCarrier
      ))
      .expects(
        TrustBusinessPartnerRecordRequest(
          Left(expectedTrn),
          Some(expectedName),
          ggCredId.value,
          createNewEnrolmentIfMissing
        ),
        lang,
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
          mockGetNumberOfUnsuccessfulAttempts[IndividualSautrNameMatchDetails](
            ggCredId
          )(Left(Error("")))

          testIsErrorOfType[IndividualSautrNameMatchDetails, NameMatchServiceError.BackendError](
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

          mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
            Right(
              Some(
                UnsuccessfulNameMatchAttempts(1, maxRetries, nameMatchDetails)
              )
            )
          )

          await(
            service.getNumberOfUnsuccessfulAttempts[IndividualSautrNameMatchDetails](ggCredId).value
          ) shouldBe Right(
            Some(UnsuccessfulNameMatchAttempts(1, maxRetries, nameMatchDetails))
          )

        }

      }

      "return None" when {

        "there is no record of attempts for the given gg cred id" in {
          mockGetNumberOfUnsuccessfulAttempts(ggCredId)(Right(None))

          await(
            service.getNumberOfUnsuccessfulAttempts[IndividualSautrNameMatchDetails](ggCredId).value
          ) shouldBe Right(
            None
          )
        }

      }

      "indicate when too many unsuccessful attempts have been made" when {

        "a number can be found and it's equal to the configured maximum" in {
          val nameMatchDetails = sample[TrustNameMatchDetails]
          val auditEvent       = NameMatchAccountLocked(
            maxRetries,
            maxRetries,
            None,
            None,
            Some(nameMatchDetails.name.value),
            None,
            None,
            None,
            Some(nameMatchDetails.trn.value)
          )

          inSequence {
            mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
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
            mockSendNameMatchAccountLockedEvent(auditEvent)
          }

          testIsErrorOfType[TrustNameMatchDetails, NameMatchServiceError.TooManyUnsuccessfulAttempts](
            service.getNumberOfUnsuccessfulAttempts(ggCredId)
          )
        }

        "a number can be found and it's greater than the configured maximum" in {
          val attemptsMade                      = maxRetries + 1
          val (nino, sautr, cgtReference)       = (sample[NINO], sample[SAUTR], sample[CgtReference])
          val representeeCgtRefNameMatchDetails =
            sample[IndividualRepresenteeNameMatchDetails].copy(id = RepresenteeCgtReference(cgtReference))
          val representeeNinoNameMatchDetails   =
            sample[IndividualRepresenteeNameMatchDetails].copy(id = RepresenteeNino(nino))
          val representeeSautrNameMatchDetails  =
            sample[IndividualRepresenteeNameMatchDetails].copy(id = RepresenteeSautr(sautr))
          val representeeNoIdNameMatchDetails   =
            sample[IndividualRepresenteeNameMatchDetails].copy(id = RepresenteeReferenceId.NoReferenceId)
          val individualSautrNameMatch          =
            sample[IndividualSautrNameMatchDetails].copy(sautr = sautr)

          val testCases = List[(NameMatchDetails, NameMatchAccountLocked)](
            representeeCgtRefNameMatchDetails -> NameMatchAccountLocked(
              attemptsMade,
              maxRetries,
              Some(representeeCgtRefNameMatchDetails.name.firstName),
              Some(representeeCgtRefNameMatchDetails.name.lastName),
              None,
              Some(cgtReference.value),
              None,
              None,
              None
            ),
            representeeNinoNameMatchDetails   -> NameMatchAccountLocked(
              attemptsMade,
              maxRetries,
              Some(representeeNinoNameMatchDetails.name.firstName),
              Some(representeeNinoNameMatchDetails.name.lastName),
              None,
              None,
              None,
              Some(nino.value),
              None
            ),
            representeeSautrNameMatchDetails  -> NameMatchAccountLocked(
              attemptsMade,
              maxRetries,
              Some(representeeSautrNameMatchDetails.name.firstName),
              Some(representeeSautrNameMatchDetails.name.lastName),
              None,
              None,
              Some(sautr.value),
              None,
              None
            ),
            representeeNoIdNameMatchDetails   -> NameMatchAccountLocked(
              attemptsMade,
              maxRetries,
              Some(representeeNoIdNameMatchDetails.name.firstName),
              Some(representeeNoIdNameMatchDetails.name.lastName),
              None,
              None,
              None,
              None,
              None
            ),
            individualSautrNameMatch          -> NameMatchAccountLocked(
              attemptsMade,
              maxRetries,
              Some(individualSautrNameMatch.name.firstName),
              Some(individualSautrNameMatch.name.lastName),
              None,
              None,
              Some(sautr.value),
              None,
              None
            )
          )

          testCases.foreach { case (nameMatchDetails, auditEvent) =>
            withClue(s"For name match details $nameMatchDetails: ") {
              inSequence {
                mockGetNumberOfUnsuccessfulAttempts(ggCredId)(
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
                mockSendNameMatchAccountLockedEvent(auditEvent)
              }

              testIsErrorOfType[
                IndividualRepresenteeNameMatchDetails,
                NameMatchServiceError.TooManyUnsuccessfulAttempts
              ](
                service.getNumberOfUnsuccessfulAttempts(ggCredId)
              )
            }
          }
        }
      }

    }

    "attempting a name match for an individual with an SA UTR" must {

      val nameMatchDetails = sample[IndividualSautrNameMatchDetails]

      behave like commonBprNameMatchBehaviour[
        IndividualSautrNameMatchDetails,
        (BusinessPartnerRecord, BusinessPartnerRecordResponse)
      ](
        nameMatchDetails,
        IndividualNameWithSaUtrAuditDetails(
          nameMatchDetails.name.firstName,
          nameMatchDetails.name.lastName,
          nameMatchDetails.sautr.value
        ),
        ggCredId =>
          mockGetIndividualBpr(
            Left(nameMatchDetails.sautr),
            nameMatchDetails.name,
            ggCredId,
            createNewEnrolmentIfMissing = true,
            lang
          )(_),
        service.attemptBusinessPartnerRecordNameMatch(_, _, _, lang),
        _ -> _
      )

    }

    "attempting a name match for a trust" must {

      val nameMatchDetails = sample[TrustNameMatchDetails]

      behave like commonBprNameMatchBehaviour[
        TrustNameMatchDetails,
        (BusinessPartnerRecord, BusinessPartnerRecordResponse)
      ](
        nameMatchDetails,
        TrustNameWithTrnAuditDetails(
          nameMatchDetails.name.value,
          nameMatchDetails.trn.value
        ),
        ggCreId =>
          mockGetTrustBpr(
            nameMatchDetails.trn,
            nameMatchDetails.name,
            ggCreId,
            createNewEnrolmentIfMissing = true,
            lang
          )(_),
        service.attemptBusinessPartnerRecordNameMatch(_, _, _, lang),
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
            None,
            lang
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

        behave like commonBprNameMatchBehaviour[IndividualRepresenteeNameMatchDetails, RepresenteeReferenceId](
          nameMatchDetails,
          IndividualNameWithNinoAuditDetails(
            nameMatchDetails.name.firstName,
            nameMatchDetails.name.lastName,
            representeeNino.value.value
          ),
          ggCredId =>
            mockGetIndividualBpr(
              Right(representeeNino.value),
              nameMatchDetails.name,
              ggCredId,
              createNewEnrolmentIfMissing = false,
              lang
            )(_),
          service.attemptNameMatch(_, _, _, lang),
          (_, _) => representeeNino
        )

      }

      "passed an sautr" must {

        val representeeSautr = sample[RepresenteeSautr]
        val nameMatchDetails = IndividualRepresenteeNameMatchDetails(
          sample[IndividualName],
          representeeSautr
        )

        behave like commonBprNameMatchBehaviour[IndividualRepresenteeNameMatchDetails, RepresenteeReferenceId](
          nameMatchDetails,
          IndividualNameWithSaUtrAuditDetails(
            nameMatchDetails.name.firstName,
            nameMatchDetails.name.lastName,
            representeeSautr.value.value
          ),
          ggCredId =>
            mockGetIndividualBpr(
              Left(representeeSautr.value),
              nameMatchDetails.name,
              ggCredId,
              createNewEnrolmentIfMissing = false,
              lang
            )(_),
          service.attemptNameMatch(_, _, _, lang),
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
            mockSendCgtAccountNameMatchAttemptEvent(
              maxRetries,
              maxRetries,
              nameMatchDetails.name,
              cgtReference
            )

            testIsErrorOfType[IndividualRepresenteeNameMatchDetails, NameMatchServiceError.TooManyUnsuccessfulAttempts](
              service.attemptNameMatch(
                nameMatchDetails,
                ggCredId,
                Some(
                  UnsuccessfulNameMatchAttempts(
                    maxRetries,
                    maxRetries,
                    sample[IndividualRepresenteeNameMatchDetails]
                  )
                ),
                lang
              )
            )
          }

          "the number of previous attempts passed in exceeds the configured maximum" in {
            mockSendCgtAccountNameMatchAttemptEvent(
              maxRetries + 1,
              maxRetries,
              nameMatchDetails.name,
              cgtReference
            )

            testIsErrorOfType[IndividualRepresenteeNameMatchDetails, NameMatchServiceError.TooManyUnsuccessfulAttempts](
              service.attemptNameMatch(
                nameMatchDetails,
                ggCredId,
                Some(
                  UnsuccessfulNameMatchAttempts(
                    maxRetries + 1,
                    maxRetries,
                    sample[IndividualRepresenteeNameMatchDetails]
                  )
                ),
                lang
              )
            )
          }

          "the user has now exceed the configured maximum number of unsuccessful attempts" when {
            def test(subscribedDetails: Option[SubscribedDetails]): Unit = {
              inSequence {
                mockSendCgtAccountNameMatchAttemptEvent(
                  maxRetries,
                  maxRetries,
                  nameMatchDetails.name,
                  cgtReference
                )
                mockGetSubscriptionDetails(
                  cgtReference,
                  Right(subscribedDetails)
                )
                mockStoreNumberOfUnsuccessfulAttempts(
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
                  ),
                  lang
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
              mockSendCgtAccountNameMatchAttemptEvent(
                1,
                maxRetries,
                nameMatchDetails.name,
                cgtReference
              )
              mockGetSubscriptionDetails(cgtReference, Left(Error("")))
            }

            testIsErrorOfType[IndividualRepresenteeNameMatchDetails, NameMatchServiceError.BackendError](
              service.attemptNameMatch(
                nameMatchDetails,
                ggCredId,
                None,
                lang
              )
            )
          }

          "there is an error updating the number of unsuccessful attempts in the retry store" in {
            inSequence {
              mockSendCgtAccountNameMatchAttemptEvent(
                1,
                maxRetries,
                nameMatchDetails.name,
                cgtReference
              )
              mockGetSubscriptionDetails(cgtReference, Right(None))
              mockStoreNumberOfUnsuccessfulAttempts(
                ggCredId,
                UnsuccessfulNameMatchAttempts(1, maxRetries, nameMatchDetails)
              )(
                Left(Error(""))
              )
            }

            testIsErrorOfType[IndividualRepresenteeNameMatchDetails, NameMatchServiceError.BackendError](
              service.attemptNameMatch(
                nameMatchDetails,
                ggCredId,
                None,
                lang
              )
            )
          }

        }

        "return NameMatchFailed" when {

          def test(subscribedDetails: Option[SubscribedDetails]): Unit = {
            inSequence {
              mockSendCgtAccountNameMatchAttemptEvent(
                2,
                maxRetries,
                nameMatchDetails.name,
                cgtReference
              )
              mockGetSubscriptionDetails(cgtReference, Right(subscribedDetails))
              mockStoreNumberOfUnsuccessfulAttempts(
                ggCredId,
                UnsuccessfulNameMatchAttempts(2, maxRetries, nameMatchDetails)
              )(
                Right(())
              )
            }

            testIsErrorOfType[
              IndividualRepresenteeNameMatchDetails,
              NameMatchServiceError.NameMatchFailed[IndividualRepresenteeNameMatchDetails]
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
                ),
                lang
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
            mockSendCgtAccountNameMatchAttemptEvent(
              1,
              maxRetries,
              nameMatchDetails.name,
              cgtReference
            )

            testIsErrorOfType[
              IndividualRepresenteeNameMatchDetails,
              NameMatchServiceError.NameMatchFailed[IndividualRepresenteeNameMatchDetails]
            ](
              service.attemptNameMatch(
                nameMatchDetails,
                ggCredId,
                Some(
                  UnsuccessfulNameMatchAttempts(1, maxRetries, nameMatchDetails)
                ),
                lang
              )
            )

          }

        }

        "return a successful response" when {

          "the subscription details are found and the name matches" in {
            val subscribedDetails = sample[SubscribedDetails]
              .copy(name = Right(nameMatchDetails.name))

            inSequence {
              mockSendCgtAccountNameMatchAttemptEvent(
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
              service.attemptNameMatch(nameMatchDetails, ggCredId, None, lang)
            await(result.value) shouldBe Right(representeeCgtReference)
          }

        }

      }

    }

  }

  private def testIsErrorOfType[A <: NameMatchDetails, E <: NameMatchServiceError[A] : ClassTag](
    result: EitherT[Future, NameMatchServiceError[A], _]
  ): Unit =
    await(result.value) match {
      case Left(_: E) => ()
      case other      =>
        fail(s"expected error of type ${classTag[E]} but found $other")
    }

  private def commonBprNameMatchBehaviour[A <: NameMatchDetails : Gen, B](
    sampleNameMatchDetails: A,
    auditEvent: BusinessPartnerRecordNameMatchAuditDetails,
    mockGetBpr: GGCredId => Either[Error, BusinessPartnerRecordResponse] => Unit,
    toResult: (
      A,
      GGCredId,
      Option[UnsuccessfulNameMatchAttempts[A]]
    ) => EitherT[Future, NameMatchServiceError[A], B],
    toSuccessfulResponse: (BusinessPartnerRecord, BusinessPartnerRecordResponse) => B
  ): Unit = {
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
            mockGetBpr(ggCredId)(Right(BusinessPartnerRecordResponse(None, None, None)))
            mockStoreNumberOfUnsuccessfulAttempts(
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
          mockGetBpr(ggCredId)(Left(Error("")))
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
          mockGetBpr(ggCredId)(Right(BusinessPartnerRecordResponse(None, None, None)))
          mockStoreNumberOfUnsuccessfulAttempts(
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

            mockGetBpr(ggCredId)(Right(BusinessPartnerRecordResponse(None, None, None)))

            mockStoreNumberOfUnsuccessfulAttempts(
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
        val bpr         = sample[BusinessPartnerRecord]
        val bprResponse = BusinessPartnerRecordResponse(Some(bpr), None, None)

        inSequence {
          mockSendBprNameMatchAttemptEvent(
            1,
            maxRetries,
            auditEvent
          )(())
          mockGetBpr(ggCredId)(Right(bprResponse))
        }

        val result = toResult(
          sampleNameMatchDetails,
          ggCredId,
          None
        )

        await(result.value) shouldBe Right(toSuccessfulResponse(bpr, bprResponse))
      }

      "the name match succeeded and a BPR was found with an existing cgt reference" in {
        val bpr          = sample[BusinessPartnerRecord]
        val cgtReference = sample[CgtReference]
        val bprResponse  = BusinessPartnerRecordResponse(Some(bpr), Some(cgtReference), None)

        inSequence {
          mockSendBprNameMatchAttemptEvent(
            1,
            maxRetries,
            auditEvent
          )(())
          mockGetBpr(ggCredId)(Right(bprResponse))
        }

        val result = toResult(
          sampleNameMatchDetails,
          ggCredId,
          None
        )

        await(result.value) shouldBe Right(toSuccessfulResponse(bpr, bprResponse))
      }

      "the name match succeeded and a BPR was found with subscription details of a new enrolment" in {
        val bpr          = sample[BusinessPartnerRecord]
        val cgtReference = sample[CgtReference]
        val bprResponse  = BusinessPartnerRecordResponse(Some(bpr), Some(cgtReference), Some(sample[SubscribedDetails]))

        inSequence {
          mockSendBprNameMatchAttemptEvent(
            1,
            maxRetries,
            auditEvent
          )(())
          mockGetBpr(ggCredId)(Right(bprResponse))
        }

        val result = toResult(
          sampleNameMatchDetails,
          ggCredId,
          None
        )

        await(result.value) shouldBe Right(toSuccessfulResponse(bpr, bprResponse))
      }

    }

  }

}
