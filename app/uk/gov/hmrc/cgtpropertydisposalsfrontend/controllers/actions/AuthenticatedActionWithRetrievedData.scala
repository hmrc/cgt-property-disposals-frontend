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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.actions

import java.time.LocalDate
import org.joda.time.{LocalDate => JodaLocaDate}
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc._
import play.api.mvc.Results.SeeOther
import uk.gov.hmrc.auth.core.retrieve.{~, Name => GGName, _}
import uk.gov.hmrc.auth.core.{ConfidenceLevel, _}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.{DateOfBirth, Email, NINO, Name, SAUTR, UserType}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.repos.SessionStore
import uk.gov.hmrc.cgtpropertydisposalsfrontend.util.toFuture
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

final case class AuthenticatedRequestWithRetrievedData[A](userType: UserType,
                                                          request: MessagesRequest[A])
  extends WrappedRequest[A](request)

@Singleton
class AuthenticatedActionWithRetrievedData @Inject()(
                                                      val authConnector: AuthConnector,
                                                      val config: Configuration,
                                                      val errorHandler: ErrorHandler,
                                                      val sessionStore: SessionStore
                                                    )(implicit val executionContext: ExecutionContext)
  extends AuthenticatedActionBase[AuthenticatedRequestWithRetrievedData] {

  override def authorisedFunction[A](auth: AuthorisedFunctions,
                                     request: MessagesRequest[A]
                                    ): Future[Either[Result, AuthenticatedRequestWithRetrievedData[A]]] = {
    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

    auth
      .authorised()
      .retrieve(
        v2.Retrievals.confidenceLevel and
          v2.Retrievals.affinityGroup and
          v2.Retrievals.nino and
          v2.Retrievals.itmpName and
          v2.Retrievals.name and
          v2.Retrievals.itmpDateOfBirth and
          v2.Retrievals.email and
          v2.Retrievals.allEnrolments
      ) {
        case cl ~ Some(AffinityGroup.Individual) ~ maybeNino ~ maybeItmpName ~ maybeGGName ~ _ ~ maybeEmail ~ _ if cl < ConfidenceLevel.L200 =>
          withName(maybeItmpName, maybeGGName, request)(name =>
            Right(AuthenticatedRequestWithRetrievedData(
              UserType.IndividualWithInsufficientConfidenceLevel(maybeNino.map(NINO),
                name,
                maybeEmail.filter(_.nonEmpty).map(Email(_))
              ),
              request))
          )

        case individual @ _ ~ Some(AffinityGroup.Individual) ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ =>
          individual match {
            case  _ ~ _ ~ Some(nino) ~ maybeItmpName ~ maybeGGName ~ maybeDob ~ maybeEmail ~ _ =>
              withName(maybeItmpName, maybeGGName, request)( name =>
                Right(AuthenticatedRequestWithRetrievedData(
                  UserType.Individual(
                    Right(NINO(nino)),
                    name,
                    maybeDob.map(d => DateOfBirth(toLocalDate(d))),
                    maybeEmail.filter(_.nonEmpty).map(Email(_))
                  ),
                  request
                ))
              )

          }

        case _ @  _ ~ Some(AffinityGroup.Organisation) ~ _ ~ _ ~ _ ~ _ ~ maybeEmail ~ enrolments =>
          handleOrganisation(request, enrolments, maybeEmail)

        case _ @ _ ~ otherAffinityGroup ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ =>
          logger.warn(s"Got request for unsupported affinity group $otherAffinityGroup")
          Left(errorHandler.errorResult()(request))

      }
  }

  private def withName[A](itmpName: Option[ItmpName],
                          ggName: Option[GGName],
                          request: MessagesRequest[A]
                         )(f: Name => Either[Result, AuthenticatedRequestWithRetrievedData[A]]
                         ): Either[Result, AuthenticatedRequestWithRetrievedData[A]] = {
    def itmpNameToName(itmpName: ItmpName): Option[Name] =
      itmpName match {
        case ItmpName(Some(givenName), _, Some(familyName)) => Some(Name(givenName, familyName))
        case _                                              => None
      }

    def ggNameToName(ggName: GGName): Option[Name] =
      ggName match {
        case GGName(Some(name), _) =>
          name.split(' ').toList.filter(_.nonEmpty) match {
            case Nil      => None // no name
            case firstName :: tl =>
              tl.lastOption match {
                case Some(lastName) => Some(Name(firstName, lastName))
                case None           => None // only one name
              }
          }
        case GGName(None, _) => None
      }

    itmpName.flatMap(itmpNameToName).orElse(ggName.flatMap(ggNameToName))
      .fold[Either[Result, AuthenticatedRequestWithRetrievedData[A]]]{
        logger.warn(s"Failed to retrieve name")
        Left(errorHandler.errorResult()(request))
      }(f)
  }

  private def toLocalDate(jodaDate: JodaLocaDate): LocalDate =
    LocalDate.of(jodaDate.getYear, jodaDate.getMonthOfYear, jodaDate.getDayOfMonth)

  private def handleOrganisation[A](request: MessagesRequest[A],
                                    enrolments: Enrolments,
                                    email: Option[String]
                                   ): Either[Result, AuthenticatedRequestWithRetrievedData[A]] =
  // work out if it is an organisation or not
    enrolments.getEnrolment("HMRC-TERS-ORG") match {
      case None =>
        Right(AuthenticatedRequestWithRetrievedData(UserType.OrganisationUnregisteredTrust, request))

      case Some(trustEnrolment) =>
        trustEnrolment.getIdentifier("SAUTR")
          .fold[Either[Result, AuthenticatedRequestWithRetrievedData[A]]]{
            logger.warn(
              s"Could not find SAUTR identifier for user with trust enrolment $trustEnrolment. " +
                s"Found identifier keys [${trustEnrolment.identifiers.map(_.key).mkString(",")}]"
            )
            Left(errorHandler.errorResult()(request))
          }( id =>
            Right(AuthenticatedRequestWithRetrievedData(
              UserType.Trust(SAUTR(id.value), email.filter(_.nonEmpty).map(Email(_))),
              request
            ))
          )
    }


}
