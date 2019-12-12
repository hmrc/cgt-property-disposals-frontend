package uk.gov.hmrc.cgtpropertydisposalsfrontend.filters

import akka.stream.Materializer
import com.google.inject.Inject
import play.api.Configuration
import play.api.mvc.{Filter, MessagesControllerComponents, RequestHeader, Result}
import play.api.mvc.Results.{NotFound, Redirect}
import uk.gov.hmrc.auth.otac.{OtacAuthConnector, OtacAuthorisationFunctions}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.config.ErrorHandler
import uk.gov.hmrc.http.SessionKeys
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.cgtpropertydisposalsfrontend.controllers.routes

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal


class EmailWhitelistingFilter @Inject()(
                                         override val mat: Materializer,
                                         val authConnector: OtacAuthConnector,
                                         config: Configuration,
                                         errorHandler: ErrorHandler,
                                         mcc: MessagesControllerComponents)(implicit ec: ExecutionContext) extends Filter with OtacAuthorisationFunctions{

  val whitelistingEnabled = config.underlying.getBoolean("passcodeAuthentication.enabled")

  val otacUrl = config.underlying.getString("otac.url")

  val selfBaseUrl: String = config.underlying.getString("self.url")

  override def apply(f: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] = {

    val messages = mcc.messagesApi.preferred(rh)

    if(whitelistingEnabled){
      rh.session.get(SessionKeys.otacToken)
        .orElse(rh.queryString.get("p").flatMap(_.headOption))
        .orElse(rh.cookies.get("whitelisting").map(_.value))
        .map {
          token =>
            implicit val hc = HeaderCarrierConverter.fromHeadersAndSession(rh.headers, Some(rh.session))
            withVerifiedPasscode[Result]("capital-gains-tax-property-disposals", Some(token)) {
              f(rh)
            }.recover {
              case NonFatal(_) =>
                Redirect(s"$otacUrl?p=$token")
                  .addingToSession(
                    SessionKeys.redirect -> s"$selfBaseUrl${routes.StartController.start()}?p=$token",
                    SessionKeys.otacToken -> token
                  )(rh)

            }
        }.getOrElse {
        Future.successful(errorHandler.errorResult()(rh))
      }
    }else{
      f(rh)
    }
  }
}

