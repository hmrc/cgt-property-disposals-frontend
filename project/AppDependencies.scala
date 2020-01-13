import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc"       %% "govuk-template"           % "5.45.0-play-26",
    "uk.gov.hmrc"       %% "play-ui"                  % "8.4.0-play-26",
    "uk.gov.hmrc"       %% "bootstrap-play-26"        % "1.3.0",
    "uk.gov.hmrc"       %% "mongo-caching"            % "6.6.0-play-26",
    "org.typelevel"     %% "cats-core"                % "1.6.1",
    "com.github.kxbmap" %% "configs"                  % "0.4.4",
    "org.julienrf"      %% "play-json-derived-codecs" % "3.3"
  )

  val test = Seq(
    "org.scalatest"              %% "scalatest"                 % "3.0.8"          % "test",
    "org.jsoup"                  % "jsoup"                      % "1.10.2"         % "test",
    "com.typesafe.play"          %% "play-test"                 % current          % "test",
    "org.scalamock"              %% "scalamock"                 % "4.2.0"          % "test",
    "uk.gov.hmrc"                %% "reactivemongo-test"        % "4.15.0-play-26" % "test",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.1"          % "test",
    "org.pegdown"                % "pegdown"                    % "1.6.0"          % "test, it",
    "uk.gov.hmrc"                %% "service-integration-test"  % "0.9.0-play-26"  % "test, it",
    "org.scalatestplus.play"     %% "scalatestplus-play"        % "3.1.2"          % "test, it"
  )

}
