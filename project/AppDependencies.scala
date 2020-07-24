import play.core.PlayVersion.current
import play.sbt.PlayImport._
import sbt.Keys.libraryDependencies
import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc"                %% "govuk-template"             % "5.54.0-play-26",
    "uk.gov.hmrc"                %% "play-ui"                    % "8.4.0-play-26",
    "uk.gov.hmrc"                %% "bootstrap-frontend-play-26" % "2.24.0",
    "uk.gov.hmrc"                %% "mongo-caching"              % "6.15.0-play-26",
    "uk.gov.hmrc"                %% "domain"                     % "5.6.0-play-26",
    "uk.gov.hmrc"                %% "play-language"              % "4.3.0-play-26",
    "org.typelevel"              %% "cats-core"                  % "2.1.0",
    "com.github.kxbmap"          %% "configs"                    % "0.4.4",
    "org.julienrf"               %% "play-json-derived-codecs"   % "7.0.0",
    "com.github.julien-truffaut" %% "monocle-core"               % "2.0.0",
    "com.github.julien-truffaut" %% "monocle-macro"              % "2.0.0",
    "com.github.ghik"             % "silencer-lib"               % "1.6.0" % Provided cross CrossVersion.full
  )

  val test = Seq(
    "org.scalatest"              %% "scalatest"                 % "3.0.8"          % "test",
    "org.jsoup"                   % "jsoup"                     % "1.12.1"         % "test",
    "com.typesafe.play"          %% "play-test"                 % current          % "test",
    "org.scalamock"              %% "scalamock"                 % "4.2.0"          % "test",
    "uk.gov.hmrc"                %% "reactivemongo-test"        % "4.21.0-play-26" % "test",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.1"          % "test",
    "org.pegdown"                 % "pegdown"                   % "1.6.0"          % "test, it",
    "uk.gov.hmrc"                %% "service-integration-test"  % "0.10.0-play-26" % "test, it",
    "org.scalatestplus.play"     %% "scalatestplus-play"        % "3.1.2"          % "test, it"
  )

}
