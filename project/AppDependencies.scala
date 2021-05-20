import play.core.PlayVersion.current
import play.sbt.PlayImport._
import sbt.Keys.libraryDependencies
import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc"                %% "govuk-template"             % "5.66.0-play-26",
    "uk.gov.hmrc"                %% "play-ui"                    % "9.4.0-play-26",
    "uk.gov.hmrc"                %% "bootstrap-frontend-play-26" % "4.2.0",
    "uk.gov.hmrc"                %% "mongo-caching"              % "7.0.0-play-26",
    "uk.gov.hmrc"                %% "domain"                     % "5.11.0-play-26",
    "uk.gov.hmrc"                %% "play-language"              % "5.0.0-play-26",
    "org.typelevel"              %% "cats-core"                  % "2.1.0",
    "com.github.kxbmap"          %% "configs"                    % "0.4.4",
    "org.julienrf"               %% "play-json-derived-codecs"   % "7.0.0",
    "com.github.julien-truffaut" %% "monocle-core"               % "2.0.0",
    "com.github.julien-truffaut" %% "monocle-macro"              % "2.0.0",
    "com.github.ghik"             % "silencer-lib"               % "1.6.0" % Provided cross CrossVersion.full
  )

  val test = Seq(
    "org.scalatest"              %% "scalatest"                 % "3.0.9"          % "test",
    "org.jsoup"                   % "jsoup"                     % "1.12.1"         % "test",
    "com.typesafe.play"          %% "play-test"                 % current          % "test",
    "org.scalamock"              %% "scalamock"                 % "4.2.0"          % "test",
    "uk.gov.hmrc"                %% "reactivemongo-test"        % "4.21.0-play-26" % "test",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.1"          % "test",
    "org.pegdown"                 % "pegdown"                   % "1.6.0"          % "test, it",
    "org.scalatestplus.play"     %% "scalatestplus-play"        % "3.1.2"          % "test, it"
  )

}
