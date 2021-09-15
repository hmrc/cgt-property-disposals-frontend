import play.core.PlayVersion.current
import play.sbt.PlayImport._
import sbt.Keys.libraryDependencies
import sbt._

object AppDependencies {

  val playVersion = "play-28"

  val compile = Seq(
    "uk.gov.hmrc"                %% "play-frontend-hmrc"                % s"0.85.0-$playVersion",
    "uk.gov.hmrc"                %% "govuk-template"                    % s"5.68.0-$playVersion",
    "uk.gov.hmrc"                %% "play-ui"                           % s"9.4.0-$playVersion",
    "uk.gov.hmrc"                %% s"bootstrap-frontend-$playVersion"  % "5.12.0",
    "uk.gov.hmrc"                %% "mongo-caching"                     % s"7.0.0-$playVersion",
    "uk.gov.hmrc"                %% "domain"                            % s"6.1.0-$playVersion",
    "uk.gov.hmrc"                %% "play-language"                     % s"5.0.0-$playVersion",
    "org.typelevel"              %% "cats-core"                         % "2.6.0",
    "com.github.kxbmap"          %% "configs"                           % "0.6.1",
    "org.julienrf"               %% "play-json-derived-codecs"          % "10.0.2",
    "com.github.julien-truffaut" %% "monocle-core"                      % "2.0.0",
    "com.github.julien-truffaut" %% "monocle-macro"                     % "2.0.0",
    compilerPlugin("com.github.ghik" % "silencer-plugin"    % "1.7.1" cross CrossVersion.full),
    "com.github.ghik"             % "silencer-lib"                      % "1.7.1" % Provided cross CrossVersion.full
  )

  val test = Seq(
    "org.scalatest"              %% "scalatest"                 % "3.2.9"               % "test",
    "org.jsoup"                   % "jsoup"                     % "1.12.1"              % "test",
    "com.typesafe.play"          %% "play-test"                 % current               % "test",
    "org.scalamock"              %% "scalamock"                 % "4.2.0"               % "test",
    "org.scalatestplus"          %% "scalatestplus-scalacheck"  % "3.1.0.0-RC2"         % "test, it",
    "com.vladsch.flexmark"        % "flexmark-all"              % "0.35.10"             % "test",
    "uk.gov.hmrc"                %% "reactivemongo-test"        % s"5.0.0-$playVersion" % "test",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.1"               % "test",
    "org.scalatestplus.play"     %% "scalatestplus-play"        % "5.1.0"               % "test, it"
  )

}
