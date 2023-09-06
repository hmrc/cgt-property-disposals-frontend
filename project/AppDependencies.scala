import sbt.*

object AppDependencies {

  val playVersion      = "play-28"
  val bootstrapVersion = "7.19.0"
  val silencerVersion  = "1.7.12"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"                %% "play-frontend-hmrc"               % s"7.7.0-$playVersion",
    "uk.gov.hmrc"                %% s"bootstrap-frontend-$playVersion" % bootstrapVersion,
    "uk.gov.hmrc.mongo"          %% s"hmrc-mongo-$playVersion"         % "0.70.0",
    "uk.gov.hmrc"                %% "domain"                           % s"8.3.0-$playVersion",
    "org.typelevel"              %% "cats-core"                        % "2.6.0",
    "com.github.kxbmap"          %% "configs"                          % "0.6.1",
    "org.julienrf"               %% "play-json-derived-codecs"         % "10.0.2",
    "com.github.julien-truffaut" %% "monocle-core"                     % "2.0.0",
    "com.github.julien-truffaut" %% "monocle-macro"                    % "2.0.0"
  )

  val test: Seq[ModuleID] = Seq(
    "org.jsoup"                   % "jsoup"                         % "1.12.1"         % "test",
    "org.scalamock"              %% "scalamock"                     % "5.2.0"          % "test",
    "org.scalatestplus"          %% "scalatestplus-scalacheck"      % "3.1.0.0-RC2"    % "test",
    "uk.gov.hmrc.mongo"          %% s"hmrc-mongo-test-$playVersion" % "0.70.0"         % "test",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.16"     % "1.3.1"          % "test",
    "org.scalatestplus.play"     %% "scalatestplus-play"            % "5.1.0"          % "test",
    "uk.gov.hmrc"                %% s"bootstrap-test-$playVersion"  % bootstrapVersion % "test"
  )

}
