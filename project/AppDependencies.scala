import sbt.*

object AppDependencies {
  val playVersion      = "play-30"
  val bootstrapVersion = "9.12.0"
  val mongoVersion     = "2.6.0"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"       %% s"play-frontend-hmrc-$playVersion" % "12.1.0",
    "uk.gov.hmrc"       %% s"bootstrap-frontend-$playVersion" % bootstrapVersion,
    "uk.gov.hmrc.mongo" %% s"hmrc-mongo-$playVersion"         % mongoVersion,
    "uk.gov.hmrc"       %% s"domain-$playVersion"             % "10.0.0",
    "org.typelevel"     %% "cats-core"                        % "2.12.0",
    "dev.optics"        %% "monocle-core"                     % "3.1.0",
    "dev.optics"        %% "monocle-macro"                    % "3.1.0"
  )

  def test(scope: String = "test"): Seq[ModuleID] = Seq(
    "org.jsoup"           % "jsoup"                         % "1.19.1"         % scope,
    "org.scalamock"      %% "scalamock"                     % "6.0.0"          % scope,
    "org.scalatest"      %% "scalatest"                     % "3.2.19"         % scope,
    "org.scalacheck"     %% "scalacheck"                    % "1.18.1"         % scope,
    "io.github.martinhh" %% "scalacheck-derived"            % "0.8.2"          % scope,
    "org.scalatestplus"   % "scalacheck-1-18_3"             % "3.2.19.0"       % scope,
    "uk.gov.hmrc.mongo"  %% s"hmrc-mongo-test-$playVersion" % mongoVersion     % scope,
    "uk.gov.hmrc"        %% s"bootstrap-test-$playVersion"  % bootstrapVersion % scope exclude ("org.playframework", "play-json_2.13")
  )
}
