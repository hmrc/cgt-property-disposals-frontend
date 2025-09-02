import sbt.*

object AppDependencies {
  val playVersion      = "play-30"
  val bootstrapVersion = "9.13.0"
  val mongoVersion     = "2.7.0"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"       %% s"play-frontend-hmrc-$playVersion" % "12.8.0",
    "uk.gov.hmrc"       %% s"bootstrap-frontend-$playVersion" % bootstrapVersion,
    "uk.gov.hmrc.mongo" %% s"hmrc-mongo-$playVersion"         % mongoVersion,
    "uk.gov.hmrc"       %% s"domain-$playVersion"             % "12.1.0",
    "org.typelevel"     %% "cats-core"                        % "2.13.0",
    "dev.optics"        %% "monocle-core"                     % "3.3.0",
    "dev.optics"        %% "monocle-macro"                    % "3.3.0"
  )

  def test(scope: String = "test"): Seq[ModuleID] = Seq(
    "org.jsoup"           % "jsoup"                         % "1.21.1"         % scope,
    "org.scalamock"      %% "scalamock"                     % "7.4.0"          % scope,
    "org.scalatest"      %% "scalatest"                     % "3.2.19"         % scope,
    "org.scalacheck"     %% "scalacheck"                    % "1.18.1"         % scope,
    "io.github.martinhh" %% "scalacheck-derived"            % "0.10.0"          % scope,
    "org.scalatestplus"   % "scalacheck-1-18_3"             % "3.2.19.0"       % scope,
    "uk.gov.hmrc.mongo"  %% s"hmrc-mongo-test-$playVersion" % mongoVersion     % scope,
    "uk.gov.hmrc"        %% s"bootstrap-test-$playVersion"  % bootstrapVersion % scope exclude ("org.playframework", "play-json_2.13")
  )
}
