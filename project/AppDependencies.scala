import sbt.*

object AppDependencies {
  val playVersion      = "play-30"
  val bootstrapVersion = "9.11.0"
  val mongoVersion     = "2.6.0"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"                %% s"play-frontend-hmrc-$playVersion" % "8.5.0",
    "uk.gov.hmrc"                %% s"bootstrap-frontend-$playVersion" % bootstrapVersion,
    "uk.gov.hmrc.mongo"          %% s"hmrc-mongo-$playVersion"         % mongoVersion,
    "uk.gov.hmrc"                %% s"domain-$playVersion"             % "10.0.0",
    "org.typelevel"              %% "cats-core"                        % "2.12.0",
    "org.julienrf"               %% "play-json-derived-codecs"         % "11.0.0",
    "com.github.julien-truffaut" %% "monocle-core"                     % "2.1.0",
    "com.github.julien-truffaut" %% "monocle-macro"                    % "2.1.0"
  )

  def test(scope: String = "test"): Seq[ModuleID] = Seq(
    "org.jsoup"                   % "jsoup"                         % "1.18.3"         % scope,
    "org.scalamock"              %% "scalamock"                     % "5.2.0"          % scope,
    "org.scalatestplus"          %% "scalacheck-1-18"               % "3.2.19.0"       % scope,
    "uk.gov.hmrc.mongo"          %% s"hmrc-mongo-test-$playVersion" % mongoVersion     % scope,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.16"     % "1.3.1"          % scope,
    "uk.gov.hmrc"                %% s"bootstrap-test-$playVersion"  % bootstrapVersion % scope
  )
}
