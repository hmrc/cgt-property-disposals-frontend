import sbt.*

object AppDependencies {

  val playVersion      = "play-30"
  val bootstrapVersion = "8.3.0"
  val mongoVersion     = "1.4.0"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"                %% s"play-frontend-hmrc-$playVersion" % "8.3.0",
    "uk.gov.hmrc"                %% s"bootstrap-frontend-$playVersion" % bootstrapVersion,
    "uk.gov.hmrc.mongo"          %% s"hmrc-mongo-$playVersion"         % mongoVersion,
    "uk.gov.hmrc"                %% s"domain-$playVersion"             % "9.0.0",
    "org.typelevel"              %% "cats-core"                        % "2.9.0",
    "org.julienrf"               %% "play-json-derived-codecs"         % "10.1.0",
    "com.github.julien-truffaut" %% "monocle-core"                     % "2.1.0",
    "com.github.julien-truffaut" %% "monocle-macro"                    % "2.1.0"
  )

  def test(scope: String = "test"): Seq[ModuleID] = Seq(
    "org.jsoup"                   % "jsoup"                         % "1.15.4"         % scope,
    "org.scalamock"              %% "scalamock"                     % "5.2.0"          % scope,
    "org.scalatestplus"          %% "scalacheck-1-17"               % "3.2.16.0"       % scope,
    "uk.gov.hmrc.mongo"          %% s"hmrc-mongo-test-$playVersion" % mongoVersion     % scope,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.16"     % "1.3.1"          % scope,
    "uk.gov.hmrc"                %% s"bootstrap-test-$playVersion"  % bootstrapVersion % scope,
    "com.vladsch.flexmark"        % "flexmark-all"                  % "0.64.6"         % scope
  )
}
