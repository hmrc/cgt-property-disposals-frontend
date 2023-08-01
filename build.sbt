import scoverage.ScoverageKeys
import uk.gov.hmrc.DefaultBuildSettings.integrationTestSettings

val appName = "cgt-property-disposals-frontend"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("fix", "all compile:scalafix test:scalafix")

lazy val scoverageSettings =
  Seq(
    ScoverageKeys.coverageExcludedPackages := "<empty>;.*Reverse.*;.*(config|testonly|views).*;.*(BuildInfo|Routes).*",
    ScoverageKeys.coverageMinimumStmtTotal := 88.00,
    ScoverageKeys.coverageFailOnMinimum := true,
    ScoverageKeys.coverageHighlighting := true
  )

lazy val microservice = Project(appName, file("."))
  .enablePlugins(play.sbt.PlayScala, SbtDistributablesPlugin)
  .disablePlugins(JUnitXmlReportPlugin)
  .settings(addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"))
  .settings(addCompilerPlugin(scalafixSemanticdb))
  .settings(scalaVersion := "2.13.8")
  .settings(
    majorVersion := 2,
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test
  )
  .settings(routesImport := Seq.empty)
  .settings(TwirlKeys.templateImports := Seq.empty)
  .settings(
    scalacOptions ++= Seq(
      "-Yrangepos",
      "-language:postfixOps",
      "-Ymacro-annotations",
    ),
    scalacOptions -= "-Xlint:byname-implicit",
    scalacOptions -= "-Ywarn-by-name",
    scalacOptions -= "-Werror",
    scalacOptions -= "-Xfatal-warnings",
    scalacOptions += "-Xnon-strict-patmat-analysis",
    Test / scalacOptions --= Seq("-Ywarn-value-discard")
  )
  .configs(IntegrationTest)
  .settings(integrationTestSettings(): _*)
  .settings(scoverageSettings: _*)
  .settings(PlayKeys.playDefaultPort := 7020)
  .settings(scalafmtOnCompile := true)
