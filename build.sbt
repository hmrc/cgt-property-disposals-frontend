addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

lazy val microservice = Project("cgt-property-disposals-frontend", file("."))
  .enablePlugins(play.sbt.PlayScala, SbtDistributablesPlugin)
  .disablePlugins(JUnitXmlReportPlugin)
  .settings(
    scalaVersion := "2.13.16",
    majorVersion := 2,
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test(),
    onLoadMessage := "",
    PlayKeys.playDefaultPort := 7020,
    scalafmtOnCompile := true,
    // Disable default sbt Test options (might change with new versions of bootstrap)
    Test / testOptions -= Tests.Argument("-o", "-u", "target/test-reports", "-h", "target/test-reports/html-report"),
    // Suppress successful events in Scalatest in standard output (-o)
    // Options described here: https://www.scalatest.org/user_guide/using_scalatest_with_sbt
    Test / testOptions += Tests.Argument(
      TestFrameworks.ScalaTest,
      "-oNCHPQR",
      "-u", "target/test-reports",
      "-h", "target/test-reports/html-report"),
    scalacOptions ++= "-Wconf:src=routes/.*:s" :: "-Wconf:cat=unused-imports&src=html/.*:s"
                      :: "-Ymacro-annotations" :: "-Xlint:-byname-implicit" :: Nil,
  )
  .settings(CodeCoverageSettings.settings *)

libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
