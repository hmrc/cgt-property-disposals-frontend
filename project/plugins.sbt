resolvers += MavenRepository("HMRC-open-artefacts-maven2", "https://open.artefacts.tax.service.gov.uk/maven2")
resolvers += Resolver.url("HMRC-open-artefacts-ivy2", url("https://open.artefacts.tax.service.gov.uk/ivy2"))(
  Resolver.ivyStylePatterns
)
resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("uk.gov.hmrc"       %% "sbt-auto-build"        % "3.15.0")
addSbtPlugin("uk.gov.hmrc"       %% "sbt-distributables"    % "2.4.0")
addSbtPlugin("com.typesafe.play" %% "sbt-plugin"            % "2.8.19")
addSbtPlugin("org.scalameta"     %% "sbt-scalafmt"          % "2.4.0")
addSbtPlugin("org.scoverage"     %% "sbt-scoverage"         % "2.0.9")
addSbtPlugin("ch.epfl.scala"     %% "sbt-scalafix"          % "0.11.0")
addSbtPlugin("org.scalastyle"    %% "scalastyle-sbt-plugin" % "1.0.0")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
