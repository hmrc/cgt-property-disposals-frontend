resolvers += Resolver.url("HMRC Sbt Plugin Releases", url("https://dl.bintray.com/hmrc/sbt-plugin-releases"))(
  Resolver.ivyStylePatterns
)

resolvers += "HMRC Releases" at "https://dl.bintray.com/hmrc/releases"

resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("uk.gov.hmrc"               % "sbt-auto-build"     % "2.10.0")
addSbtPlugin("uk.gov.hmrc"               % "sbt-git-versioning" % "2.1.0")
addSbtPlugin("uk.gov.hmrc"               % "sbt-artifactory"    % "1.6.0")
addSbtPlugin("uk.gov.hmrc"               % "sbt-distributables" % "2.0.0")
addSbtPlugin("com.typesafe.play"         % "sbt-plugin"         % "2.6.24")
addSbtPlugin("org.wartremover"           % "sbt-wartremover"    % "2.4.10")
addSbtPlugin("org.scalameta"             % "sbt-scalafmt"       % "2.4.0")
addSbtPlugin("org.scoverage"             % "sbt-scoverage"      % "1.6.1")
addSbtPlugin("ch.epfl.scala"             % "sbt-scalafix"       % "0.9.19")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat"       % "0.1.13")
