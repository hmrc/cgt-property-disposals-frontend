resolvers += "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/"
resolvers += "hmrc-releases" at "https://artefacts.tax.service.gov.uk/artifactory/hmrc-releases/"

resolvers += "HMRC-open-artefacts-maven" at "https://open.artefacts.tax.service.gov.uk/maven2"
resolvers += Resolver.url("HMRC-open-artefacts-ivy", url("https://open.artefacts.tax.service.gov.uk/ivy2"))(Resolver.ivyStylePatterns)
resolvers += Resolver.jcenterRepo

addSbtPlugin("uk.gov.hmrc"               % "sbt-auto-build"     % "3.4.0")
addSbtPlugin("uk.gov.hmrc"               % "sbt-distributables" % "2.1.0")
addSbtPlugin("com.typesafe.play"         % "sbt-plugin"         % "2.8.8")
addSbtPlugin("org.wartremover"           % "sbt-wartremover"    % "2.4.12")
addSbtPlugin("org.scalameta"             % "sbt-scalafmt"       % "2.4.0")
addSbtPlugin("org.scoverage"             % "sbt-scoverage"      % "1.8.0")
addSbtPlugin("ch.epfl.scala"             % "sbt-scalafix"       % "0.9.19")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat"       % "0.1.13")

