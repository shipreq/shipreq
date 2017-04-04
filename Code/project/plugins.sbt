addSbtPlugin("com.earldouglas"    % "xsbt-web-plugin"      % "3.0.1")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"          % "0.3.0")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"              % "0.8.5")
addSbtPlugin("com.typesafe.sbt"   % "sbt-native-packager"  % "1.1.5")
addSbtPlugin("net.virtual-void"   % "sbt-dependency-graph" % "0.8.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"          % "0.6.14")
addSbtPlugin("org.scoverage"      % "sbt-scoverage"        % "1.5.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"              % "0.2.22")
addSbtPlugin("se.marcuslonnberg"  % "sbt-docker"           % "1.4.1")

// Facilitates running Scala.JS tests in real browsers
libraryDependencies += "org.scala-js" %% "scalajs-env-selenium" % "0.1.3"

// https://github.com/JetBrains/sbt-ide-settings
resolvers += Resolver.url("jetbrains-bintray", url("http://dl.bintray.com/jetbrains/sbt-plugins/"))(Resolver.ivyStylePatterns)
addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "0.1.1")
