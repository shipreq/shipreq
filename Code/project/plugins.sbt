addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.10")

libraryDependencies += "org.scala-js" %% "scalajs-env-selenium" % "0.1.3"

// Web app support plugin for XSbt using Jetty Web Server
addSbtPlugin("com.earldouglas" % "xsbt-web-plugin" % "2.0.4")

// SBT Eclipse
// addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.3.0")

// SBT IntelliJ Idea
// addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

// Dependency graph
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")

// Builds Taskman dist
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")

// Code coverage (JVM projects only)
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")

// Benchmarking
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.2")

// Shows new versions available of dependencies
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.10")

// https://github.com/JetBrains/sbt-ide-settings
resolvers += Resolver.url("jetbrains-bintray", url("http://dl.bintray.com/jetbrains/sbt-plugins/"))(Resolver.ivyStylePatterns)

addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "0.1.1")

