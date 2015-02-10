addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.0")

// Web app support plugin for XSbt using Jetty Web Server
addSbtPlugin("com.earldouglas" % "xsbt-web-plugin" % "0.9.0")

// SBT Eclipse
// addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.3.0")

// SBT IntelliJ Idea
addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

// Dependency graph
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4")

// Builds Taskman dist
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")
