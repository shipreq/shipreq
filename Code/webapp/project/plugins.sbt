// Web app support plugin for XSbt using Jetty Web Server
addSbtPlugin("com.earldouglas" % "xsbt-web-plugin" % "0.4.2")

// SBT Eclipse
 addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.3.0")

// SBT IntelliJ Idea
addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.2")

// SBT Javascript Plugin
resolvers += Resolver.url("untyped", url("http://ivy.untyped.com"))(Resolver.ivyStylePatterns)

// SBT Javascript Plugin
addSbtPlugin("com.untyped" %% "sbt-js" % "0.6")

// Dependency graph
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4")

