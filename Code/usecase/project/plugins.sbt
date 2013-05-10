// Web app support plugin for XSbt using Jetty Web Server
libraryDependencies <+= sbtVersion(v => v match {
  case "0.11.0" => "com.github.siasia" %% "xsbt-web-plugin" % "0.11.0-0.2.8"
  case "0.11.1" => "com.github.siasia" %% "xsbt-web-plugin" % "0.11.1-0.2.10"
  case "0.11.2" => "com.github.siasia" %% "xsbt-web-plugin" % "0.11.2-0.2.11"
  case "0.11.3" => "com.github.siasia" %% "xsbt-web-plugin" % "0.11.3-0.2.11.1"
  case x if x startsWith "0.12" =>
    "com.github.siasia" %% "xsbt-web-plugin" % "0.12.0-0.2.11.1"
})

// SBT Eclipse
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.2")

// SBT IntelliJ Idea
addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.4.0")

