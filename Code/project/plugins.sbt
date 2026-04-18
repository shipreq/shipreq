addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.9.31")
addSbtPlugin("com.earldouglas"    % "xsbt-web-plugin"          % "4.2.4")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"              % "0.6.0")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                  % "1.0.2")
addSbtPlugin("com.typesafe.sbt"   % "sbt-native-packager"      % "1.8.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")
addSbtPlugin("org.scala-js"       % "sbt-jsdependencies"       % "1.0.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.8.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.4.3")
addSbtPlugin("se.marcuslonnberg"  % "sbt-docker"               % "1.8.2")

libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.1"
