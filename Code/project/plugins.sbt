addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"              % "0.9.23")
addSbtPlugin("com.earldouglas"    % "xsbt-web-plugin"           % "4.2.1")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"               % "0.5.1")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                   % "1.0.0")
addSbtPlugin("com.typesafe.sbt"   % "sbt-native-packager"       % "1.7.6")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"  % "1.0.0")
addSbtPlugin("org.scala-js"       % "sbt-jsdependencies"        % "1.0.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs-env-phantomjs" % "1.0.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"               % "1.3.1")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                   % "0.4.0")
addSbtPlugin("se.marcuslonnberg"  % "sbt-docker"                % "1.8.0")

libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"
