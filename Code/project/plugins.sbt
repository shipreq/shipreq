addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"              % "0.9.18-1")
addSbtPlugin("com.earldouglas"    % "xsbt-web-plugin"           % "4.1.0")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                   % "1.0.0")
addSbtPlugin("com.typesafe.sbt"   % "sbt-native-packager"       % "1.6.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"  % "1.0.0")
addSbtPlugin("org.scala-js"       % "sbt-jsdependencies"        % "1.0.1")
addSbtPlugin("org.scala-js"       % "sbt-scalajs-env-phantomjs" % "1.0.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"               % "1.1.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                   % "0.3.7")
addSbtPlugin("se.marcuslonnberg"  % "sbt-docker"                % "1.5.0")

libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"
