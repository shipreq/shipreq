import ScalaJSKeys._

scalaJSSettings

name := "Scala.js experiment"

// scalaVersion := "2.10.4"

scalaVersion := "2.11.1"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-language:_" )

libraryDependencies ++= Seq(
  "com.github.japgolly.scalajs-react" %%% "scalajs-react" % "0.2.0"
  ,"com.github.japgolly.fork.monocle" %%% "monocle-core" % "0.4.0"
  //,"com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.0-RC1"
  ,"com.github.japgolly.fork.scalaz" %%% "scalaz-effect" % "7.0.6"
)

jsDependencies += "org.webjars" % "react" % "0.11.0" / "react-with-addons.js"

skip in packageJSDependencies := false

//==============================================================================

workbenchSettings

bootSnippet := "golly.Golly().main();"

refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)

