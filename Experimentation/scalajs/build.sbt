import ScalaJSKeys._

import scala.scalajs.sbtplugin.env.nodejs.NodeJSEnv

import scala.scalajs.sbtplugin.env.phantomjs.PhantomJSEnv

scalaJSSettings

name := "Scala.js experiment"

// scalaVersion := "2.10.4"

scalaVersion := "2.11.2"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-language:_" )

libraryDependencies ++= Seq(
  "com.github.japgolly.scalajs-react" %%% "scalajs-react" % "0.2.0"
  ,"com.github.japgolly.fork.monocle" %%% "monocle-core" % "0.4.0"
  //,"com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.0-RC1"
  ,"com.github.japgolly.fork.scalaz" %%% "scalaz-effect" % "7.0.6"
  ,"com.lihaoyi" %%% "utest" % "0.1.8" % "test"
)

jsDependencies += "org.webjars" % "react" % "0.11.1" / "react-with-addons.js" commonJSName "React"

skip in packageJSDependencies := false

//checkScalaJSIR := true

inliningMode := scala.scalajs.sbtplugin.InliningMode.Off

jsEnv in Test := new PhantomJSEnv

utest.jsrunner.Plugin.utestJsSettings

requiresDOM := true

//==============================================================================

workbenchSettings

bootSnippet := "golly.Golly().main();"

refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)

