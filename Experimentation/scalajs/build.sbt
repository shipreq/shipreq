import ScalaJSKeys._

import scala.scalajs.sbtplugin.env.nodejs.NodeJSEnv

import scala.scalajs.sbtplugin.env.phantomjs.PhantomJSEnv

scalaJSSettings

name := "Scala.js experiment"

// scalaVersion := "2.10.4"

scalaVersion := "2.11.2"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-language:_" )

libraryDependencies ++= Seq(
   "com.github.japgolly.scalajs-react" %%% "core"          % "0.5.0-SNAPSHOT"
  ,"com.github.japgolly.scalajs-react" %%% "test"          % "0.5.0-SNAPSHOT" % "test"
  ,"com.github.japgolly.scalajs-react" %%% "ext-scalaz71"  % "0.5.0-SNAPSHOT"
  ,"com.github.japgolly.fork.scalaz"   %%% "scalaz-effect" % "7.1.0-4"
  ,"com.github.japgolly.fork.monocle"  %%% "monocle-core"  % "0.5.1"
  ,"com.lihaoyi" %%% "utest" % "0.2.3" % "test"
)

jsDependencies += "org.webjars" % "react" % "0.11.1" / "react-with-addons.js" commonJSName "React"

skip in packageJSDependencies := false

//checkScalaJSIR := true

//inliningMode := scala.scalajs.sbtplugin.InliningMode.Off

jsEnv in Test := {
  import scala.scalajs.sbtplugin.env.ExternalJSEnv.RunJSArgs
  import scala.scalajs.tools.io._
  new PhantomJSEnv {
    override protected def initFiles(args: RunJSArgs): Seq[VirtualJSFile] = Seq(
      new MemVirtualJSFile("bindPolyfill.js").withContent(Polyfills.functionBind3)
    )
  }
}

utest.jsrunner.Plugin.utestJsSettings

requiresDOM := true

//==============================================================================

workbenchSettings

bootSnippet := "golly.Golly().main();"

refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)

