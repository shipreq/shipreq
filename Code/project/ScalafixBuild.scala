import sbt._
import sbt.Keys._
import scalafix.sbt.BuildInfo.{scalafixVersion => ScalafixVer}
import scalafix.sbt.ScalafixPlugin
import scalafix.sbt.ScalafixTestkitPlugin
import scalafix.sbt.ScalafixTestkitPlugin.autoImport._

object ScalafixBuild {

  private val settings: Project => Project =
    Common.settingsMinForScalafix.andThen(_
      .disablePlugins(ScalafixPlugin)
      .settings(scalacOptions ~= { _.filterNot(_ startsWith "-Yimports") })
    )

  private val testSettings: Project => Project =
    settings.andThen(
      _.settings(
        scalacOptions ~= { _.filterNot(_ matches "^-[WXY].*") },
        scalacOptions += "-P:semanticdb:synthetics:on",
        scalacOptions += "-Wconf:any:s"
      )
    )

  lazy val `scalafix-input` = (project in file("scalafix/input"))
    .configure(testSettings)

  lazy val `scalafix-output` = (project in file("scalafix/output"))
    .configure(testSettings)

  lazy val `scalafix-rules` = (project in file("scalafix/rules"))
    .configure(settings)
    .settings(libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % ScalafixVer)

  // Pending: https://github.com/scalacenter/scalafix/issues/1230
  lazy val `scalafix-tests` = (project in file("scalafix/tests"))
    .configure(settings)
    .settings(
      libraryDependencies                    += "ch.epfl.scala" % "scalafix-testkit" % ScalafixVer % Test cross CrossVersion.full,
      scalafixTestkitOutputSourceDirectories := (`scalafix-output` / Compile / sourceDirectories).value,
      scalafixTestkitInputSourceDirectories  := (`scalafix-input` / Compile / sourceDirectories).value,
      scalafixTestkitInputClasspath          := (`scalafix-input` / Compile / fullClasspath).value,
      scalafixTestkitInputScalacOptions      := (`scalafix-input` / Compile / scalacOptions).value,
      scalafixTestkitInputScalaVersion       := (`scalafix-input` / Compile / scalaVersion).value
    )
    .dependsOn(`scalafix-input`, `scalafix-rules`)
    .enablePlugins(ScalafixTestkitPlugin)

  def projects: Seq[ProjectReference] =
    if (Common.releaseMode)
      Nil
    else
      Seq(
        `scalafix-input`,
        `scalafix-output`,
        `scalafix-rules`,
        `scalafix-tests`)
}
