val enableScalaRewrites = false

{
  if (Common.inCI)
    Nil
  else
    Seq(
      ThisBuild / scalacOptions              += "-P:semanticdb:synthetics:on",
      ThisBuild / scalacOptions              += "-Yrangepos",
      ThisBuild / semanticdbEnabled          := true,
      ThisBuild / scalafixScalaBinaryVersion := "2.13",
      ThisBuild / semanticdbVersion          := "4.3.22",

      ThisBuild / scalafixDependencies ++= Seq(
        "com.github.liancheng" %% "organize-imports" % "0.4.1"
      )
    )
}

{
  if (Common.releaseMode || !enableScalaRewrites)
    Nil
  else
    Seq(
      ThisBuild / scalafixDependencies += "org.scala-lang" %% "scala-rewrites" % "0.1.0-SNAPSHOT"
    )
}
