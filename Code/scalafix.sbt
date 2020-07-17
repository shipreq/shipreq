val enableScalaRewrites = false

{
  if (Common.releaseMode)
    Nil
  else
    Seq(

      ThisBuild / scalacOptions += "-Yrangepos",

      ThisBuild / semanticdbEnabled := true,

      ThisBuild / semanticdbVersion := "4.3.20",

      ThisBuild / scalafixDependencies ++= Seq(
        "com.github.liancheng" %% "organize-imports" % "0.3.1-RC3"
      )

    )
}

{
  if (Common.releaseMode || !enableScalaRewrites)
    Nil
  else
    Seq(
      ThisBuild / scalacOptions += "-P:semanticdb:synthetics:on",
      ThisBuild / scalafixDependencies += "org.scala-lang" %% "scala-rewrites" % "0.1.0-SNAPSHOT"
    )
}
