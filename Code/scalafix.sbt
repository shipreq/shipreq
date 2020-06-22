{
  if (Common.releaseMode)
    Nil
  else
    Seq(

      ThisBuild / scalacOptions += "-Yrangepos",

      ThisBuild / semanticdbEnabled := true,

      ThisBuild / semanticdbVersion := "4.3.14",

      ThisBuild / scalafixDependencies ++= Seq(
        "com.github.liancheng" %% "organize-imports" % "0.3.1-RC3"
      )

    )
}
