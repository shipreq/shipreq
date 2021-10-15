{
  if (Common.inCI)
    Nil
  else
    Seq(
      ThisBuild / scalacOptions              += "-P:semanticdb:synthetics:on",
      ThisBuild / scalacOptions              += "-Yrangepos",
      ThisBuild / semanticdbEnabled          := true,
      ThisBuild / scalafixScalaBinaryVersion := "2.13",
      ThisBuild / semanticdbVersion          := "4.4.28",

      ThisBuild / scalafixDependencies ++= Seq(
        "com.github.liancheng" %% "organize-imports" % "0.5.0"
      )
    )
}
