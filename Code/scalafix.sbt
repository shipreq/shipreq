{
  if (Common.inCI)
    Nil
  else
    Seq(
      ThisBuild / scalacOptions     += "-P:semanticdb:synthetics:on",
      ThisBuild / scalacOptions     += "-Yrangepos",
      ThisBuild / semanticdbEnabled := true,
      ThisBuild / semanticdbVersion := "4.8.4",
    )
}
