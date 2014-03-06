name := "ShipReq"

organization := "com.beardedlogic.shipreq"

organizationName := "Bearded Logic"

startYear := Some(2013)

scalaVersion := "2.10.3"

initialize ~= { _ =>
  sys.props("scalac.patmat.analysisBudget") = "off"
}
