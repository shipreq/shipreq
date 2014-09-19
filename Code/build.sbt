name := "ShipReq"

startYear := Some(2013)

initialize ~= { _ =>
  sys.props("scalac.patmat.analysisBudget") = "off"
}

// For jawn required by upickle
resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"
