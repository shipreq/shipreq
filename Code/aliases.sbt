addCommandAlias("B"  , "project base")
addCommandAlias("BU" , "project base-util-jvm")
addCommandAlias("BT" , "project base-test-jvm")
addCommandAlias("T"  , "project taskman")
addCommandAlias("W"  , "project webapp")
addCommandAlias("TA" , "project taskman-api")
addCommandAlias("TAL", "project taskman-api-logic")
addCommandAlias("TS" , "project taskman-server")
addCommandAlias("TSL", "project taskman-server-logic")
addCommandAlias("WB" , "project webapp-base-jvm")
addCommandAlias("WBM", "project webapp-base-member-jvm")
addCommandAlias("WT" , "project webapp-base-test-jvm")
addCommandAlias("WC" , "project webapp-client")
addCommandAlias("WCA", "project webapp-client-public-js")
addCommandAlias("WCB", "project webapp-client-base")
addCommandAlias("WCH", "project webapp-client-home")
addCommandAlias("WCP", "project webapp-client-project")
addCommandAlias("WW" , "project webapp-client-ww")
addCommandAlias("SD" , "project webapp-sampledata-jvm")
addCommandAlias("WSL", "project webapp-server-logic-jvm")
addCommandAlias("WS" , "project webapp-server")
addCommandAlias("BM" , "project benchmark-jvm")
addCommandAlias("BMJ", "project benchmark-js")
addCommandAlias("U"  , "project utils")

addCommandAlias("verifySampleData",
  "webapp-sampledata-jvm/runMain shipreq.webapp.sampledata.VerifySampleData")

addCommandAlias("js",
  "webapp-server/webappPrepare")

addCommandAlias("up",
  ";webapp-server/jetty:stop ;webapp-server/jetty:start")

addCommandAlias("d",
  "webapp-server/jetty:stop")

addCommandAlias("jsSizes",
  ";jsSizesFast ;jsSizesFull")

addCommandAlias("dockers",
  ";root/compile ;taskman-server/docker ;webapp-server/docker")

// See https://github.com/rtimush/sbt-updates#usage-as-project-plugin
addCommandAlias("deps",
  ";dependencyUpdates; reload plugins; dependencyUpdates; reload return")
