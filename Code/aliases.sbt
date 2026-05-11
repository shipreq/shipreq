addCommandAlias("B"   , "project base")
addCommandAlias("BP"  , "project basePredefJVM")
addCommandAlias("BPJ" , "project basePredefJS")
addCommandAlias("BU"  , "project baseUtilJVM")
addCommandAlias("BUJ" , "project baseUtilJS")
addCommandAlias("BO"  , "project baseOps")
addCommandAlias("BD"  , "project baseDb")
addCommandAlias("BT"  , "project baseTestJVM")
addCommandAlias("BTJ" , "project baseTestJS")
addCommandAlias("T"   , "project taskman")
addCommandAlias("TAL" , "project taskmanApiLogic")
addCommandAlias("TA"  , "project taskmanApi")
addCommandAlias("TSL" , "project taskmanServerLogic")
addCommandAlias("TSS" , "project taskmanServerSchema")
addCommandAlias("TS"  , "project taskmanServer")
addCommandAlias("W"   , "project webapp")
addCommandAlias("WB"  , "project webappBaseJVM")
addCommandAlias("WBJ" , "project webappBaseJS")
addCommandAlias("WBT" , "project webappBaseTestJVM")
addCommandAlias("WBTJ", "project webappBaseTestJS")
addCommandAlias("WM"  , "project webappMemberJVM")
addCommandAlias("WMJ" , "project webappMemberJS")
addCommandAlias("WMT" , "project webappMemberTestJVM")
addCommandAlias("WMTJ", "project webappMemberTestJS")
addCommandAlias("WSD" , "project webappSampleDataJVM")
addCommandAlias("WSDJ", "project webappSampleDataJS")
addCommandAlias("WCA" , "project webappClientPublicJVM")
addCommandAlias("WCAJ", "project webappClientPublicJS")
addCommandAlias("WCL" , "project webappClientLoaders")
addCommandAlias("WCH" , "project webappClientHome")
addCommandAlias("WWA" , "project webappClientWwApi")
addCommandAlias("WW"  , "project webappClientWw")
addCommandAlias("WCP" , "project webappClientProject")
addCommandAlias("WSL" , "project webappServerLogicJVM")
addCommandAlias("WSLJ", "project webappServerLogicJS")
addCommandAlias("WS"  , "project webappServer")
addCommandAlias("BM"  , "project benchmarkJVM")
addCommandAlias("BMJ" , "project benchmarkJS")
addCommandAlias("U"   , "project utils")

// addCommandAlias("", "project webappMacroJVM")
// addCommandAlias("", "project webappMacroJS")
// addCommandAlias("", "project webappSsrJVM")
// addCommandAlias("", "project webappSsrJS")

addCommandAlias("verifySampleData",
  "webappSampleDataJVM/runMain shipreq.webapp.sampledata.VerifySampleData")

addCommandAlias("js",
  "webappServer/webappPrepare")

addCommandAlias("up",
  ";webappServer/Jetty/stop ;webappServer/Jetty/start")

addCommandAlias("d",
  "webappServer/Jetty/stop")

addCommandAlias("jsSizes",
  ";jsSizesFast ;jsSizesFull")

addCommandAlias("dockers",
  ";root/compile ;taskmanServer/docker ;webappServer/docker")

// See https://github.com/rtimush/sbt-updates#usage-as-project-plugin
addCommandAlias("deps",
  ";dependencyUpdates; reload plugins; dependencyUpdates; reload return")

addCommandAlias("wtt",
  ";webappServerLogicJVM/test ;webappServer/test")
