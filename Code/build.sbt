name      := "ShipReq"
startYear := Some(2013)

// Allow ctrl-c to kill forked tasks without killing SBT
cancelable in Global := true

val root                  = ShipReqBuild.root
val js                    = ShipReqBuild.js

val base                  = ShipReqBuild.base
val baseUtilJvm           = ShipReqBuild.baseUtilJvm
val baseUtilJs            = ShipReqBuild.baseUtilJs
val baseOps               = ShipReqBuild.baseOps
val baseDb                = ShipReqBuild.baseDb
val baseTestJvm           = ShipReqBuild.baseTestJvm
val baseTestJs            = ShipReqBuild.baseTestJs

val taskman               = TaskmanBuild.taskman
val taskmanApiLogic       = TaskmanBuild.taskmanApiLogic
val taskmanApi            = TaskmanBuild.taskmanApi
val taskmanServerLogic    = TaskmanBuild.taskmanServerLogic
val taskmanServerSchema   = TaskmanBuild.taskmanServerSchema
val taskmanServer         = TaskmanBuild.taskmanServer

val webapp                = WebappBuild.webapp
val webappMacroJvm        = WebappBuild.webappMacroJvm
val webappMacroJs         = WebappBuild.webappMacroJs
val webappBaseJvm         = WebappBuild.webappBaseJvm
val webappBaseJs          = WebappBuild.webappBaseJs
val webappBaseMemberJvm   = WebappBuild.webappBaseMemberJvm
val webappBaseMemberJs    = WebappBuild.webappBaseMemberJs
val webappBaseTestJvm     = WebappBuild.webappBaseTestJvm
val webappBaseTestJs      = WebappBuild.webappBaseTestJs
val webappClientPublicJvm = WebappBuild.webappClientPublicJvm
val webappClientPublicJs  = WebappBuild.webappClientPublicJs
val webappClientLoaders   = WebappBuild.webappClientLoaders
val webappClientHome      = WebappBuild.webappClientHome
val webappClientWwApi     = WebappBuild.webappClientWwApi
val webappClientWw        = WebappBuild.webappClientWw
val webappClientProject   = WebappBuild.webappClientProject
val webappSsrJvm          = WebappBuild.webappSsrJvm
val webappSsrJs           = WebappBuild.webappSsrJs
val webappServerLogicJvm  = WebappBuild.webappServerLogicJvm
val webappServerLogicJs   = WebappBuild.webappServerLogicJs
val webappServer          = WebappBuild.webappServer

val benchmarkJvm          = ShipReqBuild.benchmarkJvm
val benchmarkJs           = ShipReqBuild.benchmarkJs
val utils                 = ShipReqBuild.utils
