name      := "ShipReq"
startYear := Some(2013)

val root                 = ShipReqBuild.root

val base                 = ShipReqBuild.base
val baseUtilJvm          = ShipReqBuild.baseUtilJvm
val baseUtilJs           = ShipReqBuild.baseUtilJs
val baseDb               = ShipReqBuild.baseDb
val baseTestJvm          = ShipReqBuild.baseTestJvm
val baseTestJs           = ShipReqBuild.baseTestJs

val taskman              = TaskmanBuild.taskman
val taskmanApiLogic      = TaskmanBuild.taskmanApiLogic
val taskmanApiImpl       = TaskmanBuild.taskmanApiImpl
val taskmanApi           = TaskmanBuild.taskmanApi
val taskmanServerLogic   = TaskmanBuild.taskmanServerLogic
val taskmanServerSchema  = TaskmanBuild.taskmanServerSchema
val taskmanServerImpl    = TaskmanBuild.taskmanServerImpl
val taskmanServer        = TaskmanBuild.taskmanServer

val webapp               = WebappBuild.webapp
val webappMacroJvm       = WebappBuild.webappMacroJvm
val webappMacroJs        = WebappBuild.webappMacroJs
val webappBaseJvm        = WebappBuild.webappBaseJvm
val webappBaseJs         = WebappBuild.webappBaseJs
val webappBaseServerJvm  = WebappBuild.webappBaseServerJvm
val webappBaseServerJs   = WebappBuild.webappBaseServerJs
val webappBaseTestJvm    = WebappBuild.webappBaseTestJvm
val webappBaseTestJs     = WebappBuild.webappBaseTestJs
val webappClient         = WebappBuild.webappClient
val webappClientBase     = WebappBuild.webappClientBase
val webappClientBaseTest = WebappBuild.webappClientBaseTest
val webappClientHome     = WebappBuild.webappClientHome
val webappClientWwApi    = WebappBuild.webappClientWwApi
val webappClientWw       = WebappBuild.webappClientWw
val webappClientProject  = WebappBuild.webappClientProject
val webappGenJvm         = WebappBuild.webappGenJvm
val webappGenJs          = WebappBuild.webappGenJs
val webappServer         = WebappBuild.webappServer

val benchmarkJvm         = ShipReqBuild.benchmarkJvm
val benchmarkJs          = ShipReqBuild.benchmarkJs
val utils                = ShipReqBuild.utils

