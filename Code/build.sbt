name := "ShipReq"

ThisBuild / organization     := "com.beardedlogic.shipreq"
ThisBuild / organizationName := "Bearded Logic"
ThisBuild / shellPrompt      := ((s: State) => Project.extract(s).currentRef.project + "> ")
ThisBuild / startYear        := Some(2013)

// Allow ctrl-c to kill forked tasks without killing SBT
Global / cancelable := true

val root                  = ShipReqBuild.root

val base                  = ShipReqBuild.base
val basePredefJvm         = ShipReqBuild.basePredefJvm
val basePredefJs          = ShipReqBuild.basePredefJs
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
val webappMacroJVM        = WebappBuild.webappMacroJVM
val webappMacroJS         = WebappBuild.webappMacroJS
val webappBaseJVM         = WebappBuild.webappBaseJVM
val webappBaseJS          = WebappBuild.webappBaseJS
val webappBaseTestJVM     = WebappBuild.webappBaseTestJVM
val webappBaseTestJS      = WebappBuild.webappBaseTestJS
val webappMemberJVM       = WebappBuild.webappMemberJVM
val webappMemberJS        = WebappBuild.webappMemberJS
val webappMemberTestJVM   = WebappBuild.webappMemberTestJVM
val webappMemberTestJS    = WebappBuild.webappMemberTestJS
val webappSampleDataJVM   = WebappBuild.webappSampleDataJVM
val webappSampleDataJS    = WebappBuild.webappSampleDataJS
val webappClientPublicJVM = WebappBuild.webappClientPublicJVM
val webappClientPublicJS  = WebappBuild.webappClientPublicJS
val webappClientLoaders   = WebappBuild.webappClientLoaders
val webappClientHome      = WebappBuild.webappClientHome
val webappClientWwApi     = WebappBuild.webappClientWwApi
val webappClientWw        = WebappBuild.webappClientWw
val webappClientProject   = WebappBuild.webappClientProject
val webappSsrJVM          = WebappBuild.webappSsrJVM
val webappSsrJS           = WebappBuild.webappSsrJS
val webappServerLogicJVM  = WebappBuild.webappServerLogicJVM
val webappServerLogicJS   = WebappBuild.webappServerLogicJS
val webappServer          = WebappBuild.webappServer

val benchmarkJvm          = ShipReqBuild.benchmarkJvm
val benchmarkJs           = ShipReqBuild.benchmarkJs
val utils                 = ShipReqBuild.utils

Global / concurrentRestrictions += Tags.limit(CustomTags.Node, 2)
Global / concurrentRestrictions += Tags.limit(Tags.Test, 2)

ThisBuild / evictionErrorLevel := Level.Info
