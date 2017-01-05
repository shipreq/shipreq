import sbt.{project => _, _}
import Keys._
import Common.Functions._
import Dependencies._
import ShipReqBuild._

object TaskmanBuild {

  lazy val taskman =
    project("taskman")
      .configure(Common.settings)
      .aggregate(taskmanApi, taskmanServer)
      .dependsOn(taskmanApi, taskmanServer)

  lazy val taskmanApiLogic =
    project("taskman-api-logic")
      .configure(Common.settings, Common.jvmSettings)
      .deps(
        Scalaz.core ++ Scalaz.effect ++
        testScope(Specs2.combo ++ scalaCheck ++ Scala.reflect)
      )
      .dependsOn(baseUtilJvm)

  lazy val taskmanApiImpl =
    project("taskman-api-impl")
      .configure(Common.settings, Common.jvmSettings)
      .deps(
        Json4s.jackson ++
        testScope(Specs2.combo ++ scalaCheck ++ Scala.reflect))
      .dependsOn(taskmanApiLogic, baseDb)
      .dependsOn(taskmanServerSchema % "test")
      .dependsOn(baseTestJvm % "test")
      //.dependsOn(baseUtilJvm) // Stupid IDEA auto-import needs this

  lazy val taskmanApi =
    project("taskman-api")
      .configure(Common.settings, Common.jvmSettings)
      .aggregate(taskmanApiLogic, taskmanApiImpl)
      .dependsOn(taskmanApiLogic, taskmanApiImpl)

  lazy val taskmanServerLogic =
    project("taskman-server-logic")
      .configure(Common.settings, Common.jvmSettings)
      .deps(logback ++ testScope(Specs2.combo))
      .dependsOn(taskmanApiLogic)
      .dependsOn(baseTestJvm % "test")
      .configure(dontInline) // crashes scalac 2.11.2

  lazy val taskmanServerSchema =
    project("taskman-server-schema")
      .configure(Common.settings, Common.jvmSettings)
      .dependsOn(baseDb)

  lazy val taskmanServerImpl = {
    import sbtassembly.Plugin._
    import AssemblyKeys._

    def consoleCmds =
      """
        |import org.json4s._
        |import org.json4s.jackson.JsonMethods._
        |import org.json4s.JsonDSL._
      """.stripMargin

    project("taskman-server-impl")
      .configure(Common.settings, Common.jvmSettings, Dependencies.useKindProjector)
      .deps(
        Akka.actor ++ javaMail ++ okHttp ++ httpCore ++
        testScope(Akka.testkit ++ Specs2.combo)
      )
      .dependsOn(taskmanServerLogic, taskmanServerSchema, taskmanApi)
      .dependsOn(baseTestJvm % "test")
      .settings(assemblySettings: _*)
      .settings(
        initialCommands += consoleCmds,
        parallelExecution in Test := false,
        test in assembly := {}) // Disable tests during assembly
      .configure(dontInline) // because Akka docs + crashes scalac 2.11.2
  }

  lazy val taskmanServer =
    project("taskman-server")
      .configure(Common.settings, Common.jvmSettings)
      .aggregate(taskmanServerLogic, taskmanServerImpl, taskmanServerSchema)
      .dependsOn(taskmanServerLogic, taskmanServerImpl, taskmanServerSchema)

}