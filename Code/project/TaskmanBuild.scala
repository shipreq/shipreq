import sbt._
import sbt.Keys._
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport.Universal
import sbtdocker.DockerPlugin
import Common._
import Dependencies._
import ShipReqBuild._

object TaskmanBuild {

  lazy val taskman =
    project
      .configure(Common.jvmSettings)
      .aggregate(taskmanApiLogic, taskmanApi, taskmanServerLogic, taskmanServer, taskmanServerSchema)
      .dependsOn(taskmanApiLogic, taskmanApi, taskmanServerLogic, taskmanServer, taskmanServerSchema)

  lazy val taskmanApiLogic =
    project
      .in(file("taskman-api-logic"))
      .configure(Common.jvmSettings)
      .deps(Circe.main ++ testScope(μTest ++ scalaCheck ++ Scala.reflect ++ Microlibs.testUtil))
      .dependsOn(baseUtilJvm)
      .dependsOn(baseTestJvm % Test)

  lazy val taskmanApi =
    project
      .in(file("taskman-api"))
      .configure(Common.jvmSettings, DockerEnv.test)
      .deps(testScope(μTest ++ scalaCheck ++ Scala.reflect))
      .dependsOn(taskmanApiLogic, baseDb)
      .dependsOn(taskmanServerSchema % Test)
      .dependsOn(baseTestJvm % Test)
      .settings(Test / parallelExecution := false)

  lazy val taskmanServerLogic =
    project
      .in(file("taskman-server-logic"))
      .configure(Common.jvmSettings)
      .deps(Logback.withPlugins ++ testScope(μTest ++ scalaCheck))
      .dependsOn(taskmanApiLogic)
      .dependsOn(baseTestJvm % Test)

  lazy val taskmanServerSchema =
    project
      .in(file("taskman-server-schema"))
      .configure(Common.jvmSettings)
      .dependsOn(baseDb)

  object TaskmanServer {
    val serverClass = "shipreq.taskman.server.app.Server"

    val fixJarFilename = Def.setting((_: String) match {
      case n if n contains "shipreq" => n.replace("-" + version.value, "")
      case n => n
    })
  }

  lazy val taskmanServer: Project = {
    import TaskmanServer._

    // Integrate run/runMain with the Docker dev env
    def runWithDockerDev: Project => Project =
      _.configure(DockerEnv.dev.commands)
      .settings(
        Compile / run / fork           := true,
        Compile / run / javaOptions   ++= DockerEnv.dev.javaOptions("taskman", baseDirectory.value),
        Compile / run / runner         := (Compile / run / runner).dependsOn(DockerEnv.dev.devEnvStart).value,
        Runtime / fullClasspathAsJars  += DockerEnv.dev.resDir("taskman", baseDirectory.value),
      )

    Project("taskmanServer", file("taskman-server"))
      .enablePlugins(JavaAppPackaging, DockerPlugin)
      .configure(Common.jvmSettings, DockerEnv.test)
      .deps(
        Akka.actor ++ javaMail ++ OkHttp.core ++ httpCore ++ commonsIo ++ Logback.withPlugins ++
        Prometheus.client ++ Prometheus.hotspot ++ Prometheus.httpserver ++ Prometheus.logback ++
        testScope(Akka.testkit ++ μTest))
      .dependsOn(taskmanServerLogic, taskmanServerSchema, taskmanApi)
      .dependsOn(baseTestJvm % Test)
      .configure(Docker.settingsFor("taskman"))
      .configure(runWithDockerDev)
      .settings(
        dependencyOverrides ++= OkHttp.core(LibDependency.JVM), // because jaegerClient wants okhttp 4

        Compile / mainClass := Some(serverClass),
        Compile / run / javaOptions += "-XX:+UseG1GC",

        // Remove versions from package filenames for Docker layer reuse.
        Universal / mappings :=
          (Universal / mappings).value.map {
            case (f, n) => (f, fixJarFilename.value(n))
          },

        Test / parallelExecution := false)
      .configure(dontInline) // because Akka docs
  }

}