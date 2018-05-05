import sbt.{project => _, _}
import Keys._
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import com.typesafe.sbt.packager.{Keys => PackagerKeys}
import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport.Universal
import sbtdocker.DockerPlugin, DockerPlugin.autoImport._
import Common._
import Dependencies._
import ShipReqBuild._

object TaskmanBuild {

  lazy val taskman =
    project("taskman")
      .configure(Common.jvmSettings)
      .aggregate(taskmanApiLogic, taskmanApi, taskmanServerLogic, taskmanServer, taskmanServerSchema)
      .dependsOn(taskmanApiLogic, taskmanApi, taskmanServerLogic, taskmanServer, taskmanServerSchema)

  lazy val taskmanApiLogic =
    project("taskman-api-logic")
      .configure(Common.jvmSettings)
      .deps(testScope(μTest ++ scalaCheck ++ Scala.reflect ++ Microlibs.testUtil))
      .dependsOn(baseUtilJvm)

  lazy val taskmanApi =
    project("taskman-api")
      .configure(Common.jvmSettings, DockerEnv.test.required)
      .deps(
        Json4s.jackson ++
        testScope(Specs2.combo ++ scalaCheck ++ Scala.reflect))
      .dependsOn(taskmanApiLogic, baseDb)
      .dependsOn(taskmanServerSchema % Test)
      .dependsOn(baseTestJvm % Test)
    .settings(fork in Test := true) // else modules using specs2 v3+ seem to interfere with each other

  lazy val taskmanServerLogic =
    project("taskman-server-logic")
      .configure(Common.jvmSettings)
      .deps(Logback.withPlugins ++ testScope(Specs2.combo))
      .dependsOn(taskmanApiLogic)
      .dependsOn(baseTestJvm % Test)

  lazy val taskmanServerSchema =
    project("taskman-server-schema")
      .configure(Common.jvmSettings)
      .dependsOn(baseDb)

  lazy val taskmanServer: Project = {

    def consoleCmds =
      """
        |import org.json4s._
        |import org.json4s.jackson.JsonMethods._
        |import org.json4s.JsonDSL._
      """.stripMargin

    val serverClass = "shipreq.taskman.server.app.Server"

    val fixJarFilename = Def.setting((_: String) match {
      case n if n contains "shipreq" => n.replace("-" + version.value, "")
      case n => n
    })

    // Integrate run/runMain with the Docker dev env
    def runWithDockerDev: Project => Project =
      _.configure(DockerEnv.dev.commands)
      .settings(
        fork          in (Compile, run)  := true,
        fullClasspath in Runtime         += DockerEnv.dev.resDir("taskman", baseDirectory.value),
        javaOptions   in (Compile, run) ++= DockerEnv.dev.javaOptions("taskman", baseDirectory.value),
        runner        in (Compile, run)  := (runner in (Compile, run)).dependsOn(DockerEnv.dev.devEnvStart).value)

    project("taskman-server")
      .enablePlugins(JavaAppPackaging, DockerPlugin)
      .configure(Common.jvmSettings, DockerEnv.test.required)
      .deps(
        Akka.actor ++ javaMail ++ OkHttp.core ++ httpCore ++ commonsIo ++ Logback.withPlugins ++
        testScope(Akka.testkit ++ Specs2.combo))
      .dependsOn(taskmanServerLogic, taskmanServerSchema, taskmanApi)
      .dependsOn(baseTestJvm % Test)
      .configure(Common.dockerBaseSettings("taskman"))
      .configure(runWithDockerDev)
      .settings(
        initialCommands += consoleCmds,
        mainClass := Some(serverClass),
        javaOptions in(Compile, run) += "-XX:+UseG1GC", // Default in Java 9, may as well use it now

        // Remove versions from package filenames for Docker layer reuse.
        mappings in Universal :=
          (mappings in Universal).value.map {
            case (f, n) => (f, fixJarFilename.value(n))
          },

        dockerfile in docker := {
          val root = "/taskman"
          val lib = s"$root/lib/"
          val stageDir = PackagerKeys.stage.value
          val jars = (stageDir / "lib").listFiles().toList
          val jarTiers: List[List[File]] =
            jars.groupBy(_.getName match {
              case f if f contains   "taskman-server"             => 92
              case f if f contains   "taskman-server-logic"       => 91
              case f if f contains   "taskman"                    => 90
              case f if f contains   "shipreq"                    => 80
              case f if f contains   "japgolly"                   => 70
              case f if f matches    "^org.scala-lang.scalap?-.*" => 0
              case f if f startsWith "org.scala-lang."            => 1
              case _                                              => 50
            })
            .toList
            .sortBy(_._1)
            .map(_._2.sortBy(_.getName))
          // printFileBatches(jarTiers)

          val classpath = PackagerKeys.scriptClasspath.value
            .map(n => fixJarFilename.value(lib + n))
            .mkString(":")

          new Dockerfile {
            def runInBash(cmds: String*) = run("/bin/bash", "-c", cmds.mkString(";"))

            from(Dependencies.Docker.baseImage)
            workDir(root)
            jarTiers.foreach(copy(_, lib))
            copy(sourceDirectory.value / "docker", s"$root/")
            runInBash(
              s"sed -i 's|{{cp}}|$classpath|' $root/bin/run",
              s"sed -i 's|{{mainClass}}|$serverClass|' $root/bin/taskman")
            env(Common.dockerBaseEnv.value: _*)
            cmd("bin/taskman")
          }
        },

        fork in Test := true, // else modules using specs2 v3+ seem to interfere with each other
        parallelExecution in Test := false)
      .configure(dontInline) // because Akka docs
  }

}