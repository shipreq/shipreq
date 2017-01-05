import sbt.{project => _, _}
import Keys._
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import com.typesafe.sbt.packager.{Keys => PackagerKeys}
import sbtdocker.DockerPlugin, DockerPlugin.autoImport._
import Common.Functions._
import Common.Values.{devMode, releaseMode}
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
        testScope(Specs2.combo ++ scalaCheck ++ Scala.reflect))
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

    def consoleCmds =
      """
        |import org.json4s._
        |import org.json4s.jackson.JsonMethods._
        |import org.json4s.JsonDSL._
      """.stripMargin

    def printFileBatches(batches: List[List[File]]): Unit = {
      val sep = "=" * 100
      println(sep)
      batches.foreach { files =>
        files.iterator.map(_.getName).foreach(println)
        println(sep)
      }
    }

    val serverClass = "shipreq.taskman.server.app.Server"

    project("taskman-server-impl")
      .enablePlugins(JavaAppPackaging, DockerPlugin)
      .configure(Common.settings, Common.jvmSettings)
      .deps(
        Akka.actor ++ javaMail ++ okHttp ++ httpCore ++
        testScope(Akka.testkit ++ Specs2.combo))
      .dependsOn(taskmanServerLogic, taskmanServerSchema, taskmanApi)
      .dependsOn(baseTestJvm % "test")
      .settings(
        initialCommands += consoleCmds,
        mainClass := Some(serverClass),
        buildOptions in docker := BuildOptions(pullBaseImage = BuildOptions.Pull.Always),

        imageNames in docker := {
          var versions = Seq(version.value, "latest")
          // if (!isSnapshot.value) versions :+= "latest"
          versions.map(ver => ImageName(s"shipreq/taskman:$ver"))
        },

        dockerfile in docker := {
          val root = "/taskman"
          val lib = s"$root/lib/"
          val stageDir = PackagerKeys.stage.value
          val jars = (stageDir / "lib").listFiles().toList
          val jarTiers: List[List[File]] =
            jars.groupBy(_.getName match {
              case f if f contains   "taskman-server-impl"        => 92
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
          printFileBatches(jarTiers)

          val classpath = PackagerKeys.scriptClasspath.value.map(lib + _).mkString(":")

          new Dockerfile {
            def runInBash(cmds: String*) = run("/bin/bash", "-c", cmds.mkString(";"))

            from("anapsix/alpine-java:8_server-jre_unlimited")
            workDir(root)
            jarTiers.foreach(copy(_, lib))
            copy(sourceDirectory.value / "docker", s"$root/")
            runInBash(
              s"sed -i 's|{{cp}}|$classpath|' $root/bin/run",
              s"sed -i 's|{{mainClass}}|$serverClass|' $root/bin/taskman")
            env(
              "VERSION" -> version.value,
              "BUILD_MODE" -> (if (releaseMode) "release" else "dev"))
            cmd("bin/taskman")
          }
        },

        parallelExecution in Test := false)
      .configure(dontInline) // because Akka docs + crashes scalac 2.11.2
  }

  lazy val taskmanServer =
    project("taskman-server")
      .configure(Common.settings, Common.jvmSettings)
      .aggregate(taskmanServerLogic, taskmanServerImpl, taskmanServerSchema)
      .dependsOn(taskmanServerLogic, taskmanServerImpl, taskmanServerSchema)

}