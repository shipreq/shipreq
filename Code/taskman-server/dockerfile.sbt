import com.typesafe.sbt.packager.{Keys => PackagerKeys}
import TaskmanBuild.TaskmanServer._

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

  val fixJarFilenameValue = fixJarFilename.value
  val classpath = PackagerKeys.scriptClasspath.value
    .map(n => fixJarFilenameValue(lib + n))
    .mkString(":")

  new Dockerfile {
    def runInBash(cmds: String*) = run("/bin/bash", "-c", cmds.mkString(";"))

    from(Docker.baseImage)

    env("NAME" -> "shipreq/taskman")

    workDir(root)

    expose(9031)

    jarTiers.foreach(copy(_, lib))

    copy(sourceDirectory.value / "docker", s"$root/")

    runInBash(
      s"sed -i 's|{{cp}}|$classpath|' $root/bin/run",
      s"sed -i 's|{{mainClass}}|$serverClass|' $root/bin/taskman")

    env(Docker.envVars.value: _*)

    cmd("bin/taskman")
  }
}