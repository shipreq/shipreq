import sbt._, Keys._
import Common.releaseMode

object DockerEnv {
  import sys.process._

  def javaOptionsFromDockerComposeEnv(serviceName: String, envRoot: File, filename: String = "docker-compose.yml"): List[String] = {
    val service = s"  $serviceName:"
    val envVar = "\\$\\{?([A-Za-z0-9_]+)\\}?".r
    def processEntry(e: String) =
      envVar.replaceAllIn(e, m => envFileValue(envRoot, m group 1))
    var inService = false
    var inEnv = false
    val b = List.newBuilder[String]
    IO.readLines(envRoot / filename) foreach {
      case `service`                                           => inService = true
      case s if s.matches("^  [a-z].*:")                       => inService = false; inEnv = false
      case "    environment:"                                  => inEnv = true
      case s if s.matches("^    [a-z].*:")                     => inEnv = false
      case s if inService && inEnv && s.startsWith("      - ") => b += "-D" + processEntry(s.drop(8).trim)
      case _                                                   => ()
    }
    b.result()
  }

  def javaOptionsFromProps(props: File): List[String] =
    javaOptionsFromProps(IO readLines props)

  def javaOptionsFromProps(props: List[String]): List[String] =
    props
      .iterator
      .map(_.replaceFirst(" *#.+", "").trim.replaceFirst(" *= *", "="))
      .filter(_.nonEmpty)
      .map("-D" + _)
      .toList

  def dockerEnvsRoot(baseDirectory: File): File =
    baseDirectory / "../docker"

  def envFileValue(envRoot: File, key: String): String = {
    val k = key + "="
    val f = envRoot / ".env"
    IO.readLines(f)
      .find(_.startsWith(k))
      .getOrElse(sys error s"Can't find $k in ${f.absolutePath}")
      .drop(k.length)
  }

  final case class Options(value: List[String]) {
    def add(k: String, v: String): Options =
      Options(s"-D$k=$v" :: value.filterNot(_ startsWith s"-D$k="))
  }

  final class ServiceRef(startFn: () => Unit, stopFn: () => Unit) {
    private var up = false

    val start: () => Unit = () =>
      if (!up) {
        startFn()
        up = true
      }

    val stop: () => Unit = () => {
      up = false
      stopFn()
    }
  }

  def envRef(env: String)(services: String*) = {
    val ss = services.mkString(" ")
    new ServiceRef(
      () => s"bin/env $env up -d $ss".!!,
      () => s"bin/env $env stop $ss".!!)
  }

  // ===================================================================================================================

  object dev {

    val devEnvStart = taskKey[Unit]("Starts up the dev environment.")
    val devEnvStop = taskKey[Unit]("Stops the dev environment.")

    private val env = envRef("dev")("postgres", "elasticsearch", "logstash")

    val commands: Project => Project =
      _.settings(
        devEnvStart in ThisBuild := env.start(),
        devEnvStop in ThisBuild := env.stop())

    def envRoot(baseDirectory: File): File =
      DockerEnv.dockerEnvsRoot(baseDirectory) / "dev"

    private def runMode =
      if (releaseMode) "production" else "development"

    def javaOptions(serviceName: String, baseDirectory: File): List[String] = {
      val envRoot = this.envRoot(baseDirectory)
      val jaegerPort = envFileValue(envRoot, "PORT_JAEGER_COLLECTOR")
      Options(DockerEnv.javaOptionsFromDockerComposeEnv(serviceName, envRoot))
        .add("db.host", "localhost")
        .add("db.port", envFileValue(envRoot, "PORT_POSTGRES"))
        .add("JAEGER_ENDPOINT", s"http://localhost:$jaegerPort/api/traces")
        .add("run.mode", runMode)
        .value
    }

    def resDir(serviceName: String, baseDirectory: File): File =
      envRoot(baseDirectory) / serviceName
  }

  // ===================================================================================================================

  object test {

    val testEnvStart = taskKey[Unit]("Starts up a test environment.")

    private val env = envRef("test")()

    val required: Project => Project =
      _.settings(
        testEnvStart in ThisBuild := env.start(),
        testOptions in Test += Tests.Setup(env.start))
  }
}
