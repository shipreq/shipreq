import sbt._, Keys._
import Common.releaseMode

/** Unlike [[Docker]], this is about spinning up a local environment using Docker.
  *
  * For example, spinning up a local Postgres and Redis for tests to connect to.
  */
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
    baseDirectory / "../envs"

  def envFileValue(envRoot: File, key: String): String = {
    val k = key + "="
    val f = envRoot / ".env"
    IO.readLines(f)
      .find(_.startsWith(k))
      .getOrElse(sys error s"Can't find $k in ${f.absolutePath}")
      .drop(k.length)
  }

  final case class JavaOptions(asList: List[String]) extends AnyVal {
    def add(k: String, v: String): JavaOptions =
      JavaOptions(s"-D$k=$v" :: asList.filterNot(_ startsWith s"-D$k="))
  }

  object JavaOptions {
    def fromDockerComposeEnv(serviceName: String, envRoot: File, filename: String = "docker-compose.yml") =
      apply(DockerEnv.javaOptionsFromDockerComposeEnv(serviceName, envRoot, filename))
  }

  final class Services(startFn: () => Unit, stopFn: () => Unit) {
    private var up = false

    val start: () => Unit =
      () =>
        if (!up) {
          startFn()
          up = true
        }

    val stop: () => Unit =
      () => {
        up = false
        stopFn()
      }
  }

  object Services {
    def fromDockerCompose(env: String, only: Iterable[String] = Nil): Services = {
      val names = only.mkString(" ")
      new Services(
        () => s"bin/env $env up -d $names".!,
        () => s"bin/env $env stop $names".!)
    }
  }

  // ===================================================================================================================

  object dev {

    val devEnvStart = taskKey[Unit]("Starts up the dev environment.")
    val devEnvStop = taskKey[Unit]("Stops the dev environment.")

    private val services = Services.fromDockerCompose(env = "dev", only = Seq("postgres", "redis"))

    // The port on which to serve the webapp when it's run from sbt
    val port = 8080

    val commands: Project => Project =
      _.settings(
        ThisBuild / devEnvStart := services.start(),
        ThisBuild / devEnvStop := services.stop())

    def envRoot(baseDirectory: File): File =
      DockerEnv.dockerEnvsRoot(baseDirectory) / "dev"

    private def runMode =
      if (releaseMode) "production" else "development"

    def javaOptions(serviceName: String, baseDirectory: File): List[String] = {
      val envRoot      = this.envRoot(baseDirectory)
      val postgresPort = envFileValue(envRoot, "PORT_POSTGRES")
      val jaegerPort   = envFileValue(envRoot, "PORT_JAEGER_COLLECTOR")
      val redisPort    = envFileValue(envRoot, "PORT_REDIS")
      JavaOptions.fromDockerComposeEnv(serviceName, envRoot)
        .add("JAEGER_ENDPOINT"    , s"http://localhost:$jaegerPort/api/traces")
        .add("LOG_APPENDER"       , "STDOUT")
        .add("db.host"            , "localhost")
        .add("db.port"            , postgresPort)
        .add("redis.url"          , s"redis://localhost:$redisPort")
        .add("run.mode"           , runMode)
        .add("shipreq.ssr.enabled", "false")
        .add("shipreq.url"        , s"http://localhost:$port")
        .asList
    }

    def resDir(serviceName: String, baseDirectory: File): File =
      envRoot(baseDirectory) / serviceName
  }

  // ===================================================================================================================

  object test extends (Project => Project) {

    val testEnvStart = taskKey[Unit]("Starts up a test environment.")

    private val services = Services.fromDockerCompose(env = "test")

    override def apply(p: Project): Project =
      p.settings(
        ThisBuild / testEnvStart := services.start(),
        Test / testOptions += Tests.Setup(services.start))
  }
}
