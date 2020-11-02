import sbt._
import sbt.librarymanagement.ModuleFilter
import scala.languageFeature._
import LibDependency._

object Dependencies {

  object Java {
    val major = 11
  }

  object Graal {
    val ver = "20.2.0"
  }

  object Scala {
    private val mm = scalaItself(version)
    def version  = "2.13.3"
    val library  = mm("scala-library")
    val reflect  = mm("scala-reflect")
    val compiler = mm("scala-compiler") ++ reflect ++ scalaXml
    val p        = mm("scalap") ++ compiler
    val all      = reflect ++ library ++ p
    val macroDef = reflect ++ library ++ (compiler % Provided)
  }

  object React {
    private val mm = MultiModule.js("com.github.japgolly.scalajs-react", "1.7.5")
    val core    = mm("core")
    val test    = mm("test")
    val monocle = mm("ext-monocle-scalaz") ++ Monocle.core
    val extra   = mm("extra")
    val most    = core ++ monocle ++ extra
  }

  object Monocle {
    private val mm = MultiModule.jvmAndJs("com.github.julien-truffaut", "1.6.3")
    val core   = mm("monocle-core")
    val macros = mm("monocle-macro") ++ core
  }

  object Microlibs {
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.microlibs", "2.5")
    val adtMacros  = mm("adt-macros")
    val macroUtils = mm("macro-utils")
    val nonempty   = mm("nonempty")
    val recursion  = mm("recursion")
    val scalazExt  = mm("scalaz-ext")
    val stdlibExt  = mm("stdlib-ext")
    val testUtil   = mm("test-util")
    val utils      = mm("utils")
  }

  object Nyaya {
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.nyaya", "0.9.2")
    val util = mm("nyaya-util") ++ scalaz
    val prop = mm("nyaya-prop") ++ scalaz
    val gen  = mm("nyaya-gen")  ++ scalaz
    val test = mm("nyaya-test")
  }

  object ScalaGraal {
    private val ver   = "1.1.1"
    private val jvm   = MultiModule.scala("com.github.japgolly.scala-graal", ver)
    private val both  = MultiModule.jvmAndJs("com.github.japgolly.scala-graal", ver)
    val core          = jvm("core") ++ graal
    val coreJs        = jvm("core-js") ++ core
    val extBoopickle  = both("ext-boopickle")
    val extPrometheus = jvm("ext-prometheus") ++ coreJs

    lazy val graal = jvmOnly("org.graalvm.sdk" % "graal-sdk" % Graal.ver)
  }

  object TestState {
    val Ver = "2.4.1"
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.test-state", Ver)
    private val js = MultiModule.js("com.github.japgolly.test-state", Ver)
    val core            = mm("core")
    val scalaz          = mm("ext-scalaz") ++ core ++ Dependencies.scalaz
    val nyaya           = mm("ext-nyaya") ++ Dependencies.scalaz ++ Nyaya.gen ++ Nyaya.test
    val scalajsReact    = js("ext-scalajs-react")
    val domZipperSizzle = js("dom-zipper-sizzle")
  }

  object UnivEq {
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.univeq", "1.2.1")
    val univeq = mm("univeq")
    val scalaz = mm("univeq-scalaz") ++ univeq ++ Dependencies.scalaz
  }

  object ScalaCSS {
    private val mm = MultiModule.js("com.github.japgolly.scalacss", "0.6.1")
    val core  = mm("core")
    val react = mm("ext-react") ++ core
  }

  object SLF4J {
    private val mm = MultiModule.java("org.slf4j", "1.7.30")
    val api = mm("slf4j-api")
    val jcl = mm("jcl-over-slf4j")
  }

  object Logback {
    val version = "1.2.3"
    private val mm = MultiModule.java("ch.qos.logback", version)

    val core = mm("logback-classic") ++ mm("logback-core")

    val withPlugins = core ++
      jvmOnly("net.logstash.logback" % "logstash-logback-encoder" % "6.4")
  }

  object Lift {
    private val mm = MultiModule.scala("net.liftweb", "3.4.2")
    val webkit  = mm("lift-webkit") ++ Scala.all // because it contains lift-json
    val testkit = mm("lift-testkit")
  }

  object Doobie {
    private val mm = MultiModule.scala("org.tpolecat", "0.9.2")
    val core          = mm("doobie-core")
    val postgres      = mm("doobie-postgres")
    val postgresCirce = mm("doobie-postgres-circe")
    val hikari        = mm("doobie-hikari")
    val main          = core ++ postgres ++ postgresCirce ++ hikari
  }

  object Circe {
    private val mm = MultiModule.jvmAndJs("io.circe", "0.13.0")
    val core    = mm("circe-core")
    val parser  = mm("circe-parser")
    val testing = mm("circe-testing")
    val main    = core ++ parser
  }

  object JJWT {
    private val mm = MultiModule.java("io.jsonwebtoken", "0.11.2")
    val api     = mm("jjwt-api")
    val impl    = mm("jjwt-impl") % Runtime
    val jackson = mm("jjwt-jackson") % Runtime
    val all     = api ++ impl ++ jackson
  }

  object Akka {
    private val mm = MultiModule.scala("com.typesafe.akka", "2.6.10")
    val actor   = mm("akka-actor") ++ mm("akka-slf4j")
    val testkit = mm("akka-testkit")
  }

  object OkHttp {
    private val mm = MultiModule.java("com.squareup.okhttp3", "3.14.9")
    val core = mm("okhttp")
    // val urlConnection = mm("okhttp-urlconnection") ++ core
  }

  object LibJetty {
    private def ver = "9.4.33.v20201020"
    private val mm = MultiModule.java("org.eclipse.jetty", ver)
    private val ws = MultiModule.java("org.eclipse.jetty.websocket", ver)

    val http                     = mm("jetty-http")
    val servlet                  = mm("jetty-servlet")
    val servlets                 = mm("jetty-servlets")
    val webapp                   = mm("jetty-webapp")
    val runner                   = mm("jetty-runner")
    val distTarGz                = mm("jetty-distribution").modAll(_.artifacts(Artifact("jetty-distribution", "tar.gz", "tar.gz")).intransitive())
    val websocketApi             = ws("websocket-api")
    val websocketServer          = ws("websocket-server")
    val websocketServlet         = ws("websocket-servlet")
    val javaxWebsocketServerImpl = ws("javax-websocket-server-impl")

    // Upgrade in step with Jetty else java.lang.SecurityExceptions will abound
    val javaxServletApi   = jvmOnly("javax.servlet"   % "javax.servlet-api"   % "3.1.0")
    val javaxWebsocketApi = jvmOnly("javax.websocket" % "javax.websocket-api" % "1.0")

    val devRun = runner.modAll(_.intransitive()) ++ javaxWebsocketServerImpl ++ servlets
  }

  object Prometheus {
    private val mm = MultiModule.java("io.prometheus", "0.9.0")
    val client     = mm("simpleclient")
    val hotspot    = mm("simpleclient_hotspot")
    val httpserver = mm("simpleclient_httpserver")
    val logback    = mm("simpleclient_logback")
    val servlet    = mm("simpleclient_servlet")
  }

  val scalajsBenchmark = jsOnly("com.github.japgolly.scalajs-benchmark" %% "benchmark"         % "0.8.0")
  val scalajsDom       = jsOnly("org.scala-js"                          %% "scalajs-dom"       % "1.1.0")
  val scalajsJavaTime  = jsOnly("org.scala-js"                          %% "scalajs-java-time" % "1.0.0")

  val boopickle   = jvmAndJs("io.suzaku",                        "boopickle",   "1.3.3")
  val clearConfig = jvmAndJs("com.github.japgolly.clearconfig",  "core",        "1.4.0")
  val parboiled   = jvmAndJs("org.parboiled",                    "parboiled",   "2.2.1")
  val scalaz      = jvmAndJs("org.scalaz",                       "scalaz-core", "7.2.30")
  val shapeless   = jvmAndJs("com.chuusai",                      "shapeless",   "2.3.3")
  val μTest       = jvmAndJs("com.github.japgolly.fork",         "utest",       "1.0.3")
  val pprint      = jvmAndJs("com.lihaoyi",                      "pprint",      "0.6.0")

  val catsEffect   = jvmOnly("org.typelevel"              %% "cats-effect"           % "2.2.0")
  val commonsIo    = jvmOnly("org.apache.directory.studio" % "org.apache.commons.io" % "2.4")
  val commonsText  = jvmOnly("org.apache.commons"          % "commons-text"          % "1.9")
  val flyway       = jvmOnly("org.flywaydb"                % "flyway-core"           % "6.5.7")
  val hikariCP     = jvmOnly("com.zaxxer"                  % "HikariCP"              % "3.4.5")
  val httpCore     = jvmOnly("org.apache.httpcomponents"   % "httpcore"              % "4.4.13")
  val javaMail     = jvmOnly("com.sun.mail"                % "javax.mail"            % "1.6.2")
  val jaegerClient = jvmOnly("io.jaegertracing"            % "jaeger-client"         % "1.4.0")
  val postgresql   = jvmOnly("org.postgresql"              % "postgresql"            % "42.2.18")
  val redisson     = jvmOnly("org.redisson"                % "redisson"              % "3.13.6")
  val scaffeine    = jvmOnly("com.github.blemale"         %% "scaffeine"             % "4.0.2")
  val scalaCheck   = jvmOnly("org.scalacheck"             %% "scalacheck"            % "1.14.3")
  val scalaLogging = jvmOnly("com.typesafe.scala-logging" %% "scala-logging"         % "3.9.2")
  val scalaXml     = jvmOnly("org.scala-lang.modules"     %% "scala-xml"             % "1.3.0")

  val betterMonadicFor = compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
  val useBetterMonadicFor = (_: Project).settings(addCompilerPlugin(betterMonadicFor))

  val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
  val useKindProjector = (_: Project).settings(addCompilerPlugin(kindProjector))

  val updateExclusions: ModuleFilter = {
    def matchesRegex(r: String) = new PatternFilter(r.r.pattern)
    def containsRegex(r: String) = matchesRegex(s".*(?:$r).*")
    def fn(f: String => Boolean) = new SimpleFilter(f)
    var filters = moduleFilter(NothingFilter)

    // Scalaz: 7.2.x only
    filters |= moduleFilter("org.scalaz", revision = fn(!_.startsWith("7.2.")))
    filters |= moduleFilter("com.github.julien-truffaut", revision = fn(!_.startsWith("1.6.")))

    // OkHTTP: 3.x only
    filters |= moduleFilter("com.squareup.okhttp3", revision = fn(!_.startsWith("3.")))

    // Jetty: stable only
    filters |= moduleFilter("org.eclipse.jetty", revision = containsRegex("alpha|beta"))

    // Jetty javax APIs
    filters |= moduleFilter("javax.servlet", "javax.servlet-api", fn(!_.startsWith("3.")))
    filters |= moduleFilter("javax.websocket", "javax.websocket-api", "1.1")

    filters
  }
}
