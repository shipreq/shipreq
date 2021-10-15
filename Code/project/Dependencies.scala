import sbt._
import sbt.librarymanagement.ModuleFilter
import scala.languageFeature._
import LibDependency._

object Dependencies {

  object Akka {
    private val mm = MultiModule.scala("com.typesafe.akka", "2.6.16")
    val actor   = mm("akka-actor") ++ mm("akka-slf4j")
    val testkit = mm("akka-testkit")
  }

  object Cats {
    private val mm = MultiModule.jvmAndJs("org.typelevel", "2.6.1")
    val core = mm("cats-core")
    val free = mm("cats-free")
  }

  object CatsEffect {
    private val mm = MultiModule.jvmAndJs("org.typelevel", "2.5.4")
    val core    = mm("cats-effect")
    // val kernal  = mm("cats-effect-kernel")
    val laws    = mm("cats-effect-laws")
    // val std     = mm("cats-effect-std")
    // val testkit = mm("cats-effect-testkit")
  }

  object Circe {
    private val mm = MultiModule.jvmAndJs("io.circe", "0.14.1")
    val core    = mm("circe-core")
    val parser  = mm("circe-parser")
    val testing = mm("circe-testing")
    val main    = core ++ parser
  }

  object Doobie {
    private val mm = MultiModule.scala("org.tpolecat", "0.9.4")
    val core          = mm("doobie-core")
    val postgres      = mm("doobie-postgres")
    val postgresCirce = mm("doobie-postgres-circe")
    val hikari        = mm("doobie-hikari")
    val main          = core ++ postgres ++ postgresCirce ++ hikari
  }

  object Graal {
    // Note: when changing this, make sure to also change:
    //   - :/Docker/dev-build_env/Dockerfile (the aur/jdk11-graalvm-bin git sha)
    //   - :/Docker/shipreq-base/Dockerfile
    val ver = "21.2.0"
  }

  object Java {
    val major = 11
  }

  object Jetty {
    private def ver = "9.4.44.v20210927"
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

  object JJWT {
    private val mm = MultiModule.java("io.jsonwebtoken", "0.11.2")
    val api     = mm("jjwt-api")
    val impl    = mm("jjwt-impl") % Runtime
    val jackson = mm("jjwt-jackson") % Runtime
    val all     = api ++ impl ++ jackson
  }

  object Lift {
    private val mm = MultiModule.scala("net.liftweb", "3.5.0")
    val webkit  = mm("lift-webkit") ++ Scala.all // because it contains lift-json
    val testkit = mm("lift-testkit")
  }

  object Logback {
    val version = "1.2.6"
    private val mm = MultiModule.java("ch.qos.logback", version)

    val core = mm("logback-classic") ++ mm("logback-core")

    val withPlugins = core ++
      jvmOnly("net.logstash.logback" % "logstash-logback-encoder" % "6.6")
  }

  object Microlibs {
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.microlibs", "3.0.1")
    val adtMacros   = mm("adt-macros")
    val catsExt     = mm("cats-ext") ++ Cats.core
    val compileTime = mm("compile-time")
    val disjunction = mm("disjunction")
    val macroUtils  = mm("macro-utils")
    val multimap    = mm("multimap")
    val nonempty    = mm("nonempty")
    val recursion   = mm("recursion")
    val stdlibExt   = mm("stdlib-ext")
    val testUtil    = mm("test-util")
    val types       = mm("types")
    val utils       = mm("utils")
  }

  object Monocle {
    private val mm = MultiModule.jvmAndJs("dev.optics", "3.1.0")
    val core   = mm("monocle-core")
    val macros = mm("monocle-macro") ++ core
  }

  object Nyaya {
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.nyaya", "0.11.0")
    val gen  = mm("nyaya-gen")  ++ Cats.core
    val prop = mm("nyaya-prop") ++ Cats.core
    val test = mm("nyaya-test")
    val util = mm("nyaya-util") ++ Cats.core
  }

  object OkHttp {
    private val mm = MultiModule.java("com.squareup.okhttp3", "3.14.9")
    val core = mm("okhttp")
    // val urlConnection = mm("okhttp-urlconnection") ++ core
  }

  object Prometheus {
    private val mm = MultiModule.java("io.prometheus", "0.12.0")
    val client     = mm("simpleclient")
    val hotspot    = mm("simpleclient_hotspot")
    val httpserver = mm("simpleclient_httpserver")
    val logback    = mm("simpleclient_logback")
    val servlet    = mm("simpleclient_servlet")
  }

  object React {
    private val mm = MultiModule.js("com.github.japgolly.scalajs-react", "2.0.0-RC3")
    val cats    = mm("core-ext-cats")
    val core    = mm("core")
    val extra   = mm("extra")
    val monocle = mm("extra-ext-monocle3") ++ Monocle.core
    val test    = mm("test")
    val most    = core ++ monocle ++ extra
  }

  object Scala {
    private val mm = scalaItself(version)
    def version  = "2.13.6"
    val library  = mm("scala-library")
    val reflect  = mm("scala-reflect")
    val compiler = mm("scala-compiler") ++ reflect ++ scalaXml
    val p        = mm("scalap") ++ compiler
    val all      = reflect ++ library ++ p
    val macroDef = reflect ++ library ++ (compiler % Provided)
  }

  object ScalaCSS {
    private val ver      = "0.8.0-RC1"
    private val jvmAndJs = MultiModule.jvmAndJs("com.github.japgolly.scalacss", ver)
    private val js       = MultiModule.js("com.github.japgolly.scalacss", ver)
    val core             = jvmAndJs("core")
    val react            = js("ext-react") ++ core
  }

  object ScalaGraal {
    private val ver   = "1.2.0"
    private val jvm   = MultiModule.scala("com.github.japgolly.scala-graal", ver)
    private val both  = MultiModule.jvmAndJs("com.github.japgolly.scala-graal", ver)
    val core          = jvm("core") ++ graal
    val coreJs        = jvm("core-js") ++ core
    val extBoopickle  = both("ext-boopickle")
    val extPrometheus = jvm("ext-prometheus") ++ coreJs

    lazy val graal = jvmOnly("org.graalvm.sdk" % "graal-sdk" % Graal.ver)
  }

  object SLF4J {
    private val mm = MultiModule.java("org.slf4j", "1.7.32")
    val api = mm("slf4j-api")
    val jcl = mm("jcl-over-slf4j")
  }

  object TestState {
    val Ver = "2.5.0-RC1"
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.test-state", Ver)
    private val js = MultiModule.js("com.github.japgolly.test-state", Ver)
    val core            = mm("core")
    val cats            = mm("ext-cats") ++ core ++ Cats.core
    val domZipperSizzle = js("dom-zipper-sizzle")
    val nyaya           = mm("ext-nyaya") ++ Cats.core ++ Nyaya.gen ++ Nyaya.test
    val scalajsReact    = js("ext-scalajs-react")
  }

  object UnivEq {
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.univeq", "1.6.0")
    val univeq = mm("univeq")
    val cats   = mm("univeq-cats") ++ univeq ++ Cats.core
  }

  // ===================================================================================================================

  val boopickle   = jvmAndJs("io.suzaku",                        "boopickle",   "1.4.0")
  val clearConfig = jvmAndJs("com.github.japgolly.clearconfig",  "core",        "2.0.0")
  val parboiled   = jvmAndJs("org.parboiled",                    "parboiled",   "2.3.0")
  val pprint      = jvmAndJs("com.lihaoyi",                      "pprint",      "0.6.6")
  val shapeless   = jvmAndJs("com.chuusai",                      "shapeless",   "2.3.7")
  val utest       = jvmAndJs("com.github.japgolly.fork",         "utest",       "1.0.3")

  val commonsIo    = jvmOnly("org.apache.directory.studio" % "org.apache.commons.io" % "2.4")
  val commonsText  = jvmOnly("org.apache.commons"          % "commons-text"          % "1.9")
  val flyway       = jvmOnly("org.flywaydb"                % "flyway-core"           % "8.0.1")
  val hikariCP     = jvmOnly("com.zaxxer"                  % "HikariCP"              % "3.4.5") // don't go to v4 yet, wait for Doobie
  val httpCore     = jvmOnly("org.apache.httpcomponents"   % "httpcore"              % "4.4.14")
  val jaegerClient = jvmOnly("io.jaegertracing"            % "jaeger-client"         % "1.6.0")
  val javaMail     = jvmOnly("com.sun.mail"                % "javax.mail"            % "1.6.2")
  val postgresql   = jvmOnly("org.postgresql"              % "postgresql"            % "42.2.20")
  val redisson     = jvmOnly("org.redisson"                % "redisson"              % "3.16.3")
  val scaffeine    = jvmOnly("com.github.blemale"         %% "scaffeine"             % "4.0.2")
  val scalaCheck   = jvmOnly("org.scalacheck"             %% "scalacheck"            % "1.15.4")
  val scalaLogging = jvmOnly("com.typesafe.scala-logging" %% "scala-logging"         % "3.9.4")
  val scalaXml     = jvmOnly("org.scala-lang.modules"     %% "scala-xml"             % "1.3.0")
  val scalaz       = jvmOnly("org.scalaz"                 %% "scalaz-core"           % "7.3.5") // only used for scalaz.Heap

  val scalajsBenchmark = jsOnly("com.github.japgolly.scalajs-benchmark" %% "benchmark"         % "0.10.0-RC2")
  val scalajsDom       = jsOnly("org.scala-js"                          %% "scalajs-dom"       % "1.1.0")
  val scalajsJavaTime  = jsOnly("org.scala-js"                          %% "scalajs-java-time" % "1.0.0")

  // ===================================================================================================================

  val betterMonadicFor = compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
  val useBetterMonadicFor = (_: Project).settings(addCompilerPlugin(betterMonadicFor))

  val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)
  val useKindProjector = (_: Project).settings(addCompilerPlugin(kindProjector))

  // ===================================================================================================================

  def globalDependencyOverrides = (
    UnivEq.cats ++
    UnivEq.univeq
  ).allModuleIds

  val updateExclusions: ModuleFilter = {
    def matchesRegex(r: String) = new PatternFilter(r.r.pattern)
    def containsRegex(r: String) = matchesRegex(s".*(?:$r).*")
    def fn(f: String => Boolean) = new SimpleFilter(f)
    var filters = moduleFilter(NothingFilter)

    // Scala: I already know. Shoosh please.
    filters |= moduleFilter("org.scala-lang", "scala-compiler")
    filters |= moduleFilter("org.scala-lang", "scala-library")
    filters |= moduleFilter("org.scala-lang", "scala-reflect")
    filters |= moduleFilter("org.scala-lang", "scalap")

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
