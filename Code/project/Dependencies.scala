import sbt._
import scala.languageFeature._
import LibDependency._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{CrossType => _, crossProject => _, _}

object Dependencies {

  object Java {
    val major = 8
    val minor = 152
    val build = "16"
  }

  object Docker {
    val baseImage = s"anapsix/alpine-java:${Java.major}u${Java.minor}b${Java.build}_server-jre_unlimited"
  }

  object Scala {
    private val mm = scalaItself(version)
    def version  = "2.12.8"
    val library  = mm("scala-library")
    val reflect  = mm("scala-reflect")
    val compiler = mm("scala-compiler") ++ reflect ++ scalaXml
    val p        = mm("scalap") ++ compiler
    val all      = reflect ++ library ++ p
    val macroDef = reflect ++ library ++ (compiler % Provided)
  }

  object Scalaz {
    private val mm = MultiModule.jvmAndJs("org.scalaz", "7.2.27")
    val core       = mm("scalaz-core")
    val effect     = mm("scalaz-effect") ++ core
    val concurrent = mm("scalaz-concurrent") ++ effect
  }

  object Monocle {
    private val mm = MultiModule.jvmAndJs("com.github.julien-truffaut", "1.5.0")
    val core   = mm("monocle-core")
    val macros = mm("monocle-macro") ++ core
  }

  object Microlibs {
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.microlibs", "1.18")
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
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.nyaya", "0.8.1")
    val util = mm("nyaya-util") ++ Scalaz.core
    val prop = mm("nyaya-prop") ++ Scalaz.core
    val gen  = mm("nyaya-gen")  ++ Scalaz.core
    val test = mm("nyaya-test")
  }

  object TestState {
    val Ver = "2.3.0"
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.test-state", Ver)
    private val js = MultiModule.js("com.github.japgolly.test-state", Ver)
    val core            = mm("core")
    val scalaz          = mm("ext-scalaz") ++ core ++ Scalaz.core
    val nyaya           = mm("ext-nyaya") ++ scalaz ++ Nyaya.gen ++ Nyaya.test
    val scalajsReact    = js("ext-scalajs-react")
    val domZipperSizzle = js("dom-zipper-sizzle")
  }

  object UnivEq {
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.univeq", "1.0.5")
    val univeq = mm("univeq")
    val scalaz = mm("univeq-scalaz") ++ univeq ++ Scalaz.core
  }

  object React {
    private val mm = MultiModule.js("com.github.japgolly.scalajs-react", "1.4.0")
    val core    = mm("core")
    val test    = mm("test")
    val scalaz  = mm("ext-scalaz72") ++ Scalaz.effect
    val monocle = mm("ext-monocle") ++ Monocle.core
    val extra   = mm("extra")
    val most    = core ++ scalaz ++ monocle ++ extra
  }

  object ScalaCSS {
    private val mm = MultiModule.js("com.github.japgolly.scalacss", "0.5.5")
    val core  = mm("core")
    val react = mm("ext-react") ++ core
  }

  object Json4s {
    private val mm = MultiModule.scala("org.json4s", "3.6.4")
    val jackson = mm("json4s-jackson") ++ Scala.all
  }

  object SLF4J {
    private val mm = MultiModule.java("org.slf4j", "1.7.25")
    val api = mm("slf4j-api")
    val jcl = mm("jcl-over-slf4j")
  }

  object Logback {
    val version = "1.2.3"
    private val mm = MultiModule.java("ch.qos.logback", version)

    val core = mm("logback-classic") ++ mm("logback-core")

    val withPlugins = core ++
      jvmOnly("net.logstash.logback" % "logstash-logback-encoder" % "5.3")
  }

  object Lift {
    private val mm = MultiModule.scala("net.liftweb", "3.1.1")
    val webkit  = mm("lift-webkit") ++ Scala.all // because it contains lift-json
    val testkit = mm("lift-testkit")
  }

  object Doobie {
    private val mm = MultiModule.scala("org.tpolecat", "0.4.1")
    val core     = mm("doobie-core")
    val postgres = mm("doobie-postgres")
    val hikari   = mm("doobie-hikari")
    val main     = core ++ postgres ++ hikari
  }

  object Shiro {
    private val mm = MultiModule.java("org.apache.shiro", "1.3.2")
    val core = mm("shiro-core") ++ SLF4J.jcl // Use SLF4J in place of commons-logging
    val web  = mm("shiro-web")
    val all  = core ++ web
  }

  object Akka {
    val shortVer = "2.5"
    private val mm = MultiModule.scala("com.typesafe.akka", shortVer + ".20")
    val actor   = mm("akka-actor") ++ mm("akka-slf4j")
    val testkit = mm("akka-testkit")
  }

  object OkHttp {
    private val mm = MultiModule.java("com.squareup.okhttp3", "3.11.0")
    val core = mm("okhttp")
    // val urlConnection = mm("okhttp-urlconnection") ++ core
  }

  object LibJetty {
    private val mm = MultiModule.java("org.eclipse.jetty", "9.4.14.v20181114")
    val webapp = mm("jetty-webapp")
    val runner = mm("jetty-runner")
    val dist   = mm("jetty-distribution").modAll(_.artifacts(Artifact("jetty-distribution", "tar.gz", "tar.gz")).intransitive())

    // Upgrade this in step with Jetty or else java.lang.SecurityExceptions will abound.
    // It's a transitive dependency of jetty-server
    val servletApi = jvmOnly("javax.servlet" % "javax.servlet-api" % "3.1.0")
  }

  object Prometheus {
    private val mm = MultiModule.java("io.prometheus", "0.6.0")
    val client     = mm("simpleclient")
    val hotspot    = mm("simpleclient_hotspot")
    val httpserver = mm("simpleclient_httpserver")
    val servlet    = mm("simpleclient_servlet")
  }

  val scalajsBenchmark = jsOnly("com.github.japgolly.scalajs-benchmark" %% "benchmark"         % "0.2.6")
  val scalajsDom       = jsOnly("org.scala-js"                          %% "scalajs-dom"       % "0.9.6")
  val scalajsJavaTime  = jsOnly("org.scala-js"                          %% "scalajs-java-time" % "0.2.5")

  val boopickle   = jvmAndJs("io.suzaku",                        "boopickle", "1.3.0")
  val clearConfig = jvmAndJs("com.github.japgolly.clearconfig",  "core",      "1.2.2")
  val parboiled   = jvmAndJs("org.parboiled",                    "parboiled", "2.1.5")
  val shapeless   = jvmAndJs("com.chuusai",                      "shapeless", "2.3.3")
  val μPickle     = jvmAndJs("com.github.japgolly.fork.upickle", "upickle",   "custom-7")
  val μTest       = jvmAndJs("com.lihaoyi",                      "utest",     "0.6.6")

  val commonsIo    = jvmOnly("org.apache.directory.studio" % "org.apache.commons.io" % "2.4")
  val commonsLang  = jvmOnly("org.apache.commons"          % "commons-lang3"         % "3.8.1")
  val flyway       = jvmOnly("com.googlecode.flyway"       % "flyway-core"           % "2.3.1")
  val hikariCP     = jvmOnly("com.zaxxer"                  % "HikariCP"              % "3.3.0")
  val httpCore     = jvmOnly("org.apache.httpcomponents"   % "httpcore"              % "4.4.11")
  val javaMail     = jvmOnly("com.sun.mail"                % "javax.mail"            % "1.6.2")
  val kamonJaeger  = jvmOnly("io.kamon"                   %% "kamon-jaeger"          % "1.0.2")
  val postgresql   = jvmOnly("org.postgresql"              % "postgresql"            % "42.2.5")
  val scalaCheck   = jvmOnly("org.scalacheck"             %% "scalacheck"            % "1.13.5")
  val scalaLogging = jvmOnly("com.typesafe.scala-logging" %% "scala-logging"         % "3.9.2")
  val scalaXml     = jvmOnly("org.scala-lang.modules"     %% "scala-xml"             % "1.1.1")
  val twitterEval  = jvmOnly("com.twitter"                %% "util-eval"             % "6.43.0")

  val betterMonadicFor = compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")
  val useBetterMonadicFor = (_: Project).settings(addCompilerPlugin(betterMonadicFor))

  val macroParadise = compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
  val useMacroParadise = (_: Project).settings(addCompilerPlugin(macroParadise))

  val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")
  val useKindProjector = (_: Project).settings(addCompilerPlugin(kindProjector))

//  import sbt.Keys._
//  def useLocalJar(filename: String) =
//    (_: Project).settings(unmanagedJars in Compile += file("lib").getAbsoluteFile / filename)
}
