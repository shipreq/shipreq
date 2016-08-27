import sbt._
import scala.languageFeature._
import DependencyLib._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.toScalaJSGroupID

object Dependencies {

  object Scala {
    private val mm = scalaItself(version)
    def version  = "2.11.8"
    val compiler = mm("scala-compiler")
    val library  = mm("scala-library")
    val reflect  = mm("scala-reflect")
    val p        = mm("scalap")
    val all      = compiler ++ library ++ reflect ++ p
    val macroDef = reflect ++ library ++ (compiler % "provided")

    val java8compat = "org.scala-lang.modules" %% "scala-java8-compat" % "0.7.0"
  }

  object Scalaz {
    private val mm = MultiModule.jvmAndJs("org.scalaz", "7.2.5")
    val core       = mm("scalaz-core")
    val effect     = mm("scalaz-effect") ++ core
    val concurrent = mm("scalaz-concurrent") ++ effect
    val iteratee   = mm("scalaz-iteratee") ++ effect
    val scalacheck = mm("scalaz-scalacheck-binding") ++ concurrent ++ iteratee
  }

  object Monocle {
    private val mm = MultiModule.jvmAndJs("com.github.julien-truffaut", "1.2.2")
    val core   = mm("monocle-core")
    val macros = mm("monocle-macro") ++ core
  }

  object Nyaya {
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.nyaya", "0.7.0")
    val util = mm("nyaya-util") ++ Scalaz.core
    val prop = mm("nyaya-prop") ++ Scalaz.core
    val gen  = mm("nyaya-gen")  ++ Scalaz.core
    val test = mm("nyaya-test")
  }

  object TestState {
    val Ver = "2.0.0"
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.test-state", Ver)
    private val js = MultiModule.js("com.github.japgolly.test-state", Ver)
    val core            = mm("core")
    val scalaz          = mm("ext-scalaz") ++ core ++ Scalaz.core
    val nyaya           = mm("ext-nyaya") ++ scalaz ++ Nyaya.gen ++ Nyaya.test
    val scalajsReact    = js("ext-scalajs-react")
    val domZipperSizzle = js("dom-zipper-sizzle")
  }

  object UnivEq {
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.univeq", "1.0.1")
    val univeq = mm("univeq")
    val scalaz = mm("univeq-scalaz") ++ univeq ++ Scalaz.core
  }

  object React {
    private val mm = MultiModule.js("com.github.japgolly.scalajs-react", "0.11.1")
    val core    = mm("core")
    val test    = mm("test")
    val scalaz  = mm("ext-scalaz72") ++ Scalaz.effect
    val monocle = mm("ext-monocle") ++ Monocle.core
    val extra   = mm("extra")
    val most    = core ++ scalaz ++ monocle ++ extra
  }

  object ScalaCSS {
    private val mm = MultiModule.js("com.github.japgolly.scalacss", "0.4.1")
    val core  = mm("core")
    val react = mm("ext-react") ++ core
  }

  object Json4s {
    private val mm = MultiModule.scala("org.json4s", "3.3.0")
    val jackson = mm("json4s-jackson") ++ Scala.all
  }

  object SLF4J {
    private val mm = MultiModule.java("org.slf4j", "1.7.21")
    val api = mm("slf4j-api")
    val jcl = mm("jcl-over-slf4j")
  }

  object Lift {
    private val mm = MultiModule.scala("net.liftweb", "2.6")
    val webkit  = mm("lift-webkit") ++ Scala.all // because it contains lift-json
    val testkit = mm("lift-testkit")
  }

  object Shiro {
    private val mm = MultiModule.java("org.apache.shiro", "1.2.6")
    val core = mm("shiro-core") ++ SLF4J.jcl // slf4j required in place of commons-logging
    val web  = mm("shiro-web")
    val all  = core ++ web
  }

  object Akka {
    private val mm = MultiModule.scala("com.typesafe.akka", "2.3.12")
    val actor   = mm("akka-actor") ++ mm("akka-slf4j")
    val testkit = mm("akka-testkit")
  }

  object Specs2 {
    private val mm = MultiModule.scala("com.github.japgolly.fork.specs2", "2.4.17-scalaz72")
    val combo = mm("specs2-core") ++ mm("specs2-scalacheck")
  }

  object LibJetty {
    private val mm = MultiModule.java("org.eclipse.jetty", "9.3.2.v20150730")
    val webapp = mm("jetty-webapp")
    val runner = mm("jetty-runner")

    // Upgrade this in step with Jetty or else java.lang.SecurityExceptions will abound.
    // It's a transitive dependency of jetty-server
    val servletApi = jvmOnly("javax.servlet" % "javax.servlet-api" % "3.1.0")
  }

  val scalajsDom       = jsOnly("org.scala-js"                          %%%! "scalajs-dom"       % "0.9.1")
//val scalajsJavaTime  = jsOnly("org.scala-js"                          %%%! "scalajs-java-time" % "0.1.0")
  val scalajsBenchmark = jsOnly("com.github.japgolly.scalajs-benchmark" %%%! "benchmark"         % "0.2.3")

  val boopickle = jvmAndJs("me.chrons",                        "boopickle", "1.1.0")
  val parboiled = jvmAndJs("org.parboiled",                    "parboiled", "2.1.3")
  val shapeless = jvmAndJs("com.chuusai",                      "shapeless", "2.3.2")
  val μPickle   = jvmAndJs("com.github.japgolly.fork.upickle", "upickle",   "custom-5")
  val μTest     = jvmAndJs("com.lihaoyi",                      "utest",     "0.3.1")

  val okHttp      = jvmOnly("com.squareup.okhttp"         % "okhttp"                % "1.5.4")
  val httpCore    = jvmOnly("org.apache.httpcomponents"   % "httpcore"              % "4.3.3")
  val javaMail    = jvmOnly("com.sun.mail"                % "javax.mail"            % "1.5.4")
  val jodaTime    = jvmOnly("joda-time"                   % "joda-time"             % "2.3") ++
                    jvmOnly("org.joda"                    % "joda-convert"          % "1.2")
  val postgresql  = jvmOnly("org.postgresql"              % "postgresql"            % "9.4.1209")
  val slick       = jvmOnly("com.typesafe.slick"         %% "slick"                 % "2.1.0")
  val hikariCP    = jvmOnly("com.zaxxer"                  % "HikariCP"              % "2.4.7")
  val flyway      = jvmOnly("com.googlecode.flyway"       % "flyway-core"           % "2.3.1")
  val logback     = jvmOnly("ch.qos.logback"              % "logback-classic"       % "1.1.7")
  val scalate     = jvmOnly("org.scalatra.scalate"       %% "scalate-core"          % "1.7.1") ++
                    jvmOnly("org.scalatra.scalate"       %% "scalamd"               % "1.6.1") // why again?
  val commonsLang = jvmOnly("org.apache.commons"          % "commons-lang3"         % "3.4")
  val commonsIo   = jvmOnly("org.apache.directory.studio" % "org.apache.commons.io" % "2.4")
  val twitterEval = jvmOnly("com.twitter"                %% "util-eval"             % "6.36.0")
  val mockito     = jvmOnly("org.mockito"                 % "mockito-core"          % "1.10.19")
  val scalaTest   = jvmOnly("org.scalatest"              %% "scalatest"             % "2.2.5")
  val scalaCheck  = jvmOnly("org.scalacheck"             %% "scalacheck"            % "1.11.3")

  val macroParadise = compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  val useMacroParadise = (_: Project).settings(addCompilerPlugin(macroParadise))

  import sbt.Keys._
  def useLocalJar(filename: String) =
    (_: Project).settings(unmanagedJars in Compile += file("lib").getAbsoluteFile / filename)

  val useJavaTimeJS = useLocalJar("scalajs-java-time_sjs0.6_2.11-0.1.1-SNAPSHOT.jar")
}
