import sbt._
import scala.languageFeature._
import DependencyLib._

object Dependencies {

  object Scala {
    private val mm = scalaItself(version)
    def version  = "2.11.7"
    val compiler = mm("scala-compiler")
    val library  = mm("scala-library")
    val reflect  = mm("scala-reflect")
    val p        = mm("scalap")
    val all      = compiler ++ library ++ reflect ++ p
    val macroDef = reflect ++ (compiler % "provided")
  }

  object Scalaz {
    private val mm = MultiModule.jvmAndJsFork("org.scalaz", "7.1.3")("com.github.japgolly.fork.scalaz")
    val core       = mm("scalaz-core")
    val effect     = mm("scalaz-effect") ++ core
    val concurrent = mm("scalaz-concurrent") ++ effect
    val iteratee   = mm("scalaz-iteratee") ++ effect
    val scalacheck = mm("scalaz-scalacheck-binding") ++ concurrent ++ iteratee
  }

  object Monocle {
    private val mm = MultiModule.jvmAndJsFork("com.github.julien-truffaut", "1.1.1")("com.github.japgolly.fork.monocle")
    val core   = mm("monocle-core")
    val macros = mm("monocle-macro") ++ core
  }

  object Nyaya {
    private val mm = MultiModule.jvmAndJs("com.github.japgolly.nyaya", "0.5.11")
    val core = mm("nyaya-core") ++ Scalaz.core
    val test = mm("nyaya-test")
  }

  object React {
    private val mm = MultiModule.js("com.github.japgolly.scalajs-react", "0.9.1")
    val core    = mm("core")
    val test    = mm("test")
    val scalaz  = mm("ext-scalaz71") ++ Scalaz.effect
    val monocle = mm("ext-monocle") ++ Monocle.core
    val extra   = mm("extra")
    val most    = core ++ scalaz ++ monocle ++ extra
  }

  object ScalaCSS {
    private val mm = MultiModule.js("com.github.japgolly.scalacss", "0.3.0")
    val core  = mm("core")
    val react = mm("ext-react") ++ core
  }

  object Json4s {
    private val mm = MultiModule.scala("org.json4s", "3.2.10")
    val jackson = mm("json4s-jackson") ++ Scala.all
  }

  object SLF4J {
    private val mm = MultiModule.java("org.slf4j", "1.7.12")
    val api = mm("slf4j-api")
    val jcl = mm("jcl-over-slf4j")
  }

  object Lift {
    private val mm = MultiModule.scala("net.liftweb", "2.6")
    val webkit  = mm("lift-webkit") ++ Scala.all // because it contains lift-json
    val testkit = mm("lift-testkit")
  }

  object Shiro {
    private val mm = MultiModule.java("org.apache.shiro", "1.2.3")
    val core = mm("shiro-core") ++ SLF4J.jcl // slf4j required in place of commons-logging
    val web  = mm("shiro-web")
    val all  = core ++ web
  }

  object Akka {
    private val mm = MultiModule.scala("com.typesafe.akka", "2.3.4")
    val actor   = mm("akka-actor") ++ mm("akka-slf4j")
    val testkit = mm("akka-testkit")
  }

  object Specs2 {
    private val mm = MultiModule.scala("org.specs2", "2.4.17")
    val combo = mm("specs2-core") ++ mm("specs2-scalacheck")
  }

  val parboiled = jvmAndJsFork("org.parboiled", "parboiled", "2.1.0")("com.github.japgolly.fork.parboiled")

  val boopickle = jvmAndJs("me.chrons",                        "boopickle", "1.1.0")
  val shapeless = jvmAndJs("com.chuusai",                      "shapeless", "2.2.5")
  val μPickle   = jvmAndJs("com.github.japgolly.fork.upickle", "upickle",   "custom-4")
  val μTest     = jvmAndJs("com.lihaoyi",                      "utest",     "0.3.1")

  val okHttp      = jvmOnly("com.squareup.okhttp"         % "okhttp"                % "1.5.4")
  val httpCore    = jvmOnly("org.apache.httpcomponents"   % "httpcore"              % "4.3.2")
  val javaMail    = jvmOnly("com.sun.mail"                % "javax.mail"            % "1.5.2")
  val jodaTime    = jvmOnly("joda-time"                   % "joda-time"             % "2.3") ++
                    jvmOnly("org.joda"                    % "joda-convert"          % "1.2")
  val guava       = jvmOnly("com.google.guava"            % "guava"                 % "18.0") ++
                    jvmOnly("com.google.code.findbugs"    % "jsr305"                % "2.0.3") // cos Scala whinges if annotations not found
  val postgresql  = jvmOnly("org.postgresql"              % "postgresql"            % "9.4-1201-jdbc41")
  val slick       = jvmOnly("com.typesafe.slick"         %% "slick"                 % "2.1.0")
  val hikariCP    = jvmOnly("com.zaxxer"                  % "HikariCP"              % "2.4.0")
  val flyway      = jvmOnly("com.googlecode.flyway"       % "flyway-core"           % "2.3.1")
  val logback     = jvmOnly("ch.qos.logback"              % "logback-classic"       % "1.1.3")
  val scalate     = jvmOnly("org.scalatra.scalate"       %% "scalate-core"          % "1.7.1") ++
                    jvmOnly("org.scalatra.scalate"       %% "scalamd"               % "1.6.1") // why again?
  val commonsLang = jvmOnly("org.apache.commons"          % "commons-lang3"         % "3.4")
  val commonsIo   = jvmOnly("org.apache.directory.studio" % "org.apache.commons.io" % "2.4")
  val twitterEval = jvmOnly("com.twitter"                %% "util-eval"             % "6.24.0")
  val jetty       = jvmOnly("org.eclipse.jetty"           % "jetty-webapp"          % "9.2.3.v20140905")
  val servlet     = jvmOnly("org.eclipse.jetty.orbit"     % "javax.servlet"         % "3.0.0.v201112011016" artifacts Artifact("javax.servlet", "jar", "jar"))
  val mockito     = jvmOnly("org.mockito"                 % "mockito-core"          % "1.9.5")
  val scalaTest   = jvmOnly("org.scalatest"              %% "scalatest"             % "2.2.1")
  val scalaCheck  = jvmOnly("org.scalacheck"             %% "scalacheck"            % "1.11.3")
  val selenium    = jvmOnly("org.seleniumhq.selenium"     % "selenium-java"         % "2.35.0" excludeAll(
    ExclusionRule(name = "selenium-android-driver"),
    ExclusionRule(name = "selenium-htmlunit-driver"),
    ExclusionRule(name = "selenium-ie-driver"),
    ExclusionRule(name = "selenium-iphone-driver"),
    ExclusionRule(name = "selenium-safari-driver")))

  val macroParadise = compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
  def useMacroParadise = (_: Project).settings(addCompilerPlugin(macroParadise))
}
