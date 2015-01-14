import sbt._
import sbt.impl.GroupID
import scala.languageFeature._
import scala.scalajs.sbtplugin.ScalaJSPlugin._

object Deps {

  case class MS private[Deps](ms: Seq[ModuleID]) {
    def %(revision: String): MS = MS(ms.map(_ % revision))
    def ++(that: MS): MS = MS(ms ++ that.ms)
  }
  object MS {
    val empty = MS(Seq.empty)
  }

  private implicit def singleToMS(m: ModuleID) = MS(Seq(m))
  private implicit def seqToMS(ms: Seq[ModuleID]) = MS(ms)

  private def jsA(a: String) = a + "_sjs0.5"
  private def jsGA(g: String, a: String) = g %% jsA(a)

  abstract class Group(final val version: String, final val groupId: String) {
    protected implicit def d(a: String): MS = (groupId: GroupID) % a % version
    protected implicit def dd(a: String): MS = (groupId: GroupID) %% a % version
    protected implicit def js(a: String) = dd(jsA(a))
  }

  def JvmAndJs(groupId: String, name: String, version: String) =
    JvmAndJsFork(groupId, groupId, name, version)

  case class JvmAndJsFork(jvmGroupId: String, jsGroupId: String, name: String, version: String) {
    final val jvm: MS = (jvmGroupId: GroupID) %% name % version
    final val js: MS = jsGA(jsGroupId, name) % version
  }

  // -------------------------------------------------------------------------------------------------------------------

  object ScalaJS {
    // Update webapp-server/bower.json too.
    val reactJs = "org.webjars" % "react" % "0.12.1"

    val sizzleJs = "org.webjars" % "sizzle" % "2.1.1"

    object React extends Group("0.7.1", "com.github.japgolly.scalajs-react") {
      val core    = js("core")
      val test    = js("test")
      val scalaz  = js("ext-scalaz71")
      val monocle = js("ext-monocle")
      val extra   = js("extra")
      val most    = core ++ scalaz ++ monocle ++ extra
    }
    object Scalaz extends Group(Deps.Scalaz.version + "-4", "com.github.japgolly.fork.scalaz") {
      val core   = js("scalaz-core")
      val effect = js("scalaz-effect")
    }
    object Monocle extends Group(Deps.Monocle.version, "com.github.japgolly.fork.monocle") {
      val core   = js("monocle-core")
      val macros = js("monocle-macro") ++ core
    }
  }

  object Scala extends Group("2.11.5", "org.scala-lang") {
    val compiler = d("scala-compiler")
    val library  = d("scala-library")
    val reflect  = d("scala-reflect")
    val p        = d("scalap")
    val all      = compiler ++ library ++ reflect ++ p
  }

  object Scalaz extends Group("7.1.0", "org.scalaz") {
    val core       = dd("scalaz-core")
    val concurrent = dd("scalaz-concurrent")
    val effect     = dd("scalaz-effect")
    val scalacheck = dd("scalaz-scalacheck-binding")
  }

  object Monocle extends Group("1.0.1", "com.github.julien-truffaut") {
    val core   = dd("monocle-core")
    val macros = dd("monocle-macro") ++ core
  }

  object Nyaya extends Group("0.5.0", "com.github.japgolly.nyaya") {
    object jvm {
      val core = dd("nyaya-core")
      val test = dd("nyaya-test")
    }
    object js {
      val core = js("nyaya-core")
      val test = js("nyaya-test")
    }
  }

  object Json4s extends Group("3.2.10", "org.json4s") {
    val jackson = dd("json4s-jackson") ++ Scala.all
  }

  object SLF4J extends Group("1.7.7", "org.slf4j") {
    val api = d("slf4j-api")
    val jcl = d("jcl-over-slf4j")
  }

  object Lift extends Group("2.6-RC1", "net.liftweb") {
    val webkit  = dd("lift-webkit") ++ Scala.all // because it contains lift-json
    val testkit = dd("lift-testkit")
  }

  object Shiro extends Group("1.2.3", "org.apache.shiro") {
    val core = d("shiro-core") ++ SLF4J.jcl // slf4j required in place of commons-logging
    val web  = d("shiro-web")
    val all  = core ++ web
  }

  object Akka extends Group("2.3.4", "com.typesafe.akka") {
    val actor   = dd("akka-actor") ++ dd("akka-slf4j")
    val testkit = dd("akka-testkit")
  }

  object Specs2 extends Group("2.4.2", "org.specs2") {
    val combo = dd("specs2-core") ++ dd("specs2-scalacheck")
  }

  val shapeless = JvmAndJs("com.github.japgolly.fork.shapeless", "shapeless", "2.0.0")
  val μPickle   = JvmAndJs("com.github.japgolly.fork.upickle",   "upickle",   "custom-1")
  val μTest     = JvmAndJs("com.lihaoyi",                        "utest",     "0.2.3")

  // Was only needed trying to use Monocle's @Lenses. Monocle's Lenser works without this.
  // val macroParadise = compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
  // def useMacroParadiseJvm = (_: Project).settings(addCompilerPlugin(macroParadise))

  val okHttp      :MS = "com.squareup.okhttp"         % "okhttp"                % "1.5.4"
  val httpCore    :MS = "org.apache.httpcomponents"   % "httpcore"              % "4.3.2"
  val javaMail    :MS = "com.sun.mail"                % "javax.mail"            % "1.5.2"
  val jodaTime    :MS = "joda-time"                   % "joda-time"             % "2.3" ++
                        "org.joda"                    % "joda-convert"          % "1.2"
  val postgresql  :MS = "org.postgresql"              % "postgresql"            % "9.3-1102-jdbc41"
  val slick       :MS = "com.typesafe.slick"         %% "slick"                 % "2.1.0"
  val bonecp      :MS = "com.jolbox"                  % "bonecp"                % "0.8.0.RELEASE" ++
                        "com.google.code.findbugs"    % "jsr305"                % "2.0.2" // required by Guava (which is required by BoneCP)
  val flyway      :MS = "com.googlecode.flyway"       % "flyway-core"           % "2.3.1"
  val logback     :MS = "ch.qos.logback"              % "logback-classic"       % "1.1.2"
  val scalate     :MS = "org.scalatra.scalate"       %% "scalate-core"          % "1.7.0" ++
                        "org.scalatra.scalate"       %% "scalamd"               % "1.6.1" // why again?
  val commonsLang :MS = "org.apache.commons"          % "commons-lang3"         % "3.3.2"
  val commonsIo   :MS = "org.apache.directory.studio" % "org.apache.commons.io" % "2.4"
  val twitterEval :MS = "com.twitter"                %% "util-eval"             % "6.14.0"
  val jetty       :MS = "org.eclipse.jetty"           % "jetty-webapp"          % "9.2.3.v20140905"
  val servlet     :MS = "org.eclipse.jetty.orbit"     % "javax.servlet"         % "3.0.0.v201112011016" artifacts Artifact("javax.servlet", "jar", "jar")
  val mockito     :MS = "org.mockito"                 % "mockito-core"          % "1.9.5"
  val scalaTest   :MS = "org.scalatest"              %% "scalatest"             % "2.2.1"
  val scalaCheck  :MS = "org.scalacheck"             %% "scalacheck"            % "1.11.3"
  val selenium    :MS = "org.seleniumhq.selenium"     % "selenium-java"         % "2.35.0" excludeAll(
    ExclusionRule(name = "selenium-android-driver"),
    ExclusionRule(name = "selenium-htmlunit-driver"),
    ExclusionRule(name = "selenium-ie-driver"),
    ExclusionRule(name = "selenium-iphone-driver"),
    ExclusionRule(name = "selenium-safari-driver"))
}
