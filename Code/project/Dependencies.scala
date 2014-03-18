import sbt._
import sbt.impl.GroupID
import scala.languageFeature._

object Deps {

  case class MS private[Deps](ms: Seq[ModuleID]) {
    def %(revision: String) = MS(ms.map(_ % revision))
    def ++(that: MS) = MS(ms ++ that.ms)
  }
  object MS {
    val empty = MS(Seq.empty)
  }

  private implicit def singleToMS(m: ModuleID) = MS(Seq(m))
  private implicit def seqToMS(ms: Seq[ModuleID]) = MS(ms)


  abstract class Group(final val version: String, final val groupId: GroupID) {
    protected implicit def d(a: String): MS = groupId % a % version
    protected implicit def dd(a: String): MS = groupId %% a % version
  }

  // -------------------------------------------------------------------------------------------------------------------

  object Scala extends Group("2.10.3", "org.scala-lang") {
    val compiler = d("scala-compiler")
    val library  = d("scala-library")
    val reflect  = d("scala-reflect")
    val p        = d("scalap")
    val all      = compiler ++ library ++ reflect ++ p
  }

  object Scalaz extends Group("7.1.0-M6", "org.scalaz") {
    val core       = dd("scalaz-core")
    val concurrent = dd("scalaz-concurrent")
    val effect     = dd("scalaz-effect")
  }

  object Json4s extends Group("3.2.7", "org.json4s") {
    val jackson = dd("json4s-jackson") ++ Scala.all
  }

  object SLF4J extends Group("1.7.6", "org.slf4j") {
    val api = d("slf4j-api")
    val jcl = d("jcl-over-slf4j")
  }

  object Lift extends Group("2.6-M2-golly-1", "net.liftweb") {
    val webkit  = dd("lift-webkit") ++ Scala.all // because it contains lift-json
    val testkit = dd("lift-testkit")
  }

  object Shiro extends Group("1.2.2", "org.apache.shiro") {
    val core = d("shiro-core") ++ SLF4J.jcl // slf4j required in place of commons-logging
    val web  = d("shiro-web")
    val all  = core ++ web
  }

  object Akka extends Group("2.3.0", "com.typesafe.akka") {
    val actor   = dd("akka-actor") ++ dd("akka-slf4j")
    val testkit = dd("akka-testkit")
  }

  val jodaTime    :MS = "joda-time"                   % "joda-time"             % "2.3" ++
                        "org.joda"                    % "joda-convert"          % "1.2"
  val postgresql  :MS = "org.postgresql"              % "postgresql"            % "9.3-1101-jdbc41"
  val slick       :MS = "com.typesafe.slick"         %% "slick"                 % "1.0.1"
  val bonecp      :MS = "com.jolbox"                  % "bonecp"                % "0.8.0.RELEASE" ++
                        "com.google.code.findbugs"    % "jsr305"                % "2.0.2" // required by Guava (which is required by BoneCP)
  val flyway      :MS = "com.googlecode.flyway"       % "flyway-core"           % "2.3.1"
  val logback     :MS = "ch.qos.logback"              % "logback-classic"       % "1.1.1"
  val scalate     :MS = "org.fusesource.scalate"     %% "scalate-core"          % "1.6.1" ++
                        "org.fusesource.scalamd"     %% "scalamd"               % "1.6" // why again?
  val commonsLang :MS = "org.apache.commons"          % "commons-lang3"         % "3.1"
  val commonsIo   :MS = "org.apache.directory.studio" % "org.apache.commons.io" % "2.4"
  val twitterEval :MS = "com.twitter"                %% "util-eval"             % "6.5.0"
  val jetty       :MS = "org.eclipse.jetty"           % "jetty-webapp"          % "9.1.1.v20140108"
  val servlet     :MS = "org.eclipse.jetty.orbit"     % "javax.servlet"         % "3.0.0.v201112011016" artifacts Artifact("javax.servlet", "jar", "jar")
  val mockito     :MS = "org.mockito"                 % "mockito-core"          % "1.9.5"
  val scalaTest   :MS = "org.scalatest"              %% "scalatest"             % "2.1.0"
  val scalaCheck  :MS = "org.scalacheck"             %% "scalacheck"            % "1.11.3"
  val specs2      :MS = "org.specs2"                 %% "specs2"                % "2.3.10-scalaz-7.1.0-M6"
  val selenium    :MS = "org.seleniumhq.selenium"     % "selenium-java"         % "2.35.0" excludeAll(
    ExclusionRule(name = "selenium-android-driver"),
    ExclusionRule(name = "selenium-htmlunit-driver"),
    ExclusionRule(name = "selenium-ie-driver"),
    ExclusionRule(name = "selenium-iphone-driver"),
    ExclusionRule(name = "selenium-safari-driver"))
}