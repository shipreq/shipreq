package shipreq.base.util

import japgolly.microlibs.config._
import java.util.Locale
import scalaz.std.list.listInstance
import scalaz.syntax.applicative._
import scalaz.Scalaz.Id

sealed abstract class RunMode(val id: Int, val name: String, _altNames: String*) {
  override def toString = name
  val names: List[String] = (name :: _altNames.toList).distinct

  def configSources: Sources[Id] = {
    val files = RunMode.filenames(this)(_.mkString("", ".", ".props")) ::: "default.props" :: Nil
    val sources = files.map(Source.propFileOnClasspath[Id](_, optional = true))
    Source.environment[Id] > Sources.highToLowPri(sources: _*) > Source.system[Id]
  }
}

object RunMode {
  case object Development extends RunMode(1, "Development", "dev")
  case object Test        extends RunMode(2, "Test")
  case object Staging     extends RunMode(3, "Staging")
  case object Production  extends RunMode(4, "Production", "prod")
  case object Pilot       extends RunMode(5, "Pilot")
  case object Profile     extends RunMode(6, "Profile")

  val values: List[RunMode] =
    List(Development, Test, Staging, Production, Pilot, Profile)

  private[this] val normaliseName: String => String =
    _ toLowerCase Locale.ENGLISH

  private[this] val nameToMode: Map[String, RunMode] =
      values
        .flatMap(m => m.names.map(n => normaliseName(n) -> m))
        .toMap

  def forName(n: String): Option[RunMode] =
    nameToMode.get(normaliseName(n))

  implicit val configParser: ConfigParser[RunMode] =
    ConfigParser.Implicits.Defaults.parseString.mapOption(forName)

  lazy val Current: RunMode =
    Config.getOrUse("run.mode", detectFromStackTrace())
      .run(Source.environment[Id] > Source.system[Id])
      .getOrDie()

  def detectFromStackTrace(st: Array[StackTraceElement] = Thread.currentThread.getStackTrace): RunMode =
    if (doesStackTraceContainKnownTestRunner(st))
      Test
    else
      Development

  private def doesStackTraceContainKnownTestRunner(st: Array[StackTraceElement]): Boolean = {
    val names = List(
      "org.apache.maven.surefire.booter.SurefireBooter",
      "sbt.TestRunner",
      "org.jetbrains.plugins.scala.testingSupport.scalaTest.ScalaTestRunner",
      "org.scalatest.",
      "org.scalatools.testing.",
      "org.specs2."
    )
    st.exists(e => names.exists(e.getClassName.startsWith))
  }

  def filenames(rm: RunMode)(combineComponents: Seq[String] => String): List[String] = {
    def mkFilename(components: String*): List[String] = {
      val cs = components.filter(c => (c ne null) && c.nonEmpty)
      if (cs.isEmpty) Nil else combineComponents(cs) :: Nil
    }
    val runModeNames = rm.names.map(_.toLowerCase(Locale.ENGLISH))
    val userNames = List(System.getProperty("user.name"), "default", "")
    (runModeNames |@| userNames)((a,b) => mkFilename(a,b)).flatten.distinct
  }
}
