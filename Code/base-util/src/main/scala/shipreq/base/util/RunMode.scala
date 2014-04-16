package shipreq.base.util

import java.util.{Properties, Locale}
import scalaz.\/-
import shipreq.base.util.ExternalValueReader.Retriever

sealed abstract class RunMode(val id: Int, val name: String, _altNames: String*) {
  override def toString = name
  val names: List[String] = (name :: _altNames.toList).distinct
}

object RunMode {
  case object Development extends RunMode(1, "Development", "dev")
  case object Test        extends RunMode(2, "Test")
  case object Staging     extends RunMode(3, "Staging")
  case object Production  extends RunMode(4, "Production", "prod")
  case object Pilot       extends RunMode(5, "Pilot")
  case object Profile     extends RunMode(6, "Profile")

  val values = List(Development, Test, Staging, Production, Pilot, Profile)

  private[this] val normaliseName: String => String =
    _ toLowerCase Locale.ENGLISH

  private[this] val nameToMode: Map[String, RunMode] =
      values.toList
        .flatMap(m => m.names.map(n => (normaliseName(n) -> m)))
        .toMap

  def forName(n: String): Option[RunMode] =
    nameToMode.get(normaliseName(n))

  def retriever(implicit r: Retriever[String]): Retriever[RunMode] =
    new StringBasedValueReader(r).tryParseE[RunMode](s =>
      forName(s) match {
        case Some(m) => \/-(m)
        case None    => ErrorOr.error(s"Unable to parse run mode: $s")
      }
    )

  val retrieverFromSysProps: Retriever[RunMode] =
    retriever(JPropertiesValueReader(Props.systemProps(new Properties)).retrieverS)

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
}
