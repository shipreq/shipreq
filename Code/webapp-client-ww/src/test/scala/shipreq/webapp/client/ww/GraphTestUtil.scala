package shipreq.webapp.client.ww

import japgolly.microlibs.nonempty.NonEmptySet
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.Text
import shipreq.webapp.client.ww.GraphViz.DOT
import sourcecode.Line

object GraphTestUtil {

  private val _normalise = raw"""([\]{};])""".r

  private def normaliseDOT(d: DOT): String =
    _normalise.replaceAllIn(d.content, "$1\n")
      .replace("]\n[", " ")
      .replace("][", " ")

  def assertDOT(actual: DOT, expect: DOT)(implicit l: Line): Unit = {
    val expect2 = expect.content
      .trim
      .replaceAll("\\s*(?:<!:)//[^\r\n]+", "")
      .replaceAll("\n *", "")
      .replaceAll("(?:GenericReqId|UseCaseId)\\((\\d+)\\)", "$1")
    val e = normaliseDOT(DOT(expect2))
    val a = normaliseDOT(actual)
    if (a != e) {
      println()
      println(actual.content)
      println()
      assertMultiline(actual = a, expect = e)
    }
  }

  def deleteReqs(id: ReqId*) =
    ReqsDelete(NonEmptySet force id.toSet, Set.empty, Text.empty)

  lazy val SIG_deadMF4: Project =
    applyEventsSuccessfully(SampleImplicationGraph.project, deleteReqs(SampleImplicationGraph.mf4))
}
