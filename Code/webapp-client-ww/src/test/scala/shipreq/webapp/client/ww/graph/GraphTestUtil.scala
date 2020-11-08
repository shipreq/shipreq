package shipreq.webapp.client.ww.graph

import shipreq.webapp.client.ww.graph.GraphViz.DOT
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.text.Text
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.{SampleImplicationGraph, TestEvent}
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
      println(a)
      println()
      assertMultiline(actual = a, expect = e)
    }
  }

  def deleteReqs(id: ReqId*) =
    ReqsDelete(NonEmptySet force id.toSet, Set.empty, Text.empty)

  lazy val SIG_dead_FR7: Project =
    applyEventsSuccessfully(SampleImplicationGraph.project, deleteReqs(SampleImplicationGraph.fr7))

  lazy val SIG_dead_FR7_MF4: Project =
    applyEventsSuccessfully(SIG_dead_FR7, deleteReqs(SampleImplicationGraph.mf4))

  lazy val SIG_FRs_optional: Project = {
    import shipreq.webapp.member.test.project.SampleProject.Values._
    val e = TestEvent.customReqTypeUpdate(fr, implication = Optional)
    applyEventSuccessfully(SampleImplicationGraph.project, e)
  }

}
