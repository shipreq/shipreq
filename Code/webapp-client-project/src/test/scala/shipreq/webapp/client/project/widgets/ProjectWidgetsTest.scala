package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.webapp.client.project.test._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text._
import shipreq.webapp.member.test.project.SampleProject3
import shipreq.webapp.member.test.WebappTestUtil
import utest._

object ProjectWidgetsTest extends TestSuite with WebappTestUtil {

  PrepareEnv()

  private def stripClasses(html: String): String =
    html.replaceAll("""\s?class="[^"]*"""", "")

  private def stripHref(html: String): String =
    html.replaceAll("""\s?href="[^"]*"""", "")

  private def stripSpans(html: String): String =
    html.replaceAll("""<span[^>]*>""", "").replaceAll("</span>", "")

  override def tests = Tests {

    "nestedLink" - {
      import SampleProject3.Values._
      val project = SampleProject3.project
      val pt = PlainText.ForProject.noCtx(project)
      val viewTags = ViewTags(project)
      val reqDetailRC = MockRouterCtl[ExternalPubid]()
      val webWorker = TestWebWorkerClient()

      val pw = ProjectWidgets(project, pt, viewTags, reqDetailRC, webWorker)

      // frs(2) title contains a reference to MF-28 and MF-26.
      val ids = Vector(project.content.reqs.need(frs(2)).pubid)

      // We render a multi-line implication list.
      // Each item is a link to the requirement, and includes the requirement title.
      val vdom = pw.implicationList(ids, Live, Mandatory, ProjectText.SetRenderStyle.MultiLineDetailed)

      // Wrap VdomTag in a static component to render it
      val C = ScalaComponent.static("Test")(vdom)

      ReactTestUtils.withRenderedIntoDocument(C()) { m =>
        var actual = m.getDOMNode.asMounted().domCast[html.Element].outerHTML
        actual = stripClasses(actual)
        actual = stripHref(actual)
        actual = stripSpans(actual)

        // This is the expected HTML AFTER the fix.
        // Nested [MF-26] and [MF-28] should be spans, not links.
        val expected = """<ul><li><a>FR-2: #TBD{Pending [MF-26]}. [MF-28] is dead.</a></li></ul>"""

        assertEq(actual, expected)
      }
    }
  }
}
