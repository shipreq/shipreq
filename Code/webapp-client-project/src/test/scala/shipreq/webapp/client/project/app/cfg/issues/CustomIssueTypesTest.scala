package shipreq.webapp.client.project.app.cfg.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.webapp.base.data.{FilterDead, HideDead}
import shipreq.webapp.base.test.SampleProject
import shipreq.webapp.client.project.app.cfg.shared.Usage
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.test.TestUtil._
import shipreq.webapp.client.project.test._
import teststate.domzipper.sizzle.Sizzle
import utest._

object CustomIssueTypesTest extends TestSuite {

  override def tests = Tests {
    val fd    = ReactTestVar[FilterDead](HideDead)
    val g     = TestGlobal(SampleProject.project)
    val props = CustomIssueTypes.Props(g.sspUpdateConfig, g, fd.stateSnapshotWithReuse(), Usage.Show((_, _) => <.a))
    val re    = props.component
    val c     = ReactTestUtils.renderIntoDocument(re)

    def errors           = Sizzle(".errorMsg", c)
    def assertNoErrors() = assertEq("Error tag count", 0, errors.length)
    def assertError()    = assertEq("Error tag count", 1, errors.length)

    val i = sole(Sizzle(":text", c).filter(_.domCast[html.Input].value == "TO"+"DO"))
    assertNoErrors()

    // Uniqueness should extend over tag keys
    Simulation.focusChangeBlur("pri=high") run i
    assertError()

    // Uniqueness should extend over other issue keys
    Simulation.focusChangeBlur("TBD") run i
    assertError()

    // Uniqueness should ignore itself
    Simulation.focusChangeBlur("TO"+"DO") run i
    assertNoErrors()

    // Save only on valid change
    g.assertReqsSent(0)
    Simulation.focusChangeBlur("BipBop") run i
    assertNoErrors()
    g.assertReqsSent(1)
  }
}
