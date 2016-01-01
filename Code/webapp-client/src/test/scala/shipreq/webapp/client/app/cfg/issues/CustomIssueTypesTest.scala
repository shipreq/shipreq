package shipreq.webapp.client.app.cfg.issues

import japgolly.scalajs.react.test._
import scalaz.std.anyVal._
import shipreq.webapp.base.protocol.{CustomIssueTypeCrud, RemoteFn}
import shipreq.webapp.base.test.SampleProject
import shipreq.webapp.client.app.state.ClientData
import shipreq.webapp.client.data.HideDead
import shipreq.webapp.client.test.TestUtil._
import shipreq.webapp.client.test._
import utest._

object CustomIssueTypesTest extends TestSuite {

  override def tests = TestSuite {
    val remote     = RemoteFn.Instance("x", CustomIssueTypeCrud)
    val clientData = new ClientData(SampleProject.project)
    val cp         = new TestClientProtocol
    val props      = new CustomIssueTypes.Props(cp, remote, clientData, HideDead, MockRouterCtl())
    val re         = props.component
    val c          = ReactTestUtils.renderIntoDocument(re)

    def errors           = $(".errorMsg", c)
    def assertNoErrors() = assertEq("Error tag count", 0, errors.length)
    def assertError()    = assertEq("Error tag count", 1, errors.length)

    val i = sole(Sizzle(":text[value=TO"+"DO]", c))
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
    cp.assertReqsSent(0)
    Simulation.focusChangeBlur("BipBop") run i
    assertNoErrors()
    cp.assertReqsSent(1)
  }
}
