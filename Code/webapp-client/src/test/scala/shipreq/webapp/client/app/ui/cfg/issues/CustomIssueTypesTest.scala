package shipreq.webapp.client.app.ui.cfg.issues

import japgolly.scalajs.react.test._
import shipreq.webapp.base.protocol.Routine
import shipreq.webapp.base.protocol.Routines.CustomIssueTypeCrud
import shipreq.webapp.base.test.SampleProject
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.test.TestUtil._
import shipreq.webapp.client.test._
import utest._

import scalaz.std.AllInstances._

object CustomIssueTypesTest extends TestSuite {

  override def tests = TestSuite {
    val remote     = Routine.Remote("x", CustomIssueTypeCrud)
    val clientData = new ClientData(SampleProject.project)
    val cp         = new TestClientProtocol
    val props      = new CustomIssueTypes.Props(cp, remote, clientData, false)
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
    cp.assertCommsSent(0)
    Simulation.focusChangeBlur("BipBop") run i
    assertNoErrors()
    cp.assertCommsSent(1)
  }
}
