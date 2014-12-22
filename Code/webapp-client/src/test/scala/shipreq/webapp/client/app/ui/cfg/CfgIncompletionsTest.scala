package shipreq.webapp.client.app.ui.cfg

import japgolly.scalajs.react.test._
import scalaz.std.AllInstances._
import utest._
import shipreq.webapp.base.protocol.Routine
import shipreq.webapp.base.protocol.Routines.CustomIncmpTypeCrud
import shipreq.webapp.client.test._
import TestUtil._

object CfgIncompletionsTest extends TestSuite {

  override def tests = TestSuite {
    val remote     = Routine.Remote("x", CustomIncmpTypeCrud)
    val clientData = SampleProject.clientData
    val cp         = new TestClientProtocol
    val props      = new CfgIncompletions.UserDefIncompletions.Props(cp, remote, clientData, false)
    val re         = CfgIncompletions.UserDefIncompletions.Component(props)
    val c          = ReactTestUtils.renderIntoDocument(re)

    def errors           = $(".errorMsg", c)
    def assertNoErrors() = assertEq("Error tag count", 0, errors.length)
    def assertError()    = assertEq("Error tag count", 1, errors.length)

    val i = sole(Sizzle(":text[value=TODO]", c))
    assertNoErrors()

    // Uniqueness should extend over tag keys
    Simulation.focusChangeBlur("pri=high") run i
    assertError()

    // Uniqueness should extend over other incmp keys
    Simulation.focusChangeBlur("TBD") run i
    assertError()

    // Uniqueness should ignore itself
    Simulation.focusChangeBlur("TODO") run i
    assertNoErrors()

    // Save only on valid change
    cp.assertCommsSent(0)
    Simulation.focusChangeBlur("BipBop") run i
    assertNoErrors()
    cp.assertCommsSent(1)
  }
}
