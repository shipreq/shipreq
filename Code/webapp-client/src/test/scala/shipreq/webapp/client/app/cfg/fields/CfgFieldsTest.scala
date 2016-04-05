package shipreq.webapp.client.app.cfg.fields

import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import scalaz.{Equal, \/-}
import utest._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.{FieldCrud, RemoteFn}
import shipreq.webapp.base.test.{SampleProject => S}
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.client.data.{FilterDead, HideDead}
import shipreq.webapp.client.test.TestUtil._
import shipreq.webapp.client.test._
import MainTable.State

object CfgFieldsTest extends TestSuite {
  PrepareEnv()

  val remote = RemoteFn.Instance("x", FieldCrud.Fn)
  class Tester {
    lazy val filterDead = ReactTestVar[FilterDead](HideDead)
    lazy val clientData = TestClientData(S.project)
    lazy val cp         = new TestClientProtocol
    lazy val props      = new CfgFields.Props(cp, remote, clientData, filterDead.reusableVar())
    lazy val re         = MainTable.Component(props)
    lazy val c          = ReactTestUtils.renderIntoDocument(re)

    def selectNewText() =
      c.modState(State.newFieldTypeSel set \/-(CustomFieldType.Text))

    lazy val createButton =
      sole(Sizzle("button:contains('Create')", c))

    def clickCreate() =
      Simulation.click run createButton

    def getNewRow =
      Sizzle("tr.new", c).headOption
  }

  implicit val stateEquality = Equal.equalA[State]

  override def tests = TestSuite {
    val t = new Tester
    import t._

    selectNewText()
    def html = c.getDOMNode().outerHTML
    val initialView = html

    // Create new text row
    assert(getNewRow.isEmpty)
    clickCreate()
    val newRow = getNewRow.get

    // Enter data
    Simulation.focusChangeBlur("blahh").runN(
      sole(Sizzle(".name :text", newRow)),
      sole(Sizzle(".key :text", newRow)))

    // Server communication
    cp.assertReqsSent(1)
    cp.respondToLast(remote){
      import CustomTextFieldGD._
      val e = CreateCustomTextField(666, nev(Name("blahh"), Key("blahh"), Mandatory(true), ReqTypes(allReqTypes)))
      verifyEvents(clientData.project)(e)
    }
    assert(getNewRow.isEmpty)

    // Delete newly saved row
    Simulation.click run sole(Sizzle("tr:has(:text[value=blahh]) button:contains('Delete')", c))
    cp.assertReqsSent(2)
    cp.respondToLast(remote)(
      verifyEvents(clientData.project)(DeleteCustomField(666.CFText, Delete)))

    assertEq(html, initialView)
  }
}
