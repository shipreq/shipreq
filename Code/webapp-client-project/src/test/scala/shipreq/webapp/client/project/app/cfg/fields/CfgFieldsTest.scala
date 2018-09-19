package shipreq.webapp.client.project.app.cfg.fields

import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import scala.scalajs.js.JSConverters._
import org.scalajs.dom.Element
import org.scalajs.dom.html.Input
import scalaz.{Equal, \/-}
import teststate.domzipper.sizzle.Sizzle
import utest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol.{FieldCrud, ServerSideProc}
import shipreq.webapp.base.test.{SampleProject => S}
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.TestClientProtocol
import shipreq.webapp.base.test.TestState.htmlScrub
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.test.TestUtil._
import shipreq.webapp.client.project.test._
import MainTable.State

object CfgFieldsTest extends TestSuite {
  PrepareEnv()

  val remote = ServerSideProc("x", FieldCrud.Protocol)
  class Tester {
    lazy val fd    = ReactTestVar[FilterDead](HideDead)
    lazy val cd    = TestClientData(S.project)
    lazy val cp    = new TestClientProtocol(true)
    lazy val props = new CfgFields.Props(cp, remote, cd, fd.stateSnapshotWithReuse())
    lazy val re    = MainTable.Component(props)
    lazy val c     = ReactTestUtils.renderIntoDocument(re)

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

  override def tests = Tests {
    val t = new Tester
    import t._

    selectNewText()
    def html = c.getDOMNode.asElement.outerHTML
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
    cp.respondToLast(remote) {
      import CustomTextFieldGD._
      val e = FieldCustomTextCreate(666, nev(Name("blahh"), Key("blahh"), Mandatory(true), ReqTypes(allReqTypes)))
      \/-(cd.verifyEventsCB(e).runNow())
    }
    assert(getNewRow.isEmpty)

    // Delete newly saved row
    // Simulation.click run sole(Sizzle("tr:has(:text[value=blahh]) button:contains('Delete')", c))
    Simulation.click run sole(
      Sizzle("tr:has(:text)", c)
        .toArray
        .filter(Sizzle(":text", _).toArray.exists(_.domCast[Input].value == "blahh"))
        .flatMap(Sizzle("button:contains('Delete')", _).toArray[Element])
        .toJSArray)
    cp.assertReqsSent(2)
    cp.respondToLast(remote)(
      \/-(cd.verifyEventsCB(FieldCustomDelete(666.CFText)).runNow()))

    assertEq(htmlScrub run html, htmlScrub run initialView)
  }
}
