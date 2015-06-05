package shipreq.webapp.client.app.ui.cfg.fields

import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import scalaz.{Equal, \/-}
import utest._
import shipreq.base.util.ISubset
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta.{Partition, RemoteDeltaG}
import shipreq.webapp.base.protocol.FieldProtocol._
import shipreq.webapp.base.protocol.Routine
import shipreq.webapp.base.protocol.Routines.FieldCrud
import shipreq.webapp.base.test.{SampleProject => S}
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib.HideDead
import shipreq.webapp.client.test.TestUtil._
import shipreq.webapp.client.test._
import MainTable.State

object CfgFieldsTest extends TestSuite {

  val remote = Routine.Remote("x", FieldCrud)
  class Tester {
    lazy val clientData = new ClientData(S.project)
    lazy val cp         = new TestClientProtocol
    lazy val props      = new CfgFields.Props(cp, remote, clientData, HideDead)
    lazy val re         = MainTable.Component(props)
    lazy val c          = ReactTestUtils.renderIntoDocument(re)

    var rev = S.fields.rev

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
    val initialState = c.state

    // Create new text row
    assert(getNewRow.isEmpty)
    clickCreate()
    val newRow = getNewRow.get

    // Enter data
    Simulation.focusChangeBlur("blahh").runN(
      sole(Sizzle(".name :text", newRow)),
      sole(Sizzle(".key :text", newRow)))

    // Server communication
    cp.assertCommsSent(1)
    rev = rev.succ
    cp.respondToLastSuccessfully(remote){
      val newField = CustomField.Text(666, "blahh", "blahh", Mandatory, ISubset.All(), Live)
      val deltaG   = RemoteDeltaG(Partition.Fields, rev, rev)(Set.empty, List(Delta(\/-(newField), None)))
      List(deltaG)
    }
    assert(getNewRow.isEmpty)

    // Delete newly saved row
    Simulation.click run sole(Sizzle("tr:has(:text[value=blahh]) button:contains('Delete')", c))
    cp.assertCommsSent(2)
    rev = rev.succ
    cp.respondToLastSuccessfully(remote)(List(
      RemoteDeltaG(Partition.Fields, rev, rev)(Set(CustomField.Text.Id(666)), Nil)
    ))

    assertEq(c.state, initialState)
  }
}
