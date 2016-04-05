package shipreq.webapp.client.app

import japgolly.scalajs.react.test._
import utest._
import shipreq.base.util.univeq._
import shipreq.webapp.base.event.{Delete, DeleteCustomField}
import shipreq.webapp.base.test._
import shipreq.webapp.client.app.reqtable.{ReqTableTestDsl => RT}
import shipreq.webapp.client.app.reqdetail.{ReqDetailTestDsl => RD}
import shipreq.webapp.client.data.{ShowDead, HideDead}
import shipreq.webapp.client.test._
import ProjectSpaMain.{Page, Props}
import SampleProject.Values.priField
import TestState._
import UnsafeTypes._

object ProjectSpaTest extends TestSuite {
  import ProjectSpaTestDsl._

  PrepareEnv()

  def `ReqTable columns after local config change`: *.Action = (
    testReqTable(RT.showHideColumn("Priority") >> RT.sortBy("Priority"))
      >> setPage(Page.CfgFields)
      >> applyEvents("Delete Priority field", DeleteCustomField(priField, Delete))
      >> setPage(Page.ReqTable)
  )

  def `ReqTable filterDead after change on detail page`: *.Action =
    ( setPageToReqDetail("FR-1", RD.Mode.Details)
      >> testReqDetail(RD.filterDeadToggle)
      >> setPage(Page.ReqTable)
    )
    .addCheck(RT.filterDead.assert.change.lift)
    .times(3)

  override def tests = TestSuite {

    'reqTableColumnsSync - runTest(`ReqTable columns after local config change`)

    'reqTableFilterDeadSync - runTest(`ReqTable filterDead after change on detail page`)
  }
}
