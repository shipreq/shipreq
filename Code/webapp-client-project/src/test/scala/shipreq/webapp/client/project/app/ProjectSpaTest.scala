package shipreq.webapp.client.project.app

import japgolly.scalajs.react.test._
import utest._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{ShowDead, HideDead}
import shipreq.webapp.base.event.{Delete, DeleteCustomField}
import shipreq.webapp.base.test._
import shipreq.webapp.client.project.app.reqtable.{ReqTableTestDsl => RT}
import shipreq.webapp.client.project.app.reqdetail.{ReqDetailTestDsl => RD}
import shipreq.webapp.client.project.test._
import ProjectSpaMain.{Page, Props}
import SampleProject.Values.priField
import TestState._
import UnsafeTypes._

object ProjectSpaTest extends TestSuite {
  import ProjectSpaTestDsl._

  PrepareEnv()

  def `ReqTable columns after local config change`: *.Actions = (
    testReqTable(RT.showHideColumn("Priority") >> RT.sortBy("Priority"))
      >> setPage(Page.CfgFields)
      >> applyEvents("Delete Priority field", DeleteCustomField(priField, Delete))
      >> setPage(Page.ReqTable)
  )

  def `ReqTable filterDead after change on detail page`: *.Actions =
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
