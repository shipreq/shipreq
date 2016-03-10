package shipreq.webapp.client.app

import japgolly.scalajs.react.test._
import utest._
import shipreq.webapp.base.event.{Delete, DeleteCustomField}
import shipreq.webapp.base.test._
import shipreq.webapp.client.app.reqtable.{ReqTableTestDsl => RT}
import shipreq.webapp.client.test._
import ProjectSpaMain.{Page, Props}
import SampleProject.Values.priField

object ProjectSpaTest extends TestSuite {
  import ProjectSpaTestDsl._

  PrepareEnv()

  def reqTableAfterLocalConfigUpdate: *.Action = (
    testReqTable(RT.showHideColumn("Priority") >> RT.sortBy("Priority"))
      >> setPage(Page.CfgFields)
      >> applyEvents("Delete Priority field", DeleteCustomField(priField, Delete))
      >> setPage(Page.ReqTable)
      >> testReqTable()
  )

  override def tests = TestSuite {
    'reqTableAfterLocalConfigUpdate - runTest(reqTableAfterLocalConfigUpdate)
  }
}
