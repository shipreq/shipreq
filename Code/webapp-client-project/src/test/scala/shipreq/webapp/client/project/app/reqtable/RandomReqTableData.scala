package shipreq.webapp.client.project.app.reqtable

import nyaya.gen._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.RandomData.reqtableData._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.Filter

object RandomReqTableData {

  val noFilter: Gen[Option[Filter.Valid]] =
    Gen pure None

  def tableSettings(p: Project, allowFilter: Boolean): Gen[TableSettings] =
    for {
      cs     ← visibleColumns(p)
      order  ← sortCriteria(cs)
      filter ← if (allowFilter) RandomData.filter.valid.forProject(p).option else noFilter
    } yield TableSettings(cs, order, filter)

}