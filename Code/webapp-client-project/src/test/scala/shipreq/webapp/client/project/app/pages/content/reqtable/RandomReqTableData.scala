package shipreq.webapp.client.project.app.pages.content.reqtable

import nyaya.gen._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.RandomData.reqtableData._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.filter.Filter

object RandomReqTableData {

  val noFilter: Gen[Option[Filter.Valid]] =
    Gen pure None

  def view(p: Project, fd: FilterDead, allowFilter: Boolean): Gen[View] =
    for {
      cs     <- visibleColumns(p)
      order  <- sortCriteria(cs)
      filter <- if (allowFilter) RandomData.filter.valid.forProject(p).option else noFilter
    } yield View(cs, order, fd, filter)

}