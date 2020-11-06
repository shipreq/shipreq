package shipreq.webapp.client.project.app.pages.content.reqtable

import nyaya.gen._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview._
import shipreq.webapp.member.project.filter.Filter
import shipreq.webapp.member.test.project.RandomData
import shipreq.webapp.member.test.project.RandomData.savedViews._

object RandomSavedView {

  val noFilter: Gen[Option[Filter.Valid]] =
    Gen pure None

  def view(p: Project, fd: FilterDead, allowFilter: Boolean): Gen[View] =
    for {
      cs     <- visibleColumns(p)
      order  <- sortCriteria(cs)
      filter <- if (allowFilter) RandomData.filter.valid.forProject(p).option else noFilter
      cfg    <- impGraphConfigForProject(p).option
    } yield View(cs, order, fd, filter, cfg)
}