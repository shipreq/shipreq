package shipreq.webapp.base.data

import nyaya.prop._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data.ProjectCatalogue.Item

case class ProjectCatalogueProps(item: Item, project: Project, eventCount: Int) {
  def assert(): Unit =
    ProjectCatalogueProps.All assert this
}

object ProjectCatalogueProps {

  type P = ProjectCatalogueProps

  val ProjectName = Prop.equal[P]("Project name")(_.project.name, _.item.name)

  val ReqCount = Prop.equal[P]("Req count")(_.project.reqs.size, _.item.reqCount)

  val EventCount = Prop.equal[P]("Event count")(_.eventCount, _.item.eventCount)

  val All: Prop[P] =
    ProjectName & ReqCount & EventCount
}
