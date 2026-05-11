package shipreq.webapp.member.test.project

import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.WebappTestUtil._

object TemplateProjects {

  private def create(t: ProjectTemplate): Project =
    applyEventSuccessfully(emptyProject1, Event.ProjectTemplateApply(t))

  // This is like this and not simply `object V1 {...}` because that somehow causes phantomjs
  // to crash when running WCP tests. (?)
  lazy val V1 = new V1
  final class V1 {
    val project = create(ProjectTemplate.V1)
    val priTG   = TagGroupId(5)
  }
}
