package shipreq.webapp.member.project.event

import shipreq.webapp.member.project.event.ApplyEventTestFns._
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.event.NoInitialEvents._
import utest._

object TemplateTest extends TestSuite {

  override def tests = Tests {

    // All templates must apply to a new project
    for (t <- ProjectTemplate.values)
      assertPass(ProjectTemplateApply(t))
  }
}
