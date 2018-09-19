package shipreq.webapp.base.event

import utest._
import ApplyEventTestFns._
import NoInitialEvents._

object TemplateTest extends TestSuite {

  override def tests = Tests {

    // All templates must apply to a new project
    for (t <- ProjectTemplate.values)
      assertPass(ProjectTemplateApply(t))
  }
}
