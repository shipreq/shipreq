package shipreq.webapp.base.event

import shipreq.webapp.base.event.ApplyEventTestFns._
import shipreq.webapp.base.event.Event._
import shipreq.webapp.base.event.NoInitialEvents._
import utest._

object TemplateTest extends TestSuite {

  override def tests = Tests {

    // All templates must apply to a new project
    for (t <- ProjectTemplate.values)
      assertPass(ProjectTemplateApply(t))
  }
}
