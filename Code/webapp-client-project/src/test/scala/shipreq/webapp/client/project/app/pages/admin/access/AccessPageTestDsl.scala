package shipreq.webapp.client.project.app.pages.admin.access

import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.test._

object AccessPageTestDsl {

  final case class Ref(global   : TestGlobal,
                       confirmJs: TestConfirmJs)

  val * = Dsl[Ref, AccessPageObs, Unit]

  val global = new TestGlobal.TestDslWithObs(*)(_.global, _.global)

  val confirmJs = new TestConfirmJs.TestDsl(*)(_.confirmJs, _.confirmJs)

  val invariants: *.Invariants =
    *.emptyInvariant

  // ===================================================================================================================

  val leaveProjectButtonLoading =
    *.focus("Leave button loading").value(_.obs.leaveProjectButtonLoading)

  // ===================================================================================================================

  val clickLeaveProject: *.Actions =
    *.action("Click 'Leave This Project'")(_.obs.leaveProjectButton.click())
}
