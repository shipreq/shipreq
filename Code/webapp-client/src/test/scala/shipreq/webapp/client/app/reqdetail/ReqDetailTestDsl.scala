package shipreq.webapp.client.app.reqdetail

import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data.Project
import teststate.{Check, Dsl}

object ReqDetailTestDsl {
  case class TestState(project: Project, expectedError: Option[String])

  val * = Dsl.sync[Unit, ReqDetailObs, TestState, String]

  private def invariantsWhenOk: *.Check =
    Check.empty

  private def invariantsWhenBad: *.Check =
    *.focus("Error message").obsAndState(_.error.reason.some, _.expectedError).assert.equal

  val invariants: *.Check =
    *.focus("isOk").obsAndState(_.isOk, _.expectedError.isEmpty).assert.equal &
    invariantsWhenOk.when(_.obs.isOk) &
    invariantsWhenBad.when(!_.obs.isOk)
}
