package shipreq.webapp.client.test

import shipreq.base.util.DebugImplicits

object TestState
  extends testate.Exports
     with testate.TestStateScalaz
     with testate.TestStateNyaya
     with DebugImplicits {

  implicit val displayTestReq: Display[TestClientProtocol.Req] =
    Display(i => s"${i.r.fn}: ${i.input}")
}
