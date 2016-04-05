package shipreq.webapp.client.test

import shipreq.base.util.DebugImplicits

object TestState
  extends testate.Exports
     with testate.TestateScalaz
     with testate.TestateNyaya
     with DebugImplicits {

  implicit val displayTestReq: Display[TestClientProtocol.Req] =
    Display(i => s"${i.r.fn}: ${i.input}")
}
