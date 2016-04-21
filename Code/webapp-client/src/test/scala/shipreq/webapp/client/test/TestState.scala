package shipreq.webapp.client.test

import org.scalajs.dom.ext.{KeyCode, KeyValue}
import japgolly.scalajs.react.test.KeyboardEventData
import shipreq.base.util.DebugImplicits

object TestState
  extends testate.Exports
     with testate.TestateScalaz
     with testate.TestateNyaya
     with DebugImplicits {

  implicit val displayTestReq: Display[TestClientProtocol.Req] =
    Display(i => s"${i.r.fn}: ${i.input}")

  val CtrlEnter = KeyboardEventData(key = KeyValue.Enter, keyCode = KeyCode.Enter, ctrlKey = true)
  val Escape    = KeyboardEventData(key = KeyValue.Escape, keyCode = KeyCode.Escape)
}
