package shipreq.webapp.client.base.test

import japgolly.scalajs.react.test._
import japgolly.univeq.UnivEq
import org.scalajs.dom.Element
import org.scalajs.dom.ext.{KeyCode, KeyValue}
import shipreq.base.util.DebugImplicits

object TestState
 extends teststate.Exports
    with teststate.domzipper.sizzle.Exports
    with teststate.ExtNyaya
    with teststate.ExtScalaJsReact
    with teststate.ExtScalaz
    with DebugImplicits {

  implicit val displayTestReq: Display[TestClientProtocol.Req] =
    Display(i => s"${i.r.fn}: ${i.input}")

  // TODO Add to DomZipper exports
  implicit def univEqDomElement[D <: Element] = UnivEq.force[D]

  val DownKey   = SimEvent.Keyboard(key = KeyValue.ArrowDown , keyCode = KeyCode.Down  )
  val UpKey     = SimEvent.Keyboard(key = KeyValue.ArrowUp   , keyCode = KeyCode.Up    )
  val LeftKey   = SimEvent.Keyboard(key = KeyValue.ArrowLeft , keyCode = KeyCode.Left  )
  val RightKey  = SimEvent.Keyboard(key = KeyValue.ArrowRight, keyCode = KeyCode.Right )
  val Home      = SimEvent.Keyboard(key = KeyValue.Home      , keyCode = KeyCode.Home  )
  val End       = SimEvent.Keyboard(key = KeyValue.End       , keyCode = KeyCode.End   )
  val Escape    = SimEvent.Keyboard(key = KeyValue.Escape    , keyCode = KeyCode.Escape)
  val Enter     = SimEvent.Keyboard(key = KeyValue.Enter     , keyCode = KeyCode.Enter )
  val CtrlEnter = SimEvent.Keyboard(key = KeyValue.Enter     , keyCode = KeyCode.Enter , ctrlKey = true)
  val F2        = SimEvent.Keyboard(key = KeyValue.F2        , keyCode = KeyCode.F2    )
}
