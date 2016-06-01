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

  val DownKey   = KeyboardEventData(key = KeyValue.ArrowDown , keyCode = KeyCode.Down  )
  val UpKey     = KeyboardEventData(key = KeyValue.ArrowUp   , keyCode = KeyCode.Up    )
  val LeftKey   = KeyboardEventData(key = KeyValue.ArrowLeft , keyCode = KeyCode.Left  )
  val RightKey  = KeyboardEventData(key = KeyValue.ArrowRight, keyCode = KeyCode.Right )
  val Home      = KeyboardEventData(key = KeyValue.Home      , keyCode = KeyCode.Home  )
  val End       = KeyboardEventData(key = KeyValue.End       , keyCode = KeyCode.End   )
  val Escape    = KeyboardEventData(key = KeyValue.Escape    , keyCode = KeyCode.Escape)
  val Enter     = KeyboardEventData(key = KeyValue.Enter     , keyCode = KeyCode.Enter )
  val F2        = KeyboardEventData(key = KeyValue.F2        , keyCode = KeyCode.F2    )
}
