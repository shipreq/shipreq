package shipreq.webapp.client.base.test

import japgolly.microlibs.testutil.TestUtil
import japgolly.scalajs.react.test._
import japgolly.univeq.UnivEq
import org.scalajs.dom.Element
import org.scalajs.dom.ext.{KeyCode, KeyValue}
import scalacss.internal.StyleA
import teststate.run.Report.AssertionSettings
import shipreq.base.util.DebugImplicits

object TestState
 extends teststate.Exports
    with teststate.domzipper.sizzle.Exports
    with teststate.ExtNyaya
    with teststate.ExtScalaJsReact
    with teststate.ExtScalaz
    with DebugImplicits {

  implicit class StyleAExt(private val self: StyleA) extends AnyVal {
    def selector: String =
      "." + self.className.value
  }


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

  // TODO Patch TestState to support using custom fail instead of throwing
  def assertTestState(r: Report[String], onFailure: => Unit = ())(implicit as: AssertionSettings, se: DisplayError[String]): Unit =
    r.failureReason match {
      case None =>
        as.onPass.print(r)
      case Some(f) =>
        onFailure
        as.onFail.print(r)
        f.cause.foreach(_.printStackTrace())
        TestUtil.fail(f.failure)
    }
}
