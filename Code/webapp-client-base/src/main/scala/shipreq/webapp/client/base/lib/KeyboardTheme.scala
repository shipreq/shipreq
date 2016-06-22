package shipreq.webapp.client.base.lib

import japgolly.scalajs.react._
import org.scalajs.dom.ext.KeyCode
import shipreq.webapp.base.text.{LineCardinality, MultiLine, SingleLine}
import shipreq.webapp.client.base.lib.KeyHandler._

/**
  * Keyboard functionality consistent throughout the entire app.
  */
object KeyboardTheme {
  val Escape    = Criterion(EventType.KeyDown , KeyCode.Escape)
  val Enter     = Criterion(EventType.KeyPress, KeyCode.Enter)
  val CtrlEnter = Criterion(EventType.KeyDown , KeyCode.Enter, ModKey.Ctrl)

  def abort(abort: => Callback): KeyHandler =
    Escape.handle(abort)

  /**
   * - Enter either inserts a newline, or commits.
   * - Ctrl-enter commits.
   */
  def commit(commit: => Option[Callback], lc: LineCardinality): List[KeyHandler] = {
    val base = CtrlEnter.handle(Callback sequenceO commit)

    // If enter unused, use for commit too
    lc match {
      case SingleLine => Enter.handle(base.response) :: base :: Nil
      case MultiLine  => base :: Nil
    }
  }

}
