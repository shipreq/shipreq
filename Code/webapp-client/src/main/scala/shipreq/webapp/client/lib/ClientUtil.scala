package shipreq.webapp.client.lib

import japgolly.scalajs.react._, vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode
import shipreq.base.util.NonEmptyVector
import shipreq.webapp.client.data.{On, Off}
import shipreq.webapp.client.widgets.Widgets

object ClientUtil {

  private[this] var GLOBAL_VAR = 0

  val uniqueInt = CallbackTo[Int] {
    // JS is single-threaded
    GLOBAL_VAR += 1
    GLOBAL_VAR
  }

  val uniqueStr: CallbackTo[String] =
    uniqueInt.map(i => s"___uqs$i")

  def textChangeRecv[R](f: String => R): ReactEventI => R =
    e => f(e.target.value)

  def checkboxOfSetPresence[A](as: Set[A])(a: A, update: Set[A] => Callback): ReactTag = {
    val currentState = On <~ as.contains(a)
    def toggled = currentState match {
      case On  => as - a
      case Off => as + a
    }
    Widgets.checkbox(currentState)(^.onChange --> update(toggled))
  }

  /**
   * Clicking, or pressing space = change.
   */
  def checkboxLikeEventHandlers(onChange: Callback): TagMod = {
    def handleKey: ReactKeyboardEventH => Callback =
      DomUtil.keyCodeSwitch(_) {
        case KeyCode.Space => onChange
      }
    TagMod(
      ^.onClick   --> onChange,
      ^.onKeyDown ==> handleKey)
  }

  val sepComma: TagMod = ", "
  val sepSpace: TagMod = " "

  def renderVector[A, B](as: Vector[A], separator: TagMod)(renderEach: A => B)(implicit g: B => TagMod): ReactTag =
    <.span(
      NonEmptyVector.option(as)
        .map(_.intercalateF(separator)(g compose renderEach).whole))

}
