package shipreq.webapp.base.feature.tablenav

import japgolly.scalajs.react._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import scala.annotation.elidable

object TableNavKeys {

  private implicit def autoSomeMovement(m: Movement): Option[Movement] =
    Some(m)

  @inline def apply(e: ReactKeyboardEventFromHtml): CallbackOption[Unit] =
    handler(e)

  val handler: ReactKeyboardEventFromHtml => CallbackOption[Unit] = e => {

    // The outer-only restriction prevents arrow-keys being overridden in textarea
    val outerOnly: CallbackOption[Unit] =
      CallbackOption.require(TableCellZipper.allowMove(e.target)) >> (
        CallbackOption.keyCodeSwitch(e) {
          case KeyCode.Up    => move(e, ↕ = Movement.Prev)
          case KeyCode.Down  => move(e, ↕ = Movement.Next)
          case KeyCode.Left  => move(e, ↔ = Movement.Prev)
          case KeyCode.Right => move(e, ↔ = Movement.Next)
          case KeyCode.Home  => move(e, ↔ = Movement.Head)
          case KeyCode.End   => move(e, ↔ = Movement.Last)
        } | CallbackOption.keyCodeSwitch(e, ctrlKey = true) {
          case KeyCode.Up    => move(e, ↕ = Movement.Head)
          case KeyCode.Down  => move(e, ↕ = Movement.Last)
          case KeyCode.Left  => move(e, ↔ = Movement.Head)
          case KeyCode.Right => move(e, ↔ = Movement.Last)
          case KeyCode.Home  => move(e, Movement.Head, Movement.Head)
          case KeyCode.End   => move(e, Movement.Last, Movement.Last)
        })

    val outerAndInner: CallbackOption[Unit] =
      CallbackOption.keyCodeSwitch(e) {
        case KeyCode.Escape => Callback(e.target.blur())
        case KeyCode.Tab    => subMove(e, Movement.Next)
      } | CallbackOption.keyCodeSwitch(e, shiftKey = true) {
        case KeyCode.Tab    => subMove(e, Movement.Prev)
      }

    CallbackOption.asEventDefault(e, outerOnly | outerAndInner)
  }

  @elidable(elidable.ASSERTION)
  private def onError(s: String): Unit =
    dom.console.error(s)

  private def move(e: ReactKeyboardEventFromHtml,
                   ↔ : Option[Movement] = None,
                   ↕ : Option[Movement] = None): Callback =
    Callback {
      import TableCellZipper.within
      (within(e.target) orElse within(e.currentTarget)) // not a fan of this but it works fine so far
        .flatMap(_.move(Axis.LeftRight, ↔))
        .flatMap(_.move(Axis.UpDown, ↕))
        .fold(onError, _.focus.focus())
    }

  private def subMove(e: ReactKeyboardEventFromHtml, m: Movement): Callback =
    Callback {
      TableCellZipper(e.target).subMove(m)
        .fold(onError, _.foreach(_.focus.focus()))
    }
}
