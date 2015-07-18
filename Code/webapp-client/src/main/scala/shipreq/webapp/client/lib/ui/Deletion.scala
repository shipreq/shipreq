package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import scalaz.effect.IO
import shipreq.webapp.base.event.{DeletionAction, HardDel, SoftDel, Restore}
import Deletion._

object Deletion {

  val buttonLabel: DeletionAction => String = {
    case Restore => "Restore"
    case SoftDel => "Delete"
    case HardDel => "Delete Forever"
  }
}

class Deletion[K](delIO: (K, DeletionAction) => IO[Unit]) {

  def button(k: K, a: DeletionAction): ReactTag =
    <.button(buttonLabel(a), ^.onClick ~~> delIO(k, a))
}