package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import scalaz.effect.IO
import shipreq.webapp.base.data.{Dead, Alive}
import shipreq.webapp.base.protocol.DeletionAction
import DeletionAction._
import Deletion._

object Deletion {

  val buttonLabel: DeletionAction => String = {
    case Restore => "Restore"
    case SoftDel => "Delete"
    case HardDel => "Delete Forever"
  }
}

class Deletion[P, K](val alive: P => Alive, delIO: (K, DeletionAction) => IO[Unit]) {

  val filterAlive: P => Boolean =
    p => alive(p) match {
      case Alive => true
      case Dead  => false
    }

  def button(k: K, a: DeletionAction): ReactTag =
    <.button(buttonLabel(a), ^.onClick ~~> delIO(k, a))
}