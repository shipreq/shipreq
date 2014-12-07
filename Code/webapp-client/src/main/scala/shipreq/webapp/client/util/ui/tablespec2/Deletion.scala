package shipreq.webapp.client.util.ui.tablespec2

import shipreq.webapp.base.data.{Dead, Alive}
import shipreq.webapp.base.protocol.DeletionAction
import DeletionAction._

import scalaz.effect.IO

object Deletion {

  val buttonLabel: DeletionAction => String = {
    case Restore => "Restore"
    case SoftDel => "Delete"
    case HardDel => "Delete Forever"
  }
}

import Deletion._

class Deletion[P, K](val alive: P => Alive, delIO: (K, DeletionAction) => IO[Unit]) {

  val filterAlive: P => Boolean =
    p => alive(p) match {
      case Alive => true
      case Dead  => false
    }

  import japgolly.scalajs.react._, vdom.prefix_<*._, ScalazReact._

  def button(k: K, a: DeletionAction): Tag =
    <.button(buttonLabel(a), *.onclick ~~> delIO(k, a))
}