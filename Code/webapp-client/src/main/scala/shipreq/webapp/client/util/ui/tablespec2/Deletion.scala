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

class Deletion[P](health: P => Alive) {

  val filterAlive: P => Boolean =
    p => health(p) match {
      case Alive => true
      case Dead  => false
    }

  import japgolly.scalajs.react._, vdom.ReactVDom.{all => *, _}, implicits._, ScalazReact._
  import vdom.ReactVDom.{all => <}

  def button(a: DeletionAction, io: IO[Unit]) =
    <.button(buttonLabel(a), *.onclick ~~> io)
}