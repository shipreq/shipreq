package shipreq.webapp.client.project.app.cfg.shared

import japgolly.scalajs.react._, vdom.prefix_<^._
import shipreq.webapp.base.event.{DeletionAction, Delete, Restore}
import Deletion._

object Deletion {

  val buttonLabel: DeletionAction => String = {
    case Restore => "Restore"
    case Delete  => "Delete"
  }
}

class Deletion[K](delIO: (K, DeletionAction) => Callback) {

  def button(k: K, a: DeletionAction): ReactTag =
    <.button(buttonLabel(a), ^.onClick --> delIO(k, a))
}