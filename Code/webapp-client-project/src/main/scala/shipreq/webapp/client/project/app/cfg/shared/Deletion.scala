package shipreq.webapp.client.project.app.cfg.shared

import japgolly.scalajs.react._, vdom.html_<^._
import japgolly.univeq.UnivEq
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.webapp.base.data.{Dead, Live}
import Deletion._
import shipreq.webapp.base.protocol.CrudAction

// This now disgusts me.

sealed abstract class DeletionAction {
  def targetState: Live

  def crudAction[Id, V](id: Id): CrudAction[Id, V]
}
object DeletionAction {
  def values = NonEmptyVector[DeletionAction](Delete, Restore)
  @inline implicit def equality: UnivEq[DeletionAction] = UnivEq.force
}

/**
 * Mark data as being [[shipreq.webapp.base.data.Dead]].
 */
case object Delete extends DeletionAction {
  override def targetState = Dead
  override def crudAction[Id, V](id: Id) = CrudAction.Delete[Id, V](id)
}

/**
 * Restore [[shipreq.webapp.base.data.Dead]] data back to [[shipreq.webapp.base.data.Live]].
 */
case object Restore extends DeletionAction {
  override def targetState = Live
  override def crudAction[Id, V](id: Id) = CrudAction.Restore[Id, V](id)
}

object Deletion {

  val buttonLabel: DeletionAction => String = {
    case Restore => "Restore"
    case Delete  => "Delete"
  }
}

class Deletion[K](delIO: (K, DeletionAction) => Callback) {

  def button(k: K, a: DeletionAction): VdomTag =
    <.button(buttonLabel(a), ^.onClick --> delIO(k, a))
}