package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.scalajs.react._
import vdom.html_<^._
import japgolly.univeq.UnivEq
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.webapp.base.data.{Dead, Live}
import shipreq.webapp.client.project.widgets.LifeButton

// This now disgusts me.

sealed abstract class DeletionAction {
  def targetState: Live
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
}

/**
 * Restore [[shipreq.webapp.base.data.Dead]] data back to [[shipreq.webapp.base.data.Live]].
 */
case object Restore extends DeletionAction {
  override def targetState = Live
}

object Deletion {

  val buttonLabel: DeletionAction => String = {
    case Restore => "Restore"
    case Delete  => "Delete"
  }
}

final case class Deletion[K](delIO: (K, DeletionAction) => Callback) extends AnyVal {

  def button(k: K, a: DeletionAction): VdomTag =
    a match {
      case Delete  => LifeButton.Delete(delIO(k, a))
      case Restore => LifeButton.Restore(delIO(k, a))
    }
}