package shipreq.webapp.base.event

import japgolly.univeq.UnivEq
import shipreq.base.util.NonEmptyVector
import shipreq.webapp.base.data.{Dead, Live}

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
