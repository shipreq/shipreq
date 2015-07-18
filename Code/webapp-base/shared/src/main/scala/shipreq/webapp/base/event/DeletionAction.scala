package shipreq.webapp.base.event

import shipreq.base.util.{NonEmptyVector, UnivEq}

sealed abstract class DeletionAction
object DeletionAction {
  def values = NonEmptyVector[DeletionAction](SoftDel, Restore, HardDel)
  @inline implicit def equality: UnivEq[DeletionAction] = UnivEq.force
}

sealed abstract class SoftDeletionAction extends DeletionAction
object SoftDeletionAction {
  def values = NonEmptyVector[SoftDeletionAction](SoftDel, Restore)
  @inline implicit def equality: UnivEq[SoftDeletionAction] = UnivEq.force
}

/**
 * Permanently remove data.
 */
case object HardDel extends DeletionAction

/**
 * Mark data as being [[shipreq.webapp.base.data.Dead]].
 */
case object SoftDel extends SoftDeletionAction

/**
 * Restore [[shipreq.webapp.base.data.Dead]] data back to [[shipreq.webapp.base.data.Live]].
 */
case object Restore extends SoftDeletionAction
