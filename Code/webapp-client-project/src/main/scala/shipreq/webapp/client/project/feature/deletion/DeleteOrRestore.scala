package shipreq.webapp.client.project.feature.deletion

import shipreq.base.util.IsoBool
import shipreq.webapp.base.data.{Dead, Live}

object DeleteOrRestore extends IsoBool.Object[DeleteOrRestore] {
  override def positive = Delete
  override def negative = Restore
}

sealed trait DeleteOrRestore extends IsoBool[DeleteOrRestore] {
  override final def companion = DeleteOrRestore
  def fromState: Live
  final def toState: Live = !fromState
}

case object Delete extends DeleteOrRestore {
  override def fromState = Live
}

case object Restore extends DeleteOrRestore {
  override def fromState = Dead
}
