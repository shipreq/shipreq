package shipreq.webapp.server.util

import shipreq.base.util.IsoBool

/** Communication Direction */
sealed trait CommDir extends IsoBool[CommDir] {
  override final def companion = CommDir
}

object CommDir extends IsoBool.Object[CommDir] {
  override def positive = Send
  override def negative = Recv

  case object Send extends CommDir
  case object Recv extends CommDir
}
