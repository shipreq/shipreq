package shipreq.webapp.base.util

import shipreq.base.util.IsoBool

sealed trait Open extends IsoBool[Open] {
  override final def companion = Open
}

case object Open extends Open with IsoBool.Object[Open] {
  override def positive = Open
  override def negative = Closed
}

case object Closed extends Open