package shipreq.base.util

sealed trait Direction extends IsoBool[Direction] {
  override final def companion = Direction
}

object Direction extends IsoBool.Object[Direction] {
  override def positive = Forwards
  override def negative = Backwards
}

case object Forwards extends Direction
case object Backwards extends Direction
