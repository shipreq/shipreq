package shipreq.base.util

object Identity {

  private[this] val instance: Any => Any =
    i => i

  def apply[A]: A => A =
    instance.asInstanceOf[A => A]

}
