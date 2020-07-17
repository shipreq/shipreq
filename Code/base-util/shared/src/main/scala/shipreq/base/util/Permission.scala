package shipreq.base.util

sealed abstract class Permission extends IsoBool.WithBoolOps[Permission] {
  override final def companion = Permission

  def apply[A](a: => A): Permission.DeniedOr[A]
  def option[A](a: => A): Option[A]
}

case object Allow extends Permission {
  override def apply[A](a: => A) = \/-(a)
  override def option[A](a: => A) = Some(a)
}

case object Deny extends Permission {
  override def apply[A](a: => A) = Permission.denied
  override def option[A](a: => A) = None
}

object Permission extends IsoBool.Object[Permission] {
  override def positive = Allow
  override def negative = Deny

  type DeniedOr[+A] = Deny.type \/ A
  val denied: Permission.DeniedOr[Nothing] = -\/(Deny)
}
