package shipreq.base.util

sealed abstract class Permission extends IsoBool[Permission] {
  override final def companion = Permission

  final def option[A](a: => A): Option[A] =
    this match {
      case Allow => Some(a)
      case Deny  => None
    }
}

object Permission extends IsoBool.Object[Permission] {
  override def positive = Allow
  override def negative = Deny
}

case object Allow extends Permission

case object Deny extends Permission
