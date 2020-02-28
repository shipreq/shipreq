package shipreq.webapp.client.project.app.pages.config_old.shared

import scalaz.Equal
import scalaz.syntax.equal._

sealed trait SaveNeed {
  def asOption[A](a: A): Option[A]
}

case object SaveNeeded extends SaveNeed {
  override def asOption[A](a: A) = Some(a)
}

case object SaveNotNeeded extends SaveNeed {
  override def asOption[A](a: A) = None
}

object SaveNeed {

  def cmpToExtract[A, B: Equal](f: A => B): (A, B) => SaveNeed = {
    val c = cmp[B]
    (a, b) => c(f(a), b)
  }

  def cmp[T: Equal]: (T, T) => SaveNeed =
    equal(_, _)

  def equal[T: Equal](a: T, b: T): SaveNeed =
    if (a ≟ b) SaveNotNeeded else SaveNeeded
}
