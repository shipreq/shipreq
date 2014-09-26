package shipreq.webapp.shared.validation

trait Constraint[-A] {

  def invalidate(a: A): List[String]

  def isValid(a: A): Boolean =
    invalidate(a).isEmpty

  final def +[B <: A](b: Constraint[B]) = Constraint[B](
    i => invalidate(i) ::: b.invalidate(i))

  /** Runs b only if this passes */
  final def >>[B <: A](b: Constraint[B]) = Constraint[B](
    i => invalidate(i) match {
      case Nil => b.invalidate(i)
      case aes => aes
    })

  final def not(errMsg: String) =
    Constraint.predicate[A](!isValid(_))(errMsg)
}

object Constraint {

  def apply[A](f: A => List[String]): Constraint[A] =
    new Constraint[A] {
      override def invalidate(a: A) = f(a)
    }

  def perf[A](b: A => Boolean, f: A => List[String]): Constraint[A] =
    new Constraint[A] {
      override def invalidate(a: A) = f(a)
      override def isValid(a: A) = b(a)
    }

  def const(es: List[String]) = apply[Any](_ => es)

  def zero = const(Nil)

  final class PendingErrMsg[A](f: String => Constraint[A]) {
    def apply(errMsg: String): Constraint[A] = f(errMsg)
  }

  def predicate[A](validityTest: A => Boolean): PendingErrMsg[A] =
    new PendingErrMsg(errMsg => {
      val err = errMsg :: Nil
      Constraint[A](a => if (validityTest(a)) Nil else err)
    })

  def not[A](p: PendingErrMsg[A])(errMsg: String): Constraint[A] = p("").not(errMsg)
}