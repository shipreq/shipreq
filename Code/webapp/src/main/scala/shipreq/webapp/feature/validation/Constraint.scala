package shipreq.webapp.feature.validation

trait Constraint[-A] {

  /** JavaSript expression. Should be encloses in parenthesis if applicable. */
  val js: String

  def invalidate(a: A): List[String]

  def isValid(a: A): Boolean =
    invalidate(a).isEmpty

  final def &[B <: A](b: Constraint[B]) =
    Constraint[B](i => invalidate(i) ::: b.invalidate(i), s"($js && ${b.js})")

  final def |[B <: A](b: Constraint[B]) =
    Constraint[B](i => if (isValid(i)) Nil else b.invalidate(i), s"($js || ${b.js})")

  /** A implies B. If A is true, check B. If A is false, don't bother with B. */
  final def -->[B <: A](b: Constraint[B]) =
    Constraint[B](i => invalidate(i) match {
      case Nil => b.invalidate(i)
      case aes => aes
    }, s"($js && ${b.js})")

  final def not(errMsg: String) =
    Constraint.predicate[A](!isValid(_), s"!$js")(errMsg)
}

object Constraint {

  def apply[A](f: A => List[String], _js: String): Constraint[A] =
    new Constraint[A] {
      override val js = _js
      override def invalidate(a: A) = f(a)
    }
  def perf[A](b: A => Boolean, f: A => List[String], _js: String): Constraint[A] =
    new Constraint[A] {
      override val js = _js
      override def invalidate(a: A) = f(a)
      override def isValid(a: A) = b(a)
    }

  def const(es: List[String]) = apply[Any](_ => es, if (es.isEmpty) "!0" else "!1")

  def zero = const(Nil)

  final class PendingErrMsg[A](f: String => Constraint[A]) {
    def apply(errMsg: String): Constraint[A] = f(errMsg)
  }

  def predicate[A](validityTest: A => Boolean, js: String): PendingErrMsg[A] =
    new PendingErrMsg(errMsg => {
      val err = errMsg :: Nil
      Constraint[A](a => if (validityTest(a)) Nil else err, js)
    })

  def not[A](p: PendingErrMsg[A])(errMsg: String): Constraint[A] = p("").not(errMsg)
}