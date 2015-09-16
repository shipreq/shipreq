package shipreq.webapp.base.util

/**
 * There exist many cases in which [[shipreq.webapp.base.data.DataProp]] has verified the integrity of a
 * [[shipreq.webapp.base.data.Project]] yet we have no compiler proof that map lookups will succeed.
 *
 * This uniformly deals with those cases.
 */
object Must {

  @inline implicit class MustExtForOption[A](private val o: Option[A]) extends AnyVal {
    @inline def mustExistElse(err: => String): A =
      o getOrElse mustNotHappen(err)
  }

  def mustNotHappen(err: String): Nothing =
    throw new InvariantViolated(err)

  final class InvariantViolated(msg: String) extends Exception(msg)
}
