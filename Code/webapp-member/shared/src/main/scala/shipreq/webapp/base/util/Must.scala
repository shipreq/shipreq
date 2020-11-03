package shipreq.webapp.base.util

import shipreq.base.util.ErrorMsg

/**
 * There exist many cases in which [[shipreq.webapp.base.data.DataProp]] has verified the integrity of a
 * [[shipreq.webapp.base.data.Project]] yet we have no compiler proof that map lookups will succeed.
 *
 * This uniformly deals with those cases.
 */
object Must {

  @inline implicit class MustExtForOption[A](private val self: Option[A]) extends AnyVal {
    @inline def mustExistElse(err: => ErrorMsg): A =
      self getOrElse mustNotHappen(err)
  }

  @inline implicit class MustExtForDisj[L, A](private val self: L \/ A) extends AnyVal {
    @inline def mustExistElse(err: => ErrorMsg): A =
      self getOrElse mustNotHappen(err)
  }

  @inline def mustNotHappen(err: ErrorMsg): Nothing =
    err.throwException()
}
