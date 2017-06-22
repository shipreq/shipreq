package shipreq.base.util

/**
  * Allocation-free Option.
  */
final case class FreeOption[+A >: Null](getOrNull: A) extends AnyVal {

  @inline def isEmpty: Boolean = null == getOrNull
  @inline def nonEmpty: Boolean = null != getOrNull

  def getOrElse[B >: A](fallback: => B): B =
    if (isEmpty) fallback else getOrNull

  def orElse[B >: A](fallback: => FreeOption[B]): FreeOption[B] =
    if (isEmpty) fallback else this

  def map[B >: Null](f: A => B): FreeOption[B] =
    FreeOption(if (isEmpty) null else f(getOrNull))

  def flatMap[B >: Null](f: A => FreeOption[B]): FreeOption[B] =
    if (isEmpty) FreeOption.empty else f(getOrNull)

  def fold[B](fallback: => B, f: A => B): B =
    if (isEmpty) fallback else f(getOrNull)

  def exists(f: A => Boolean): Boolean =
    nonEmpty && f(getOrNull)

  def forall(f: A => Boolean): Boolean =
    isEmpty || f(getOrNull)

  def filter(f: A => Boolean): FreeOption[A] =
    if (exists(f)) this else FreeOption.empty

  def foreach(f: A => Unit): Unit =
    if (nonEmpty) f(getOrNull)

  def toOption: Option[A] =
    Option(getOrNull)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object FreeOption {
  @inline def empty[A >: Null]: FreeOption[A] =
    apply(null)

  def fromOption[A >: Null](o: Option[A]): FreeOption[A] =
    apply(o.orNull)

  def when[A >: Null](cond: Boolean, a: => A): FreeOption[A] =
    apply(if (cond) a else null)

  @inline def unless[A >: Null](cond: Boolean, a: => A): FreeOption[A] =
    when(!cond, a)

  object Implicits {
    @inline implicit class FreeOption_OptionExt[A >: Null](private val self: Option[A]) extends AnyVal {
      @inline def free: FreeOption[A] =
        FreeOption.fromOption(self)
    }

  }
}