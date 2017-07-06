package shipreq.base.util

/**
  * A partial function that, given a fallback, can efficiently become a total function.
  */
final case class FnWithFallback[A, B](withFallback: (A => B) => A => B) extends AnyVal {

  def withFallbackByValue(b: B): A => B =
    withFallback(_ => b)

  def withFallbackByNeed(b: B): A => B = {
    lazy val bb = b
    withFallback(_ => bb)
  }

  def withFallbackByName(b: => B): A => B =
    withFallback(_ => b)

  def when(f: A => Boolean): A ?=> B =
    FnWithFallback(fallback => {
      val attempt = withFallback(fallback)
      a => (if (f(a)) attempt else fallback)(a)
    })

  /**
    * Attempt this partial function and if it doesn't produce a `B`, use the `next` argument.
    * Like boolean OR, or `Option#orElse`.
    */
  def |(next: FnWithFallback[A, B]): A ?=> B =
    FnWithFallback(f => withFallback(next.withFallback(f)))

  def partial(implicit ev: Null <:< B): A => Option[B] = {
    val n = ev(null)
    withFallback(_ => n).andThen(Option(_))
  }
}

object FnWithFallback {
  def when[A, B](cond: A => Boolean)(ok: A => B): A ?=> B =
    apply(f => a => if (cond(a)) ok(a) else f(a))

  def whenByValue[A, B](cond: A => Boolean)(ok: B): A ?=> B =
    apply(f => a => if (cond(a)) ok else f(a))

  def whenByNeed[A, B](cond: A => Boolean)(ok: => B): A ?=> B = {
    lazy val b = ok
    apply(f => a => if (cond(a)) b else f(a))
  }

  def whenByName[A, B](cond: A => Boolean)(ok: => B): A ?=> B =
    apply(f => a => if (cond(a)) ok else f(a))

  def extract[A, B, E](cond: A => Option[E])(ok: A => E => B): A ?=> B =
    apply(f => a => cond(a).fold(f(a))(ok(a)))
}
