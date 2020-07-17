package shipreq.base.util

/**
 * Record data as an effect for later retrieval. Mutable.
 *
 * @tparam A Datum in.
 * @tparam B Logged data out.
 */
sealed abstract class DataLog[-A, B] {
  val add: A => Unit

  def addFn[I](f: (A => Unit) => I => Unit): I => Unit

  def get(): B

  def disableIf(b: Boolean): DataLog[A, B]

  final def disableUnless(b: Boolean) = disableIf(!b)
}

// ===================================================================================================================
object DataLog {

  final class Log[-A, B](empty: B, f: (B, A) => B) extends DataLog[A, B] {
    private var seen = empty

    override val add: A => Unit =
      a => seen = f(seen, a)

    override def addFn[I](f: (A => Unit) => I => Unit): I => Unit =
      f(add)

    override def get(): B =
      seen

    override def disableIf(b: Boolean): DataLog[A, B] =
      if (b) new Ignore(empty) else this
  }

  // ===================================================================================================================

  final class Ignore[B](empty: B) extends DataLog[Any, B] {
    override val add: Any => Unit =
      _ => ()

    override def addFn[I](f: (Any => Unit) => I => Unit): I => Unit =
      add

    override def get(): B =
      empty

    override def disableIf(b: Boolean) =
      this
  }

  // ===================================================================================================================

  def apply[A, B](empty: B, f: (B, A) => B): DataLog[A, B] =
    new Log(empty, f)

  def set[A: UnivEq]: DataLog[A, Set[A]] =
    apply[A, Set[A]](Set.empty, _ + _)

  def list[A]: DataLog[A, List[A]] =
    apply[A, List[A]](Nil, _.::(_))

  def vector[A]: DataLog[A, Vector[A]] =
    apply[A, Vector[A]](Vector.empty, _ :+ _)

  def mtrie[N]: DataLog[NonEmptyVector[N], MTrie.Trie[N, Unit]] =
    apply[NonEmptyVector[N], MTrie.Trie[N, Unit]](Map.empty, _ add _)
}
