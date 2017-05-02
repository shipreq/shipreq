Heaps of my old FP code from my first year or two of FP, is shit.
I've written some of it. Let us reflect on the differences.

## `object Uniqueness`

Before:
```scala
object Uniqueness {

  def main[S, I, K, VS, V](key: S => K,
                           data: S => Stream[I],
                           ik: I => K,
                           vs: I => VS,
                           ignore: K => K => Boolean,
                           member: V => VS => Boolean): BF[S, V] = {
    val test = (s: S, v: InputCorrected[V]) => {
      val containsv = vs andThen member(v.value)
      val ignorable = ik andThen ignore(key(s))
      data(s).forall(i => !containsv(i) || ignorable(i))
    }
    new BF[S, V](ValidationPart.test(test, _))
  }

  /** Some(k₁) = Some(k₂), otherwise false */
  def ignoreO[K: Equal]: Option[K] => Option[K] => Boolean =
    _.fold[Option[K] => Boolean](_ => false)(k => _.fold(false)(k ≟ _))

  // ---------------------------------------------------------------------------

  def againstSetByKeyO[S, K: Equal, V](key: S => Option[K],
                                       data: S => Stream[(Option[K], Set[V])]): BF[S, V] =
    againstSetByKey[S, Option[K], V](key, data, ignoreO[K])

  def againstSetByKey[S, K, V](key: S => K,
                               data: S => Stream[(K, Set[V])],
                               ignore: K => K => Boolean): BF[S, V] =
    main[S, (K, Set[V]), K, Set[V], V](key, data, _._1, _._2, ignore, v => _ contains v)

  // ---------------------------------------------------------------------------

  @inline def entity[E] = new BE[E]

  final class BE[E] {
    @inline def k   [K: Equal](ek: E => K)         = new BE2[E, K        ](ek, k => k ≟ _)
    @inline def optk[K: Equal](ek: E => Option[K]) = new BE2[E, Option[K]](ek, ignoreO[K])
  }

  final class BE2[E, K](ek: E => K, ignore: K => K => Boolean) {
    @inline def v   [V: Equal](ev: E => V)         = member[V](v => v ≟ ev(_))
    @inline def optv[V: Equal](ev: E => Option[V]) = member[V](v => ev(_).fold(false)(v ≟ _))

    @inline def member[V](m: V => E => Boolean): BF[(Stream[E], K), V] =
      main[(Stream[E], K), E, K, E, V](_._2, _._1, ek, identity, ignore, m)
  }

  // ---------------------------------------------------------------------------

  def vfailure(fieldName: String): VFailure =
    VFailure.forField(fieldName, NonEmptyList("must be unique.")) // or maybe "is already in use"?

  final class BF[S, V](f: VFailure => ValidationPart[S, V, V]) {
    def fieldName(fieldName: String) = f(vfailure(fieldName))
  }
}
```

After:
```scala
object Uniqueness {

  /** Utilities that are useful in preparing data for uniqueness checking */
  object Util {
    def excludeKey[A, K: Equal](keyToExclude: K, data: TraversableOnce[A])(getKey: A => K): Iterator[A] =
      data.toIterator.filter(getKey(_) ≠ keyToExclude)

    def excludeOptionalKey[A, K: Equal](keyToExclude: Option[K], data: TraversableOnce[A])(getKey: A => K): Iterator[A] =
      keyToExclude match {
        case None    => data.toIterator
        case Some(k) => excludeKey(k, data)(getKey)
      }
  }

  // ===========================================================================

  def notUnique = Invalidity("Already in use.")

  def apply[A] = new Builder[A]

  final class Builder[A] private[Uniqueness]() {
    // final class Builder[A] private[Uniqueness](private val unit: Unit) extends AnyVal { -- https://issues.scala-lang.org/browse/SI-9646

    /** Ensure A is unique amongst a collection of Bs */
    def apply[B](data: () => TraversableOnce[B])
                (conflict: (B, A) => Boolean, ignore: B => Boolean): Invalidator[A] =
      Invalidator.test[A](
        v => data().forall(i => !conflict(i, v) || ignore(i)),
        notUnique)

    /** Convenience for common pattern of data being key/value tuples */
    def tuple[K, V](data: () => TraversableOnce[(K, V)])
                   (conflict: (V, A) => Boolean, ignore: K => Boolean): Invalidator[A] =
      apply(data)((x, a) => conflict(x._2, a), x => ignore(x._1))
  }

  /** Ensure A doesn't exist in a collection of As */
  def within[A: Equal](data: => TraversableOnce[A]): Invalidator[A] =
    Invalidator.test[A](a => data.forall(a ≠ _), notUnique)

  /** Ensure A doesn't exist in a set of As */
  def set[A: UnivEq](data: => Set[A]): Invalidator[A] =
    Invalidator.test[A](!data.contains(_), notUnique)

  // ===========================================================================

  def keyWithValue[K: Equal, V: Equal](data: () => TraversableOnce[(K, V)])(key: K): Invalidator[V] =
    apply[V].tuple(data)(Equal[V].equal, _ ≟ key)

  def optionalKeyWithValue[K: Equal, V: Equal](data: () => TraversableOnce[(Option[K], V)])(key: Option[K]): Invalidator[V] =
    keyWithValue(data)(key)(OptionalConflict.equalOption, implicitly)

  def keyWithValueSet[K: Equal, V: UnivEq](data: () => TraversableOnce[(K, Set[V])])(key: K): Invalidator[V] =
    apply[V].tuple(data)(_ contains _, _ ≟ key)

  def optionalKeyWithValueSet[K: Equal, V: UnivEq](data: () => TraversableOnce[(Option[K], Set[V])])(key: Option[K]): Invalidator[V] =
    keyWithValueSet(data)(key)(OptionalConflict.equalOption, implicitly)
}
```

Before:
```scala
def main[S, I, K, VS, V](key: S => K,
                         data: S => Stream[I],
                         ik: I => K,
                         vs: I => VS,
                         ignore: K => K => Boolean,
                         member: V => VS => Boolean): BF[S, V] = {
  val test = (s: S, v: InputCorrected[V]) => {
    val containsv = vs andThen member(v.value)
    val ignorable = ik andThen ignore(key(s))
    data(s).forall(i => !containsv(i) || ignorable(i))
  }
  new BF[S, V](ValidationPart.test(test, _))
}

def vfailure(fieldName: String): VFailure =
  VFailure.forField(fieldName, NonEmptyList("must be unique.")) // or maybe "is already in use"?

final class BF[S, V](f: VFailure => ValidationPart[S, V, V]) {
  def fieldName(fieldName: String) = f(vfailure(fieldName))
}
```

After:
```scala
def notUnique = Invalidity("Already in use.")

def apply[A] = new Builder[A]
final class Builder[A] private[Uniqueness]() {

  /** Ensure A is unique amongst a collection of Bs */
  def apply[B](data: () => TraversableOnce[B])
              (conflict: (B, A) => Boolean, ignore: B => Boolean): Invalidator[A] =
    Invalidator.test[A](
      v => data().forall(i => !conflict(i, v) || ignore(i)),
      notUnique)
}
```

Observations

* Super confusing.
  * 5 abstract types
  * 6 little functions
  * functions are all in peculair, very specific shapes. Implies implementation is abstact but not algorithm.
  * return type is called `BF`. WTF?
  * meaning of types and functions unclear. eg. "ik: I => K" is obvious by type but it's meaning is unclear. "key: S => K" too. S-key and I-key? What?
  * `vs/member` and `ik/ignore` are both only used once and composed immediately before use. In other words those 4 params could easily be consolitdated into 2. Why didn't I just do that when I wrote it?
    I was optimising for convenience of use and brevity. I should've been optimising for simplicity, readability and reuse.



```scala
def vp[I: Equal](f: HashRefKeyVS => HashRefKeyVS.Data[I]) =
  Uniqueness.main[HashRefKeyVS, (Option[I], HashRefKey), Option[I], HashRefKey, HashRefKey](
    f(_)._1, f(_)._2, _._1, _._2, Uniqueness.ignoreO[I], a => a equalsIgnoreCase _.value
  ).fieldName(FieldNames.hashRefKey)
```

```scala
private implicit val equality = Equal.equal[HashRefKey](_.value equalsIgnoreCase _.value)

final case class SubState[Id: Equal](subject: Option[Id], data: () => TraversableOnce[(Option[Id], HashRefKey)]) {
  def invalidator: Invalidator[HashRefKey] =
    Uniqueness.optionalKeyWithValue(data)(subject)
}
```
