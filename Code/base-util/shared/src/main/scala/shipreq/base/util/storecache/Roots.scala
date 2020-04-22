package shipreq.base.util.storecache

final class StoreCache1[I, S, A](mapInput: I => S,
                                 source  : S,
                                 lazyVal : LazyVal[A])
                                (implicit val qe: QuickEq[S]) extends StoreCache[I, A] {
  override def value: A =
    lazyVal.value

  override def contramap[J](f: J => I): StoreCache1[J, S, A] =
    new StoreCache1(mapInput compose f, source, lazyVal)

  override def map[B](f: A => B): StoreCache1[I, S, B] =
    new StoreCache1(mapInput, source, lazyVal map f)

  private[storecache] def next(nextInput: I, run: S => A): StoreCache1[I, S, A] = {
    val nextSource = mapInput(nextInput)
    if (qe.areEq(source, nextSource))
      this
    else
      new StoreCache1(mapInput, nextSource, LazyVal(run(nextSource)))
  }
}

final class Logic1[I, S: QuickEq, A](mapInput: I => S, run: S => A) extends StoreCache.Logic[I, A] {
  override type Cache = StoreCache1[I, S, A]

  override def contramap[J](f: J => I): Logic1[J, S, A] =
    new Logic1(mapInput compose f, run)

  override def map[B](f: A => B): Logic1[I, S, B] =
    new Logic1(mapInput, f compose run)

  override def init(init: I): Cache = {
    val s = mapInput(init)
    new StoreCache1(mapInput, s, LazyVal(run(s)))
  }

  override def next(prev: Cache, i: I): Cache =
    prev.next(i, run)
}

