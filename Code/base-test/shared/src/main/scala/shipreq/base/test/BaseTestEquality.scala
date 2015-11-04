package shipreq.base.test

trait BaseTestEquality
  extends scalaz.std.AnyValInstances
     with scalaz.std.StringInstances
     with scalaz.std.TupleInstances
     with scalaz.std.OptionInstances
     with scalaz.std.ListInstances
     with scalaz.std.VectorInstances
     with scalaz.std.StreamInstances
{
  import nyaya.util._
  import scala.collection.immutable.ListSet
  import shipreq.base.util.UnivEq
  import scalaz._

  @inline protected def univEqForce[A]: UnivEq[A] =
    UnivEq.__instance.asInstanceOf[UnivEq[A]]

  @inline implicit def univEqUnit   : UnivEq[Unit]    = univEqForce
  @inline implicit def univEqString : UnivEq[String]  = univEqForce
  @inline implicit def univEqChar   : UnivEq[Char]    = univEqForce
  @inline implicit def univEqLong   : UnivEq[Long]    = univEqForce
  @inline implicit def univEqInt    : UnivEq[Int]     = univEqForce
  @inline implicit def univEqInteger: UnivEq[Integer] = univEqForce
  @inline implicit def univEqShort  : UnivEq[Short]   = univEqForce
  @inline implicit def univEqBoolean: UnivEq[Boolean] = univEqForce

  @inline implicit def univEqOption [A: UnivEq]           : UnivEq[Option[A]]       = univEqForce
  @inline implicit def univEqSet    [A: UnivEq]           : UnivEq[Set[A]]          = univEqForce
  @inline implicit def univEqList   [A: UnivEq]           : UnivEq[List[A]]         = univEqForce
  @inline implicit def univEqListSet[A: UnivEq]           : UnivEq[ListSet[A]]      = univEqForce
  @inline implicit def univEqVector [A: UnivEq]           : UnivEq[Vector[A]]       = univEqForce
  @inline implicit def univEqMap    [K: UnivEq, V: UnivEq]: UnivEq[Map[K, V]]       = univEqForce
  @inline implicit def univEqDisj   [A: UnivEq, B: UnivEq]: UnivEq[A \/ B]          = univEqForce
  @inline implicit def univEqThese  [A: UnivEq, B: UnivEq]: UnivEq[A \&/ B]         = univEqForce
  @inline implicit def univEqNel    [A: UnivEq]           : UnivEq[NonEmptyList[A]] = univEqForce

  @inline implicit def univEqMultimap[K, L[_], V](implicit ev: UnivEq[Map[K, L[V]]]): UnivEq[Multimap[K, L, V]] = univEqForce

  @inline implicit def univEqOneAnd[F[_], A](implicit fa: UnivEq[F[A]], a: UnivEq[A]): UnivEq[OneAnd[F, A]] = univEqForce

  @inline implicit def univEqTuple2[A:UnivEq, B:UnivEq]: UnivEq[(A,B)] = univEqForce
  @inline implicit def univEqTuple3[A:UnivEq, B:UnivEq, C:UnivEq]: UnivEq[(A,B,C)] = univEqForce
  @inline implicit def univEqTuple4[A:UnivEq, B:UnivEq, C:UnivEq, D:UnivEq]: UnivEq[(A,B,C,D)] = univEqForce
  @inline implicit def univEqTuple5[A:UnivEq, B:UnivEq, C:UnivEq, D:UnivEq, E:UnivEq]: UnivEq[(A,B,C,D,E)] = univEqForce
  @inline implicit def univEqTuple6[A:UnivEq, B:UnivEq, C:UnivEq, D:UnivEq, E:UnivEq, F:UnivEq]: UnivEq[(A,B,C,D,E,F)] = univEqForce
  @inline implicit def univEqTuple7[A:UnivEq, B:UnivEq, C:UnivEq, D:UnivEq, E:UnivEq, F:UnivEq, G:UnivEq]: UnivEq[(A,B,C,D,E,F,G)] = univEqForce
  @inline implicit def univEqTuple8[A:UnivEq, B:UnivEq, C:UnivEq, D:UnivEq, E:UnivEq, F:UnivEq, G:UnivEq, H:UnivEq]: UnivEq[(A,B,C,D,E,F,G,H)] = univEqForce
  @inline implicit def univEqTuple9[A:UnivEq, B:UnivEq, C:UnivEq, D:UnivEq, E:UnivEq, F:UnivEq, G:UnivEq, H:UnivEq, I:UnivEq]: UnivEq[(A,B,C,D,E,F,G,H,I)] = univEqForce

  @inline implicit def univEqJavaClass = univEqForce[Class[_]]
}