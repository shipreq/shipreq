package shipreq.base.util

import ProvSet._
import PartialOrder.ImplicitOps._
import shipreq.base.util.PartialOrder.Cmp._

/** Provenance Set.
 *
 * This is a state-based CRDT/CvRDT (conflict-free, convergent, replicated data type).
 *
 * Laws:
 *   - idempotency
 *   - associativity
 *   - commutativity
 *
 * This is modelled in `../TLA+/provset.tla`.
 */
final case class ProvSet[K, V](values: Map[K, V], provenance: Set[ProvEntry[K]])(implicit module: Module[K, V]) {
  import module.univEqK

  type Self = ProvSet[K, V]

  def isEmpty =
    values.isEmpty & provenance.isEmpty

  @inline def nonEmpty =
    !isEmpty

  @elidable(elidable.FINEST)
  override def toString = {
    val vs = values.iterator.map { case (k, v) => if (k.toString == v.toString) k.toString else s"$k=$v"}.toArray.sortInPlace().mkString("{", ", ", "}")
    if (provenance.isEmpty)
      s"ProvSet($vs)"
    else {
      val ps = provenance.iterator.map(_.toString).toArray.sortInPlace().mkString("{", ", ", "}")
      s"ProvSet($vs, $ps)"
    }
  }

  val partialOrderP: PartialOrder[K] =
    PartialOrder[K] { (x, y) =>
      import module.partialOrderK

//   *   |A| |B| |C|
//   *   |=| |=| |=|
//   *   |3| | | | |
//   *   | |↖| | | |
//   *   |2| |2| | |
//   *   | | | |↖| |
//   *   |1| |1| |1|
//   *
//   *   ProvEntry(from = B2, to = A3)
//   *   ProvEntry(from = C1, to = B2)
//   *
//   *   Anything <= B2 is now < A3.
//   *   Anything <= C1 is now < B2.
//   *
//   *   Anything <= C1 is now < A3.
//   *
//   *   B1 < A3
//   *   B2 < A3
//   *   C1 < B2
//   *   C1 < A3


      // x = C0
      // y = A4
      // if we can find a path from x (C0) to y (A4), then x < y

//      println()

      def pathExists(from: K, to: K): Boolean = {
//        println(s"pathExists($provenance : $from -> $to)")

//        try {

          if (from <= to)
            true
          else {
            val it = provenance.iterator
            var found = false
            while (it.nonEmpty && !found) {
              val p = it.next()
              if (from <= p.from && !(p.to <= from) && pathExists(p.to, to))
                found = true
            }
            found
          }

//        } catch {
//          case t: Throwable =>
//            val msg =
//              s"""FAILURE: ${t.getMessage}
//                 |  set : $this
//                 |  from: $from
//                 |  to  : $to
//                 |
//                 |""".stripMargin
//            throw new RuntimeException(msg)
//        }

      }

      if (pathExists(from = x, to = y)) {
        assert(!pathExists(from = y, to = x), "Provenance isn't a lawful partial order! " + provenance)
        Lesser
      } else if (pathExists(from = y, to = x))
        Greater
      else
        Separate
    }

  val partialOrder: PartialOrder[K] =
    module.partialOrderK orElse partialOrderP

  def allKeys: Set[K] =
    values.keySet ++ provenance.iterator.flatMap(e => e.from :: e.to :: Nil)

  @elidable(elidable.ASSERTION)
  def assertProps(): Unit = {
    PartialOrder.Props.assert(allKeys)(partialOrder)
  }

  /*
  @elidable(elidable.ASSERTION)
  def assertProps_(msg: => String = ""): Unit =
    (new Props(module))
      .provSet(this)
      .rename(_ => scalaz.Value("ProvSet.assertProps()" + Option(msg).filter(_.nonEmpty).fold("")(" - " + _)))
      .assertSuccess()

  @inline
  def assertProps(msg: => String = ""): this.type = {
    assertProps_(msg)
    this
  }
*/

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████
  def ++(s: Self): Self = {
    val debug = !true

    if (this.isEmpty) return s
    if (s.isEmpty) return this

    try {

      new ProvSet(
        this.values ++ s.values,
        this.provenance ++ s.provenance
      )(module)
        .pruneValues

//      val x = new ProvSet(Map.empty, this.provenance ++ s.provenance)(module)
//      implicit val po = x.partialOrder
//
//      PartialOrder.Props.assert(this.allKeys ++ s.allKeys)(partialOrder)
//
//      val allKeys = this.values.keySet ++ s.values.keySet
//      var keys = allKeys
//      for (k <- allKeys) {
//        if (keys.exists(k < _))
//          keys -= k
//      }
//      var values2 = Map.empty[K, V]
//      for (k <- keys)
//        values2 = values2.updated(k, this.values.getOrElse(k, s.values(k)))
//
//      if (debug)
//        println(
//          s"""===========================================================================
//             |allKeys = ${allKeys.toList.map(_.toString).sorted.mkString(", ")}
//             |delKeys = ${(allKeys -- keys).toList.map(_.toString).sorted.mkString(", ")}
//             |newKeys = ${keys.toList.map(_.toString).sorted.mkString(", ")}
//             |
//             |""".stripMargin)
//
//      new ProvSet(values2, x.provenance)(module)

    } catch {
      case t: Throwable =>
        val msg =
          s"""FAILURE: ${t.getMessage}
             |  lhs: $this
             |  rhs: $s
             |
             |""".stripMargin
        throw new RuntimeException(msg)
    }
  }

  def pruneValues: Self = {
    implicit val po = partialOrder

    PartialOrder.Props.assert(this.allKeys)(partialOrder)

    var values2 = values
    for (k <- values.keysIterator) {
      if (values2.exists(k < _._1))
        values2 -= k
    }

    new ProvSet(values2, provenance)(module)
  }
}

object ProvSet {

  /**
   * Imagine that there's a conflict between A2 and B2, and it's resolved into A3.
   *
   * {{{
   *   |A| |B|
   *   |=| |=|
   *   |3| | |
   *   | |↖| |
   *   |2| |2|
   *   | | | |
   *   |1| |1|
   *
   *   ProvEntry(from = B2, to = A3)
   *
   *   Anything <= B2 is now < A3.
   *
   *   B1 < A3
   *   B2 < A3
   * }}}
   */
  final case class ProvEntry[K](from: K, to: K) {
    @elidable(elidable.FINEST)
    override def toString = s"$from<$to"
  }

  final case class Entry[+K, +V](key: K, value: V)

  final case class MergePair[+A](subject: A, into: A, subjectGreater: Boolean) {
    def greater: A = if (subjectGreater) subject else into
    def lesser : A = if (subjectGreater) into else subject
  }

  implicit def univEqP[K: UnivEq           ]: UnivEq[ProvEntry [K]] = UnivEq.derive
  implicit def univEq [K: UnivEq, V: UnivEq]: UnivEq[ProvSet[K, V]] = UnivEq.derive

  object Module {
    def apply[K: PartialOrder : UnivEq, V: UnivEq](mergeValues: MergePair[V] => V,
                                                   isAscending: (Entry[K, V], Entry[K, V]) => Boolean): Module[K, V] =
      new Module[K, V](mergeValues, isAscending)
  }

  final class Module[K, V](val mergeValues: MergePair[V] => V,
                           val isAscending: (Entry[K, V], Entry[K, V]) => Boolean)
                          (implicit
                           val partialOrderK: PartialOrder[K],
                           val univEqK: UnivEq[K],
                           val univEqV: UnivEq[V],
                          ) {

    type ProvSet = shipreq.base.util.ProvSet[K, V]

    implicit def univEq: UnivEq[ProvSet] =
      ProvSet.univEq

    def apply(values: Map[K, V], provenance: Set[ProvEntry[K]]): ProvSet =
      new ProvSet(values, provenance)(this)

    val empty: ProvSet =
      ProvSet[K, V](Map.empty, Set.empty)(this)

    def one(k: K, v: V): ProvSet =
      ProvSet[K, V](Map.empty.updated(k, v), Set.empty)(this)
/*
//    implicit val entryOrdering: Ordering[Entry[K, V]] =
//      new Ordering[Entry[K, V]] {
//        override def compare(x: Entry[K, V], y: Entry[K, V]): Int = {
//          partialOrderK(x.key, y.key) match {
//            case Lesser   => -1
//            case Equal    => 0
//            case Greater  => 1
//            case Separate => if (isAscending(x, y)) -1 else 1
//          }
//        }
//      }

//    implicit val entryOrdering: Ordering[Entry[K, V]] =
    def entryOrdering(prov: Set[ProvEntry[K]]): Ordering[Entry[K, V]] = {
      new Ordering[Entry[K, V]] {
        override def compare(x: Entry[K, V], y: Entry[K, V]): Int = {

          partialOrderK(x.key, y.key) match {
            case Lesser   => -1
            case Equal    => 0
            case Greater  => 1
            case Separate =>

              val `x<=y` = prov.exists(x.key <= _.height)
              val `y<=x` = prov.exists(y.key <= _.height)
              (`x<=y`, `y<=x`) match {
                case (true , false) => -1
                case (false, true ) => 1
                case (false, false)
                   | (true , true ) => if (isAscending(x, y)) -1 else 1
              }

//              if (isAscending(x, y)) -1 else 1
          }
        }
      }
    */
  }

  // ===================================================================================================================

//  final class Props[K, V](val module: Module[K, V]) {
//    import nyaya.prop._
////    import module.{Entry => E, ProvSet => S, partialOrderK}
//    import ScalazExtra._
//
//    def prov: Prop[Set[ProvEntry[K]]] = {
//      ???
////      Prop.atom[Prov]("Provenance", ps => {
////        var failure = Option.empty[String]
////        for {
////          p <- ps
////          q <- ps - p
////        } if (p isComparableTo q)
////            failure = Some("Comparable keys in same provenance.")
////        failure
////      })
//    }
//
//    def entry: Prop[E] = {
//      def keyAndProv =
//        Prop.forall[E, Set, ProvEntry[K]](_.provenance)(e =>
//          Prop.atom("Entry key & provenance", p => Option.when(p.height <= e.key)(
//            s"Entry ${e.key} has in its provenance a key $p, that is <= itself.")))
//
//      (keyAndProv & prov.contramap((_: E).provenance)).rename("entry")
//    }
//
//    def provSet: Prop[S] =
//      Prop.forall[S, Set, E](_.repr) { s =>
//
//        def compareEntries(name: => String)(c: (E, E) => Boolean): Prop[E] =
//          Prop.test[E](name, e => s.repr.forall(f => (e eq f) || c(e, f)))
//
//        def coexistenceK: Prop[E] =
//          compareEntries("Comparable sibling entries.")((e, f) =>
//            e.key.isSeparateTo(f.key))
//
//        def coexistenceP: Prop[E] =
//          compareEntries("Sibling provenance.")((e, f) =>
//            f.provenance.forall(p => !(e.key <= p.height)))
//
//        (entry & coexistenceK & coexistenceP).rename("provSet")
//      }
//  }

  // ===================================================================================================================

  object Laws {
    final case class Input[K, V](a: ProvSet[K, V],
                                 b: ProvSet[K, V],
                                 c: ProvSet[K, V]) {
      import japgolly.microlibs.stdlib_ext.StdlibExt._

      override def toString =
        s"""ProvSet.Laws.Input(
           |  a = ${a.toString.indent(6).trim},
           |  b = ${b.toString.indent(6).trim},
           |  c = ${c.toString.indent(6).trim})
           |""".stripMargin.trim
    }
  }

  final class Laws[K, V](module: Module[K, V]) {
    import nyaya.prop._
    import scalaz.Equal

    private implicit val equality = scalazEqualFromUnivEq(module.univEq)

    type ProvSet = shipreq.base.util.ProvSet[K, V]
//    type Entry   = shipreq.base.util.ProvSet.Entry[K, V]
    type Input   = Laws.Input[K, V]
    type Laws    = Prop[Input]

    private def prop2[A](name: String, p: Prop[A])(g: (ProvSet, ProvSet) => A): Laws = {
      def f(desc: String, f1: Input => ProvSet, f2: Input => ProvSet): Laws =
        Prop.evaln(s"$name ($desc)", i => {
          val a = g(f1(i), f2(i))
          p(a).liftL
        })
      f("a,b", _.a, _.b) & f("a,c", _.a, _.c) & f("b,c", _.b, _.c)
    }

    private def equal2[B: Equal](name: String,
                                 e: (ProvSet, ProvSet) => B,
                                 a: (ProvSet, ProvSet) => B): Laws = {
      def f(desc: String, f1: Input => ProvSet, f2: Input => ProvSet): Laws = {
        def mk(g: (ProvSet, ProvSet) => B): Input => B = i => g(f1(i), f2(i))
        Prop.equal[Input, B](s"$name ($desc)", mk(a), expect = mk(e))
      }
      f("a,b", _.a, _.b) & f("a,c", _.a, _.c) & f("b,c", _.b, _.c)
    }

    private val idempotency: Laws =
      equal2("idempotency",
        (x, y) => (x ++ y) ++ y,
        (x, y) => x ++ y)

    private val associativity: Laws =
      Prop.equal[Input, ProvSet]("associativity",
        i => (i.a ++ i.b) ++ i.c,
        i => i.a ++ (i.b ++ i.c))

    private val commutativity: Laws =
      equal2("commutativity",
        (x, y) => x ++ y,
        (x, y) => y ++ x)

//    private val validity: Laws = {
//      val props = new Props(module)
//      prop2("validity", props.provSet)(_ ++ _)
//    }

    val laws: Laws =
      List(
        idempotency,
        associativity,
        commutativity,
//        validity,
      ).reduce(_ & _)
  }
}
