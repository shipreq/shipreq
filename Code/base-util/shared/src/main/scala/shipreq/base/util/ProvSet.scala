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
    val vs = values.iterator.map { case (k, v) => if (k.toString == v.toString) k.toString else s"$k=$v"}.toArray.sortInPlace().mkString("{", ",", "}")
    if (provenance.isEmpty)
      vs
    else {
      val ps = provenance.iterator.map(_.toString).toArray.sortInPlace().mkString("{", ",", "}")
      s"$vs:$ps"
    }
  }

  @elidable(elidable.ASSERTION)
  def assertProps_(msg: => String = ""): Unit =
    new Props(module)
      .provSet(this)
      .rename(_ => scalaz.Value("ProvSet.assertProps()" + Option(msg).filter(_.nonEmpty).fold("")(" - " + _)))
      .assertSuccess()

  @inline
  def assertProps(msg: => String = ""): this.type = {
    assertProps_(msg)
    this
  }

  def allKeys: Set[K] =
    values.keySet ++ provenance.iterator.flatMap(e => e.from :: e.to :: Nil)

  val partialOrder: PartialOrder[K] =
    module.partialOrderK orElse partialOrderP

  private def partialOrderP: PartialOrder[K] =
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

      val provGraph = Digraph.BiDir {
        provenance.foldLeft(Digraph.emptyUniDir[K])((g, p) => g.add(p.from, p.to))
      }

      val components = provGraph.stronglyConnectedComponents

      def lesserPathExists(from: K, to: K): Boolean = {

        def go(from: K, seen: Set[K]): Boolean = {
          if (from <= to)
            true
          else if (seen.contains(from))
            false
          else {
            var found = false
            val seen2 = seen + from

            val it = components.iterator
            while (it.nonEmpty && !found) {
              val c = it.next()
              // if from <= p.from < p.to, check if p.to <= to
              // I'm subsumed by something greater than me, does *it* have a lesser path to our destination?
              if (c.exists(from <= _)) {
                for (p <- provenance) {
                  if (!found && c.contains(p.from)) {
                    if (go(p.to, seen2))
                      found = true
                  }
                }
              }
            }

            found
          }
        }

        go(from, Set.empty)
      }

      val byTotal = if (module.isAscending(
        Entry(x, null.asInstanceOf[V]),
        Entry(y, null.asInstanceOf[V])
      )) Lesser else Greater

      val `x<y` = lesserPathExists(from = x, to = y)
      val `y<x` = lesserPathExists(from = y, to = x)

      if (`x<y`) {
        if (`y<x`)
          byTotal
        else
          Lesser
      } else if (`y<x`)
        Greater
      else
        Separate

    }.memo

  def ++(s: Self): Self = {
    if (this.isEmpty)
      s
    else if (s.isEmpty)
      this
    else
      new ProvSet(
        this.values ++ s.values,
        this.provenance ++ s.provenance
      )(module)
        .pruneValues
  }

  private[util] def pruneValues: Self = {
    implicit val po = partialOrder

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
  }

  // ===================================================================================================================

  final class Props[K, V](val module: Module[K, V]) {
    import nyaya.prop._
    import module.{ProvSet => S, partialOrderK}
    import ScalazExtra._

    private val provEntry: Prop[ProvEntry[K]] =
      Prop.test("unrelated", p => p.from isSeparateTo p.to)

    private val provenance: Prop[Set[ProvEntry[K]]] =
      provEntry.forallF[Set]

    private val separateValues: Prop[Map[K, V]] =
      Prop.forall[Map[K, V], Set, K](_.keySet) { m =>
        Prop.atom[K]("value has no comparable siblings", k => {
          val bad = (m - k).find(_._1 isComparableTo k)
          bad.map(x => s"Comparable siblings found: $k & ${x._1}")
        })
      }

    private val partialOrder: Prop[S] =
      Prop.evaln[S]("partialOrder", s => PartialOrder.Props.eval(s.allKeys)(s.partialOrder).liftL)

    val provSet: Prop[S] =
      (
        separateValues.contramap[S](_.values) &
        provenance.contramap[S](_.provenance) &
        partialOrder
      ).rename("ProvSet props")
  }

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

    private val validity: Laws = {
      val props = new Props(module)
      prop2("validity", props.provSet)(_ ++ _)
    }

    private val valueRetention: Laws = {
      val prop = Prop.atom[(ProvSet, ProvSet)]("value retention", { case (x, y) =>
        val s = x ++ y
        val values = x.values.keySet ++ y.values.keySet
        if (values.isEmpty)
          Option.unless(s.values.isEmpty)("Where did these values come from?! " + s.values)
        else
          Option.unless(s.values.nonEmpty)("Values lost!").orElse {
            val badKeys = s.values.keySet -- values
            Option.unless(badKeys.isEmpty)("Mystery values found: " + badKeys)
          }
      })
      prop2("value retention", prop)((_, _))
    }

    val laws: Laws =
      List(
        idempotency,
        associativity,
        commutativity,
        validity,
        valueRetention,
      ).reduce(_ & _).rename("ProvSet (++) laws")
  }
}
