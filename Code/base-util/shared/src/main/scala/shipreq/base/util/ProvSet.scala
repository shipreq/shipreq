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
final case class ProvSet[K, V](repr: Repr[K, V])(implicit module: Module[K, V]) {
  import module.{one, partialOrderE, partialOrderK}

  type Self = ProvSet[K, V]
  type Entry = ProvSet.Entry[K, V]

  @inline def isEmpty  = repr.isEmpty
  @inline def nonEmpty = repr.nonEmpty

  @elidable(elidable.FINEST)
  override def toString =
    repr.iterator.map(_.toString).toList.sorted.mkString("ProvSet(", ",\n        ", ")")

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

  def ++(s: Self): Self =
    s.repr.foldLeft(this)(_ + _)

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████
  def +(add: Entry): Self = {
  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

    def addProv(p: Set[K], k: K): Set[K] =
      p.find(_ isComparableTo k) match {
        case None    => p + k
        case Some(j) =>
          if (j >= k)
            p
          else
            p - j + k
      }

    def mergeProvs(x: Set[K], y: Set[K]): Set[K] =
      y.foldLeft(x)(addProv)

    def mergeEntries(e: Entry, into: Entry): Entry = {
      val newProv =
        addProv(e.provenance, e.key).filterNot { k =>
          // No need for this check. As verified in ../TLA+/provset.tla this will never occur with monotonic clocks &
          // gossiping (as is the case with Drafts).
          // assert(!(k > into.key), s"Discarding provenance $k in order to add ${e.key} to ${into.key}")
          k <= into.key
        }

      val valuePair =
        MergePair(
          subject        = e.value,
          into           = into.value,
          subjectGreater = e.key > into.key)

      Entry(
        key        = into.key,
        value      = module.mergeValues(valuePair),
        provenance = mergeProvs(into.provenance, newProv)
      )
    }

    // TODO: Update TLA+
    @tailrec
    def go(base: Set[Entry], e: Entry): Set[Entry] = {

      val debug = !true

      val mo = base.find(_ isComparableTo e)

      if (debug) {
        println(
          s"""=======================================================================================
             |$base + $e
             |
             |mo: $mo
             |
             |""".stripMargin)
      }

      mo match {
        case None =>
          base + e

        case Some(i) =>
          val merged =
            if (e < i)
              mergeEntries(e, into = i)
            else
              mergeEntries(i, into = e)

          go(base - i, merged)
      }
    }

    if (isEmpty)
      one(add)
    else if (repr contains add)
      this
    else
      ProvSet(go(repr, add))
  }
}

object ProvSet {

  type Repr[K, V] = Set[Entry[K, V]]

  final case class Entry[K, V](key       : K,
                               value     : V,
                               provenance: Set[K]) {
    @elidable(elidable.FINEST)
    override def toString = {
      val prov = if (provenance.isEmpty) "" else provenance.iterator.map(_.toString).toList.sorted.mkString(" ≤{", ",", "}")
      s"{$key = $value$prov}"
    }
  }

  final case class MergePair[+A](subject: A, into: A, subjectGreater: Boolean) {
    def greater: A = if (subjectGreater) subject else into
    def lesser : A = if (subjectGreater) into else subject
  }

  implicit def univEqE[K: UnivEq, V: UnivEq]: UnivEq[Entry  [K, V]] = UnivEq.derive
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
    type Entry   = ProvSet.Entry[K, V]

    implicit def univEq: UnivEq[ProvSet] =
      ProvSet.univEq

    val empty: ProvSet =
      ProvSet[K, V](Set.empty)(this)

    val entry = Entry.apply[K, V] _

    def one(entry: Entry): ProvSet =
      ProvSet[K, V](Set.empty[Entry] + entry)(this)

    def consolidate(entries: Entry*): ProvSet =
      entries.foldLeft(empty)((s, e) => (s + e).assertProps(s"$s + $e"))

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████
    implicit val partialOrderE: PartialOrder[Entry] =
  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████
      PartialOrder((x, y) =>
        partialOrderK(x.key, y.key) match {
          case Separate =>
            val `x<=y` = y.provenance.exists(x.key <= _)
            val `y<=x` = x.provenance.exists(y.key <= _)
            (`x<=y`, `y<=x`) match {
              case (false, false) => Separate
              case (true , false) => Lesser
              case (false, true ) => Greater
              case (true , true ) => if (isAscending(x, y)) Lesser else Greater
            }

          case byKey =>
            byKey
        }
      )
  }

  // ===================================================================================================================

  final class Props[K, V](val module: Module[K, V]) {
    import nyaya.prop._
    import module.{Entry => E, ProvSet => S, partialOrderK}
    import ScalazExtra._

    type Prov = Set[K]

    def prov: Prop[Prov] =
      Prop.atom[Prov]("Provenance", ps => {
        var failure = Option.empty[String]
        for {
          p <- ps
          q <- ps - p
        } if (p isComparableTo q)
            failure = Some("Comparable keys in same provenance.")
        failure
      })

    def entry: Prop[E] = {
      def keyAndProv =
        Prop.forall[E, Set, K](_.provenance)(e =>
          Prop.atom("Entry key & provenance", p => Option.when(p <= e.key)(
            s"Entry ${e.key} has in its provenance, a key $p that is <= itself.")))

      (keyAndProv & prov.contramap((_: E).provenance)).rename("entry")
    }

    def provSet: Prop[S] =
      Prop.forall[S, Set, E](_.repr) { s =>

        def compareEntries(name: => String)(c: (E, E) => Boolean): Prop[E] =
          Prop.test[E](name, e => s.repr.forall(f => (e eq f) || c(e, f)))

        def coexistenceK: Prop[E] =
          compareEntries("Comparable sibling entries.")((e, f) =>
            e.key.isSeparateTo(f.key))

        def coexistenceP: Prop[E] =
          compareEntries("Sibling provenance.")((e, f) =>
            f.provenance.forall(p => !(e.key <= p)))

        (entry & coexistenceK & coexistenceP).rename("provSet")
      }
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
    type Entry   = shipreq.base.util.ProvSet.Entry[K, V]
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

    val laws: Laws =
      List(
        idempotency,
        associativity,
        commutativity,
        validity,
      ).reduce(_ & _)
  }
}
