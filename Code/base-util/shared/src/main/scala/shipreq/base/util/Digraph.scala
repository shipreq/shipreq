package shipreq.base.util

import monocle.Iso
import nyaya.prop.{CycleDetector => CycleDetectorN, Prop}
import scala.reflect.ClassTag

/**
  * Directed Graph.
  *
  * For acyclic graphs use [[Digraph.cycleDetector]], [[Digraph.acyclicPropUni]] or [[Digraph.acyclicPropBi]].
  */
object Digraph {

  /**
    * Uni-directional DAG.
    */
  type UniDir[A] = Multimap[A, Set, A]

  /**
    * Bi-directional DAG.
    */
  final case class BiDir[A: UnivEq](forwards: UniDir[A]) {

    lazy val backwards: UniDir[A] =
      forwards.reverse

    def apply(dir: Direction): UniDir[A] =
      dir match {
        case Forwards  => forwards
        case Backwards => backwards
      }

    def transitiveClosure(dir   : Direction,
                          keys  : IterableOnce[A],
                          filter: A => TransitiveClosure.Filter = TransitiveClosure.Filter.followAll)
                         (implicit ct: ClassTag[A]): TransitiveClosure[A] =
      TransitiveClosure.auto(keys)(apply(dir).apply, filter)

    lazy val stronglyConnectedComponents: Set[NonEmptySet[A]] =
      Digraph.stronglyConnectedComponents(this)

    def modify[B: UnivEq](f: UniDir[A] => UniDir[B]): BiDir[B] =
      BiDir(f(forwards))
  }

  // ===================================================================================================================

  implicit def equalityBiDir[A: UnivEq]: UnivEq[BiDir[A]] =
    UnivEq.derive

  def emptyUniDir[A: UnivEq]: UniDir[A] =
    UnivEq.emptySetMultimap[A, A]

  def emptyBiDir[A: UnivEq]: BiDir[A] =
    BiDir(emptyUniDir)

  def biToUni[A: UnivEq]: Iso[BiDir[A], UniDir[A]] =
    Iso((_: BiDir[A]).forwards)(BiDir(_))

  def memberIterator[A](dag: UniDir[A]): Iterator[A] =
    dag.m.iterator.flatMap(e =>
      collection.Iterator.single(e._1) ++ e._2.iterator)

  def members[A: UnivEq](dag: UniDir[A]): Set[A] = {
    val b = UnivEq.setBuilder[A]
    for (e <- dag.m.iterator) {
      b += e._1
      b ++= e._2
    }
    b.result()
  }

  class Fix[A: UnivEq : ClassTag] {
    final type UniDir = Digraph.UniDir[A]
    final type BiDir = Digraph.BiDir[A]

    final def UniDir = Multimap
    final def BiDir = Digraph.BiDir

    final def emptyUniDir: UniDir =
      Digraph.emptyUniDir

    final def emptyBiDir: BiDir =
      Digraph.emptyBiDir

    final val biToUni: Iso[BiDir, UniDir] =
      Digraph.biToUni
  }

  // ===================================================================================================================

  final case class RootsAndTerminals[A](roots: Set[A], terminals: Set[A], members: Set[A])

  object RootsAndTerminals {
    def derive[A](graph: BiDir[A]): RootsAndTerminals[A] = {
      val f = graph.forwards.m
      val b = graph.backwards.m

      var members   = Set.empty[A]
      var roots     = Set.empty[A]
      var terminals = Set.empty[A]

      val processTgt: A => Unit = a =>
        if (!members.contains(a)) {
          members += a
          if (!f.contains(a))
            terminals += a
        }

      for (x <- f) {
        val src = x._1
        val tgts = x._2

        if (!members.contains(src)) {
          members += src
          if (!b.contains(src))
            roots += src
        }

        tgts.foreach(processTgt)
      }
      apply(
        members = members,
        roots = roots,
        terminals = terminals,
      )
    }
  }

  // ===================================================================================================================

  final case class Roots[A](roots: Set[A], members: Set[A])

  object Roots {
    def derive[A](graph: BiDir[A]): Roots[A] = {
      val f = graph.forwards.m
      val b = graph.backwards.m

      var members = Set.empty[A]
      var roots   = Set.empty[A]

      val processTgt: A => Unit = a =>
        if (!members.contains(a)) {
          members += a
        }

      for (x <- f) {
        val src = x._1
        val tgts = x._2

        if (!members.contains(src)) {
          members += src
          if (!b.contains(src))
            roots += src
        }

        tgts.foreach(processTgt)
      }
      apply(
        members = members,
        roots = roots,
      )
    }
  }

  // ===================================================================================================================

  private def stronglyConnectedComponents[A: UnivEq](graph: BiDir[A]): Set[NonEmptySet[A]] = {
    // Uses Kosaraju's algorithm.

    var stack = List.empty[A]
    val visited = collection.mutable.HashSet.empty[A]

    // Pass 1
    {
      val forwards = graph.forwards

      def go(a: A): Unit = {
        val firstVisit = visited.add(a)
        if (firstVisit) {
          for (to <- forwards(a))
            go(to)
          stack ::= a
        }
      }

      for (from <- forwards.keys)
        go(from)
    }

    // Pass 2
    var results = Set.empty[NonEmptySet[A]];
    {
      val backwards = graph.backwards

      while (stack.nonEmpty) {
        val sccHead = stack.head
        stack = stack.tail

        if (visited.contains(sccHead)) {
          var sccTail = Set.empty[A]
          def go(a: A): Unit = {
            val firstVisit = visited.remove(a)
            if (firstVisit) {
              sccTail += a
              for (to <- backwards(a))
                go(to)
            }
          }
          go(sccHead)

          val scc = NonEmptySet(sccHead, sccTail)
          results += scc
        }
      }
    }

    results
  }

  // ===================================================================================================================
  // Acyclicicity

  type CycleDetector[A] = CycleDetectorN[Map[A, Set[A]], A]

  def cycleDetector[A: UnivEq, I: UnivEq](id: A => I): CycleDetector[A] =
    CycleDetectorN.Directed.multimap[Set, A, I](id, UnivEq.emptySet)

  def acyclicPropUni[A](name: => String, cd: CycleDetector[A]): Prop[UniDir[A]] =
    cd.noCycleProp(name).contramap[UniDir[A]](_.m)

  def acyclicPropBi[A](uni: Prop[UniDir[A]]): Prop[BiDir[A]] =
    uni.contramap[BiDir[A]](_.forwards)

  class FixAcyclic[A: UnivEq : ClassTag, I: UnivEq](name: String, id: A => I) extends Fix[A] {
    final val cycleDetector =
      Digraph.cycleDetector(id)

    final val acyclicPropUni: Prop[UniDir] =
      Digraph.acyclicPropUni(name, cycleDetector)

    final val acyclicPropBi: Prop[BiDir] =
      Digraph.acyclicPropBi(acyclicPropUni)
  }

}
