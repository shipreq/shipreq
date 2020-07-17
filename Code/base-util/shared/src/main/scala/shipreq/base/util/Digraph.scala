package shipreq.base.util

import monocle.Iso
import nyaya.prop.{CycleDetector => CycleDetectorN, Prop}
import nyaya.util.Multimap
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

    def memberIterator: Iterator[A] =
      Digraph.memberIterator(forwards)

    def members: Set[A] =
      Digraph.members(forwards)

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
