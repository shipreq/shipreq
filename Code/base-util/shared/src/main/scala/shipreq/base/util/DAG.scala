package shipreq.base.util

import monocle.Iso
import nyaya.prop.{CycleDetector => CycleDetectorN, Prop}
import nyaya.util.Multimap
import scala.reflect.ClassTag

/**
  * Directed Acyclic Graph.
  *
  * Acyclicity isn't automatically verified. Use [[DAG.cycleDetector]], [[DAG.propUni]] or [[DAG.propBi]].
  */
object DAG {

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

    def dir(fwd: Boolean): UniDir[A] =
      if (fwd) forwards else backwards

    def members: Set[A] =
      DAG.members(forwards)
  }

  // ===================================================================================================================

  type CycleDetector[A] = CycleDetectorN[Map[A, Set[A]], A]

  implicit def equalityBiDir[A: UnivEq]: UnivEq[BiDir[A]] =
    UnivEq.derive

  def emptyUniDir[A: UnivEq]: UniDir[A] =
    UnivEq.emptySetMultimap[A, A]

  def emptyBiDir[A: UnivEq]: BiDir[A] =
    BiDir(emptyUniDir)

  def biToUni[A: UnivEq]: Iso[BiDir[A], UniDir[A]] =
    Iso((_: BiDir[A]).forwards)(BiDir(_))

  def members[A: UnivEq](dag: UniDir[A]): Set[A] = {
    val b = UnivEq.setBuilder[A]
    for (e <- dag.m.iterator) {
      b += e._1
      b ++= e._2
    }
    b.result()
  }

  def cycleDetector[A: UnivEq, I: UnivEq](id: A => I): CycleDetector[A] =
    CycleDetectorN.Directed.multimap[Set, A, I](id, UnivEq.emptySet)

  def transitiveClosure[A: UnivEq : ClassTag](keys: Iterable[A], dead: Set[A], dag: UniDir[A]): TransitiveClosure[A] =
    TransitiveClosure.auto[A](keys)(dag.apply, !dead.contains(_))

  def propUni[A](name: => String, cd: CycleDetector[A]): Prop[UniDir[A]] =
    cd.noCycleProp(name).contramap[UniDir[A]](_.m)

  def propBi[A](uni: Prop[UniDir[A]]): Prop[BiDir[A]] =
    uni.contramap[BiDir[A]](_.forwards)

  // ===================================================================================================================

  final class Fix[A: UnivEq : ClassTag, I: UnivEq](name: String, id: A => I) {
    type UniDir = DAG.UniDir[A]
    type BiDir = DAG.BiDir[A]

    def UniDir = Multimap
    def BiDir = DAG.BiDir

    def emptyUniDir: UniDir =
      DAG.emptyUniDir

    def emptyBiDir: BiDir =
      DAG.emptyBiDir

    val biToUni: Iso[BiDir, UniDir] =
      DAG.biToUni

    val cycleDetector =
      DAG.cycleDetector(id)

    def transitiveClosure(keys: Iterable[A], dead: Set[A], dag: UniDir): TransitiveClosure[A] =
      DAG.transitiveClosure(keys, dead, dag)

    val propUni: Prop[UniDir] =
      DAG.propUni(name, cycleDetector)

    val propBi: Prop[BiDir] =
      DAG.propBi(propUni)
  }

}
