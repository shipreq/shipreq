package shipreq.base.util

import nyaya.prop.{CycleDetector, CycleFree}
import scala.collection.Iterable
import shipreq.base.util.MMTree.Children

/**
 * Many-to-Many Tree typeclass.
 *
 * @tparam I Node id
 * @tparam T Tree type
 */
trait MMTree[I, T] {
  def modChildren(id: I, f: Children[I] => Children[I]): T => T
  def removeChild(parent: I, child: I): T => T
  def keySet(t: T): Set[I]
  def cycleDetector: CycleDetector[T, I]
}

object MMTree {

  /**
   * Each key is a parent of the subject node.
   * Each value is the sibling before which the subject tag should be inserted. (None => append.)
   */
  type Parents[I] = Map[I, RelPos[I]]

  /**
   * An ordered list of the subject's children.
   */
  type Children[I] = Vector[I]

  /**
   * A tree node's relations from its own point of view.
   *
   * @tparam I Node id
   */
  final case class Relations[I](parents: Parents[I], children: Children[I]) {

    // For testing
    def allReferencedIds: Set[I] =
      parents.keySet ++
      parents.values.filter(_.isDefined).map(_.get).toSet ++
      children.toSet
  }

  object Relations {
    implicit def equality[I: UnivEq]: UnivEq[Relations[I]] = UnivEq.force

    def empty[I]: Relations[I] =
      Relations(Map.empty, Vector.empty)

    def deriveParents[I: UnivEq](id: I, tree: Map[I, Vector[I]]): Parents[I] =
      tree
        .filter(_._2 contains id)
        .foldLeft(UnivEq.emptyMap: Parents[I]) {
          case (m, (parent, sibs)) => m.updated(parent, RelPos.get(sibs, id))
        }

    def derive[I: UnivEq](id: I, tree: Map[I, Vector[I]]): Relations[I] = {
      val children = tree.getOrElse(id, Vector.empty)
      val parents = deriveParents(id, tree)
      Relations(parents, children)
    }
  }

  sealed abstract class Apply[A[_]] {
    def safeApply1[I: UnivEq, T](tt: T, id: I)(a: A[I])(implicit T: MMTree[I, T]): (I, I) \/ CycleFree[T] =
      T.cycleDetector cycleFree trustedApply1(tt, id, a)

    def safeApplyN[I: UnivEq, T](tt: T, as: Iterable[(I, A[I])])(implicit T: MMTree[I, T]): (I, I) \/ CycleFree[T] =
      T.cycleDetector cycleFree trustedApplyN(tt, as)

    def trustedApplyN[I: UnivEq, T](tt: T, as: Iterable[(I, A[I])])(implicit T: MMTree[I, T]): T =
      as.foldLeft(tt) { case (t, (id, a)) => trustedApply1(t, id, a) }

    def trustedApply1[I: UnivEq, T](tt: T, id: I, a: A[I])(implicit T: MMTree[I, T]): T
  }

  object ApplyChildren extends Apply[Children] {
    override def trustedApply1[I: UnivEq, T](tt: T, id: I, children: Children[I])(implicit T: MMTree[I, T]): T =
      T.modChildren(id, _ => children)(tt)
  }

  object ApplyParents extends Apply[Parents] {
    override def trustedApply1[I: UnivEq, T](tt: T, id: I, parents: Parents[I])(implicit T: MMTree[I, T]): T = {
      var t = tt

      // Add parents
      for ((parent, pos) <- parents)
        t = T.modChildren(parent, RelPos.set(_, id, pos))(t)

      // Remove old parents
      val oldParents = T.keySet(t) - id -- parents.keySet
      for (p <- oldParents)
        t = T.removeChild(p, id)(t)

      t
    }
  }

  object ApplyRelations extends Apply[Relations] {
    override def trustedApply1[I: UnivEq, T](tt: T, id: I, rels: Relations[I])(implicit T: MMTree[I, T]): T = {
      var t = tt
      t = ApplyChildren.trustedApply1(t, id, rels.children)
      t = ApplyParents .trustedApply1(t, id, rels.parents)
      t
    }
  }
}
