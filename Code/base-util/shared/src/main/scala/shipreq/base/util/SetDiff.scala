package shipreq.base.util

import japgolly.microlibs.nonempty.NonEmpty

/**
 * The difference between two sets.
 */
final class SetDiff[A](val removed: Set[A], val added: Set[A]) {
  assert((removed & added).isEmpty, s"Same item(s) found in removed & added: ${removed & added}")

  override def toString =
    s"SetDiff(removed = $removed, added = $added)"

  override def hashCode =
    removed.## * 31 + added.##

  override def equals(o: Any) = o match {
    case b: SetDiff[A] => (removed == b.removed) && (added == b.added)
    case _             => false
  }

  def isEmpty: Boolean =
    removed.isEmpty && added.isEmpty

  def nonEmpty = !isEmpty

  def inverse: SetDiff[A] =
    new SetDiff(added, removed)

  def allValues: Set[A] =
    added ++ removed

  def apply(to: Set[A]): Set[A] =
    (to -- removed) ++ added

  def mapApply[B](f: A => B, to: Set[B]): Set[B] =
    (to -- removed.iterator.map(f)) ++ added.iterator.map(f)

  def applyToMultimapKeys[V](mm: Multimap[A, Set, V])(v: V): Multimap[A, Set, V] = {
    var tmp = mm
    removed.foreach(a => tmp = tmp.del(a, v))
    added  .foreach(a => tmp = tmp.add(a, v))
    tmp
  }

  def applyToMultimapValues[K](mm: Multimap[K, Set, A])(k: K): Multimap[K, Set, A] =
    mm.mod(k, apply)
}

object SetDiff {
  type NE[A] = NonEmpty[SetDiff[A]]

  implicit def equality[A: UnivEq]: UnivEq[SetDiff[A]] =
    UnivEq.force

  implicit def nonEmptiness[A]: NonEmpty.ProofMono[SetDiff[A]] =
    NonEmpty.Proof.testEmptiness(_.isEmpty)

  def empty[A: UnivEq]: SetDiff[A] = {
    val e = UnivEq.emptySet[A]
    apply(e, e)
  }

  def apply[A: UnivEq](removed: Set[A], added: Set[A]): SetDiff[A] =
    new SetDiff(removed, added)

  def compare[A: UnivEq](before: Set[A], after: Set[A]): SetDiff[A] =
    SetDiff(before -- after, after -- before)

  def compareOption[A: UnivEq](before: Option[Set[A]], after: Set[A]): SetDiff[A] =
    before match {
      case Some(b) => compare(b, after)
      case None    => SetDiff(removed = Set.empty, added = after)
    }

  def compareFn[A: UnivEq](before: Set[A]): Set[A] => SetDiff[A] =
    compare(before, _)

  def xor[A: UnivEq](current: Set[A], xor: Set[A]): SetDiff[A] = {
    val (del, add) = xor.partition(current.contains)
    SetDiff(del, add)
  }
}
