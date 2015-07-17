package shipreq.base.util

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

  def apply(to: Set[A]): Set[A] =
    (to -- removed) ++ added

  def inverse: SetDiff[A] =
    new SetDiff(added, removed)

  def allValues: Set[A] =
    added ++ removed
}

object SetDiff {
  implicit def equality[A: UnivEq]: UnivEq[SetDiff[A]] = UnivEq.force

  def apply[A: UnivEq](removed: Set[A], added: Set[A]): SetDiff[A] =
    new SetDiff(removed, added)

  def compare[A: UnivEq](before: Set[A], after: Set[A]): SetDiff[A] =
    SetDiff(before -- after, after -- before)

  def compareFn[A: UnivEq](before: Set[A]): Set[A] => SetDiff[A] =
    compare(before, _)
}
