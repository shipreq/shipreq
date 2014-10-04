package shipreq.webapp.client.ui.table

import scalaz.{NonEmptyList, Equal}
import shipreq.webapp.shared.validation.{ValidatePlusR, VFailure}
import scalaz.syntax.equal._

object TableConstraint {

  def uniquenessFailure(fieldName: String) =
    VFailure.forField(fieldName, NonEmptyList("must be unique."))

  final class UniquenessB[S, R, A](b: VFailure => ValidatePlusR[S, R, A]) {
    def apply(fail: VFailure) = b(fail)
    def fieldName(fieldName: String) = apply(uniquenessFailure(fieldName))
    def failureMsg(msg: String) = apply(VFailure looseMsg msg)
  }

  def uniqueness[S, R, A, B](extract: (S, R) => Stream[A], cmp: (A, B) => Boolean) =
    new UniquenessB[S, R, B](
      fail => r => (s, b) =>
        if (extract(s, r).exists(cmp(_, b))) Some(fail) else None
    )

  def uniquenessE[S, R, A: Equal](extract: (S, R) => Stream[A]) =
    uniqueness[S, R, A, A](extract, implicitly[Equal[A]].equal)

  def uniquenessT[D, P, II, A: Equal](pa: P => A) =
    uniqueness[SavedAndUnsaved[D, P, II], Option[D], (D, SavedRow[P, II]), A](
      // D is used to key maps so Scala equality must hold, no need for Equal[D]
      (s, od) => getSaved(s).toStream.filter(r => od.forall(_ != r._1)),
      (r, a)  => a ≟ pa(r._2.p))
}
