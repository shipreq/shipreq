package shipreq.webapp.client.project.feature

import japgolly.univeq.UnivEq
import scalaz.{Equal, \/}
import shipreq.base.util._
import shipreq.webapp.base.validation.Simple._

// TODO Is this really a "feature"? Overlap with EditorStatus?
case class EditValidationFeature[+E, +A](value: ValidUpdate[E, A])

object EditValidationFeature {
  type Result[+A] = EditValidationFeature[Invalidity, A]

  @inline implicit def delegateValidUpdate[E, A](f: EditValidationFeature[E, A]): ValidUpdate[E, A] =
    f.value

  def compare[A](vr: Invalidity \/ A)(previous: A)(implicit e: Equal[A]): Result[A] =
    apply(
      ValidUpdate.fromDisjunction(vr)
        .ignore(e.equal(previous, _)))

  def compareOption[A](vr: Invalidity \/ A)(previous: Option[A])(implicit e: Equal[A]): Result[A] =
    apply(
      previous.foldLeft(
        ValidUpdate.fromDisjunction(vr))(
        (u, a) => u.ignore(e.equal(a, _))))

  def validate[A](a: A)(v: A => Invalidity \/ A)(implicit e: Equal[A]): Result[A] =
    compare(v(a))(a)(e)

  def setDiff[A](vr: Invalidity \/ SetDiff[A]): Result[SetDiff.NE[A]] =
    apply(ValidUpdate.fromDisjunction(vr).flatMap(ValidUpdate nonEmpty _))

  def compareSetOption[A: UnivEq](vr: Invalidity \/ Set[A])(previous: Option[Set[A]]): Result[SetDiff.NE[A]] =
    setDiff(vr.map(SetDiff.compareOption(previous, _)))
}
