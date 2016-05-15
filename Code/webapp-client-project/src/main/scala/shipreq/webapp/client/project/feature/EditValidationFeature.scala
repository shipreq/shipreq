package shipreq.webapp.client.project.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.univeq.UnivEq
import scalacss.ScalaCssReact._
import scalaz.Equal
import shipreq.base.util._
import shipreq.webapp.base.validation._
import shipreq.webapp.client.project.app.Style.{reqtable => *}

case class EditValidationFeature[+E, +A](value        : ValidUpdate[E, A],
                                         renderFailure: Option[ReactElement])

/** TODO Is this really a "feature"?
  */
object EditValidationFeature {
  type Result[+A] = EditValidationFeature[VFailure, A]

  @inline implicit def delegateValidUpdate[E, A](f: EditValidationFeature[E, A]): ValidUpdate[E, A] =
    f.value

  private def errorTag: ReactTag =
    <.div(*.cellEditorErrMsg)

  def apply[A](u: ValidUpdateVR[A]): Result[A] = {
    val f = u.getFailure.map[ReactElement](e => errorTag(e.toText)) // TODO Do better
    EditValidationFeature(u, f)
  }

  def compare[A](vr: ValidationResult[A])(previous: A)(implicit e: Equal[A]): Result[A] =
    apply(
      ValidUpdate.fromValidation(vr)
        .ignore(e.equal(previous, _)))

  def compareOption[A](vr: ValidationResult[A])(previous: Option[A])(implicit e: Equal[A]): Result[A] =
    apply(
      previous.foldLeft(
        ValidUpdate.fromValidation(vr))(
        (u, a) => u.ignore(e.equal(a, _))))

  def validate[A](a: A)(v: A => ValidationResult[A])(implicit e: Equal[A]): Result[A] =
    compare(v(a))(a)(e)

  def setDiff[A](vr: ValidationResult[SetDiff[A]]): Result[SetDiff.NE[A]] =
    apply(ValidUpdate.fromValidation(vr).flatMap(ValidUpdate nonEmpty _))

  def compareSetOption[A: UnivEq](vr: ValidationResult[Set[A]])(previous: Option[Set[A]]): Result[SetDiff.NE[A]] =
    setDiff(vr.map(SetDiff.compareOption(previous, _)))
}
