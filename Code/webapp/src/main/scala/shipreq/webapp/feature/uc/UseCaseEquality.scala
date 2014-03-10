package shipreq.webapp
package feature.uc

import scalaz.{Order, Equal}
import scalaz.std.map.mapEqual
import scalaz.std.string.stringInstance
import scalaz.syntax.equal._
import field._
import text._
import lib.Types._
import db.UseCaseHeader

object UseCaseEquality {

  /**
   * Fields are not customisable: No point comparing field list.
   */
  implicit val uc: Equal[UseCase] = Equal.equal((a, b) => (
    a.header === b.header
      && a.stepsAndLabels === b.stepsAndLabels
      && a.fieldValues === b.fieldValues
    ))

  /**
   * Fields are not customisable: Keys and size are guaranteed to match.
   */
  implicit val fieldValues: Equal[FieldValues] = Equal.equal((m, n) =>
    !m.exists {case (f, a) => !equalFieldValues(f, a, n(f))}
  )

  def equalFieldValues(ff: Field, a: Field#Value, b: Field#Value) = ff match {
    case f: TextField      => f.castV(a) === f.castV(b)
    case f: StepField      => sfvTextOnly.equal(f.castV(a), f.castV(b))
    case f: FlowGraphField => true
  }

  def stringTagOrder[S <: String @@ TypeTag[String]]: Order[S] = stringInstance.asInstanceOf[Order[S]]
  implicit def localStepIdOrder: Order[LocalStepId] = stringTagOrder
  implicit def stepLabelOrder: Order[StepLabel] = stringTagOrder

  implicit val stepAndLabels: Equal[StepAndLabelBiMap] = Equal.equalBy(_.value.ab)

  implicit val uch: Equal[UseCaseHeader] = Equal.equalA

  /**
   * When `FreeText.text` matches then `FreeText.refs` is guaranteed to match too.
   */
  implicit val freeText: Equal[FreeText] = Equal.equalBy(_.text)

  /**
   * Ignore the step-tree because it's easier and faster to check `UseCase.stepsAndLabels`.
   * Accordingly this equality check is unreliable if being used outside of a UC comparison.
   *
   * `StepFieldValue.field` ignored because it's not part of the value.
   * Rules of `UC.fieldValues` (ie. `map.key == map(key).field`) ensure that SFVs with different field values won't
   * be compared.
   */
  // https://github.com/scalaz/scalaz/issues/515
  /*not-implicit*/ //val sfvTextOnly: Equal[StepFieldValue] = Equal.equalBy(_.textmap)
  /*not-implicit*/ val sfvTextOnly: Equal[StepFieldValue] = Equal.equal(_.textmap === _.textmap)

  /**
   * All clauses are derived from `StepText.text`.
   *
   * `StepText.stepId` ignored because it's not part of the value.
   * Rules of `SFV.textmap` (ie. `map.key == map(key).stepId`) ensure that StepTexts with different stepId values won't
   * be compared.
   */
  implicit val stepText: Equal[StepText] = Equal.equalBy(_.text)
}
