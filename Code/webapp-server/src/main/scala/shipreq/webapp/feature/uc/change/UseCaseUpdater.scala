package shipreq.webapp.feature.uc
package change

import scala.collection.mutable.ListBuffer
import scalaz.NonEmptyList
import shipreq.webapp.feature.validation.Validators
import shipreq.webapp.util.AppliedLens
import field._
import Changes._
import UseCaseFns._

case class UseCaseUpdater(uc: UseCase, rels: UseCaseRelations) {

  @inline final def stepsAndLabels = uc.stepsAndLabels
  implicit val ctx = UcParsingCtx(uc, rels)

  /**
   * Passes a list of changes to all parts of a UC that respond to changes, allowing the components to transform
   * themselves based on the change. Finally a new UC with transformed state is returned.
   *
   * NOTE 1: StepsAndLabels is expected to be up-to-date.
   * NOTE 2: Input changes are will be present in the output changes. NoChange is a valid result here.
   */
  def respondToChanges(changes: NonEmptyList[Change]): UcUpdateResult = {

    def changeField(f: Field, fieldValue: Field#Value): ChangeResult[Field#Value, Change] = {
      val fv = f castV fieldValue
      f.changeResponder.respondToChanges(fv, changes)
    }

    val changeAllFields = {
      val changesOccurred = new ListBuffer[Change]
      var fvs = uc.fieldValues
      for (f <- uc.fields; origFv <- uc.fieldValues.get(f))
        changeField(f, origFv) match {
          case Changed(newFv, newChanges) =>
            fvs += (f -> newFv)
            changesOccurred ++= newChanges.list
          case NoChange =>
        }
      ChangeResult(fvs, changesOccurred.result)
    }

    changeAllFields.mapValue(newFVs => uc.copy(fieldValues = newFVs))
  }

  def afterRespondingToChange(change: Change): UseCase = afterRespondingToChanges(change.asOnlyChange)
  def afterRespondingToChanges(changes: NonEmptyList[Change]): UseCase = respondToChanges(changes).getValueOrElse(uc)

  /**
   * This is the core update process. All kinds of UC updates funnel through this method.
   */
  def update[V](cr: ChangeResultF[V, Change])(implicit l: AppliedLens[UseCase, V]): UcUpdateResult =
    cr.flatMapF((newValue, changes) => {
      val uc1 = l.set(newValue)
      val uc2 = correctStepsAndLabelsAfterUpdate(uc, uc1)
      val cr1 = Changed(uc2, changes)
      val cr2 = copy(uc = uc2).respondToChanges(changes)
      cr1 appendF cr2
    })

  def updateTitle(input: String): UcUpdateResult = {
    implicit val lens = AppliedLens(Lenses.ucTitleL, uc)
    def validator = Validators.usecase.title
    val c = validator.correctedU(input)

    if (c.value.isEmpty)
      // If the user clears the title field, restore the title back to its value before they cleared it
      NoChange
    else
      ChangeResult.fromValidation(validator.validateU(c))(newTitle =>
        if (newTitle == lens.get)
          NoChange
        else
          update(newTitle @: TitleChanged(lens.get, newTitle))
      )
  }
}
