package com.beardedlogic.usecase.feature.uc
package change

import scala.collection.mutable.ListBuffer
import scalaz.{NonEmptyList, -\/, \/-}
import com.beardedlogic.usecase.feature.InputValidator
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.util.AppliedLens
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
      val changesOccurred = new ListBuffer[Change]
      var fv = f castV fieldValue
      var changeResponder = f.changeResponder(fv)
      for (c <- changes.list)
        changeResponder.respondToChange(c) match {
          case Changed(newFv, newChanges) =>
            fv = newFv
            changesOccurred ++= newChanges.list
            changeResponder = f.changeResponder(fv)
          case NoChange =>
        }
      ChangeResult(fv, changesOccurred.result)
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

  @inline final def update[V](f: Field, cr: ChangeResultF[V, Change])(implicit l: AppliedLens[UseCase, V]): UcUpdateResult =
    update(cr)

  def update[V](cr: ChangeResultF[V, Change])(implicit l: AppliedLens[UseCase, V]): UcUpdateResult =
    cr.flatMapF((newValue, changes) => {
      val uc1 = l.set(newValue)
      val uc2 = correctStepsAndLabelsAfterUpdate(uc, uc1)
      val cr1 = Changed(uc2, changes)
      val cr2 = copy(uc = uc2).respondToChanges(changes)
      cr1 appendF cr2
    })

  def updateTitle(input: String): UcUpdateResult = {
    implicit val lens = alens(Lenses.ucTitleL, uc)
    InputValidator.useCaseTitle.correctAndValidate(input) match {
      case -\/(err) => ChangeFailure(err)
      case \/-(newTitle) if newTitle == lens.get => NoChange
      case \/-(newTitle) => update(newTitle @: TitleChanged(lens.get, newTitle))
    }
  }
}
