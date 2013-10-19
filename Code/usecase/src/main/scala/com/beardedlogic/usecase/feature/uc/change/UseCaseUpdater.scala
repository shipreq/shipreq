package com.beardedlogic.usecase.feature.uc
package change

import scalaz.{NonEmptyList, -\/, \/-}
import com.beardedlogic.usecase.db.UseCaseHeader
import com.beardedlogic.usecase.feature.InputValidator
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.util.AppliedLens
import field._
import Changes._
import UseCaseUpdateFns._
import UseCaseFns._

/** Narrows down the scope of a change. Paired with changes to indicate where (eg. which field) the change occurred. */
trait UcChangeDomain

object UseCaseUpdateFns {

  def keyByField(f: Field)(c: Change) = (f -> c)
}

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

    def changeField(fieldValue: Field#Value): ChangeResult[Field#Value, Change] = {
      var fieldChanges = List.empty[Change]
      var fv = fieldValue
      for (c <- changes.list)
        fv.respondToChange(c) match {
          case Changed(newFv, newChanges) =>
            fv = newFv
            fieldChanges ++= newChanges.list
          case NoChange =>
        }
      ChangeResult <~ (fv, fieldChanges)
    }

    val changeAllFields = {
      var totalChanges = List.empty[(Field, Change)]
      var fvs = uc.fieldValues
      for (f <- uc.fields; origFv <- uc.fieldValues.get(f))
        changeField(origFv) match {
          case Changed(newFv, newChanges) =>
            fvs += (f -> newFv)
            totalChanges ++= newChanges.list.map(keyByField(f))
          case NoChange =>
        }
      ChangeResult <~ (fvs, totalChanges)
    }

    changeAllFields.mapValue(newFVs => uc.copy(fieldValues = newFVs))
  }

  def afterRespondingToChange(change: Change): UseCase = afterRespondingToChanges(change.asOnlyChange)
  def afterRespondingToChanges(changes: NonEmptyList[Change]): UseCase = respondToChanges(changes).getValueOrElse(uc)

  @inline final def update[V](f: Field, cr: ChangeResultF[V, Change])(implicit l: AppliedLens[UseCase, V]): UcUpdateResult =
    update(cr, keyByField(f) _)

  def update[V](cr: ChangeResultF[V, Change], changeMapFn: Change => (UcChangeDomain, Change))(implicit l: AppliedLens[UseCase, V]): UcUpdateResult =
    cr.flatMapF((newValue, changes) => {
      val uc1 = l.set(newValue)
      val uc2 = correctStepsAndLabelsAfterUpdate(uc, uc1)
      val cr1 = Changed(uc2, changes.map(changeMapFn))
      val cr2 = copy(uc = uc2).respondToChanges(changes)
      cr1 appendF cr2
    })

  def updateTitle(input: String): UcUpdateResult = {
    implicit val lens = alens(Lenses.ucTitleL, uc)
    InputValidator.useCaseTitle.correctAndValidate(input) match {
      case -\/(err) => ChangeFailure(err)
      case \/-(newTitle) if newTitle == lens.get => NoChange
      case \/-(newTitle) => update(newTitle @: TitleChanged(lens.get, newTitle), c => (UseCaseHeader, c))
    }
  }
}
