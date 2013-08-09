package com.beardedlogic.usecase
package lib

import scala.reflect.ClassTag
import scalaz.NonEmptyList
import Types._
import change._
import field._
import text.FreeText
import model._
import util.{AppliedLens, LazyVal, BiMap}
import Changes._
import tree.TreeOps._


/** Narrows down the scope of a change. Paired with changes to indicate where (eg. which field) the change occurred. */
trait UcChangeDomain

object UseCaseHeader extends UcChangeDomain
// TODO Reverse params of UseCaseHeader
case class UseCaseHeader(title: String, number: Short)

// =====================================================================================================================

object UseCaseFns {

  def addChangesToResult(beforeTransform: UseCase, transformResult: UcUpdateResult, changes: NonEmptyList[(UcChangeDomain, Change)]): UcUpdateResult =
    transformResult match {
      case NoChange => Changed(beforeTransform, changes)
      case Changed(v,c) => Changed(v, c append changes)
      case f@ChangeFailure(_) => f
    }

  def keyByField(f: Field)(c: Change) = (f -> c)

  def filter[F <: Field](fields: List[Field])(implicit m: ClassTag[F]): List[F] =
    fields.filter(_.getClass.isAssignableFrom(m.runtimeClass)).asInstanceOf[List[F]]

  // TODO Minimise computation with savedSteps + StepAndLabelBiMap

  def extractStepAndLabelMaps(fieldValues: FieldValues, uch: UseCaseHeader): Iterable[Map[LocalIdStr, LabelStr]] =
    fieldValues.map {
      case (f, v: StepFieldValue) => generateStepAndLabelMap(f, v.tree, uch)
      case _ => Map.empty[LocalIdStr, LabelStr]
    }

  def generateSavedSteps(saveCtx: FieldSaveCtx): SavedSteps =
    BiMap(saveCtx.stepValues.map {case (localStepId, stepValue) => (stepValue.taggedDataId -> localStepId)})

  def mergeStepAndLabelMaps(maps: Iterable[Map[LocalIdStr, LabelStr]]): Map[LocalIdStr, LabelStr] =
    (Map.empty[LocalIdStr, LabelStr] /: maps)(_ ++ _)

  def generateStepAndLabelBiMap(maps: Iterable[Map[LocalIdStr, LabelStr]]): StepAndLabelBiMap =
    LazyVal(() => BiMap(mergeStepAndLabelMaps(maps)))

  def generateStepAndLabelBiMap(fieldValues: FieldValues, uch: UseCaseHeader): StepAndLabelBiMap =
    generateStepAndLabelBiMap(extractStepAndLabelMaps(fieldValues, uch))

  def generateStepAndLabelBiMap(uc: UseCase): StepAndLabelBiMap = generateStepAndLabelBiMap(uc.fieldValues, uc.header)

  def generateStepAndLabelMap(field: Field, tree: StepTree, uch: UseCaseHeader): Map[LocalIdStr, LabelStr] =
    field match {
      case f: StepField => mapIdsToFullLabels(tree.nodes, f.rootLabelPrefix(uch))
      case f => throw new IllegalStateException(s"Don't know how to mapIdsToFullLabels for field: $f")
    }

  /**
   * When a use case is updated, sometimes the stepsAndLabels map needs to be updated, other times it can be reused.
   * This will compare two UCs and return a new UC that is guaranteed to have an up-to-date stepsAndLabels map.
   *
   * @param original The UC before the update (ie. with a correct stepsAndLabels map).
   * @param updated An updated UC with a possibly-incorrect stepsAndLabels map.
   * @return A UC with the updated UC state, and a correct stepsAndLabels map.
   */
  def correctStepsAndLabelsAfterUpdate(original: UseCase, updated: UseCase): UseCase = {

    def reusable = (original eq updated) || (ucNumbersMatch && fieldValuesReusable)

    def ucNumbersMatch = original.header.number == updated.header.number

    def fieldValuesReusable = (original.fieldValues eq updated.fieldValues) || !relevantFieldValuesDiffer

    def relevantFieldValuesDiffer = original.fields.exists {
      case f: StepField => stepFieldValuesDiffer(f(original.fieldValues), f(updated.fieldValues))
      case _ => false
    }

    def stepFieldValuesDiffer(a: StepFieldValue, b: StepFieldValue) = a.tree ne b.tree

    if (reusable) updated
    else updated.regenerateStepsAndLabels
  }
}

// =====================================================================================================================

case class UseCase(
  header: UseCaseHeader,
  fields: List[Field],
  fieldValues: FieldValues,
  stepsAndLabels: StepAndLabelBiMap) {

  assume(fieldValues.keySet == fields.toSet, "There must be a field value for all fields.")

  import UseCaseFns._

  implicit protected def stepsAndLabelsImplicit = stepsAndLabels

  def toPrettyString: String = {
    val line = "-" * 98
    val fieldsPP = fields.map(f => s"  F: $f\n  V: ${fieldValues(f)}\n").mkString("\n")
    val snl = stepsAndLabels.get.ab.map {case (id, lbl) => "  %-16s <-- %s".format(lbl, id)}.toList.sorted.mkString("\n")
    (s">$line>\n"
      + s"Header: $header\n"
      + s"Fields:\n$fieldsPP\n"
      + s"StepsAndLabels (${stepsAndLabels.get.size}):\n$snl\n"
      + s"<$line<")
    .replace(FreeText.empty.toString, "FreeText.empty")
    .replace(filter[NormalCourseField](fields).head.empty.toString, "StepFieldValue.empty")
    .replace(filter[ExceptionCourseField](fields).head.empty.toString, "StepFieldValue.empty")
  }

  def pp() = { println(toPrettyString); this }

  def regenerateStepsAndLabels: UseCase =
    copy(stepsAndLabels = generateStepAndLabelBiMap(this))

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
      var fvs = this.fieldValues
      for (f <- this.fields; origFv <- this.fieldValues.get(f))
        changeField(origFv) match {
          case Changed(newFv, newChanges) =>
            fvs += (f -> newFv)
            totalChanges ++= newChanges.list.map(keyByField(f))
          case NoChange =>
        }
      ChangeResult <~ (fvs, totalChanges)
    }

    changeAllFields.map(newFVs => copy(fieldValues = newFVs))
  }

  def afterRespondingToChange(change: Change): UseCase = afterRespondingToChanges(change.asOnlyChange)
  def afterRespondingToChanges(changes: NonEmptyList[Change]): UseCase = respondToChanges(changes).getOrElse(this)

  // ------------------------------------------------------------------------------------------------------------

  @inline final def update[V](f: Field, cr: ChangeResultF[V, Change])(implicit l: AppliedLens[UseCase, V]): UcUpdateResult =
    update(cr, keyByField(f) _)

  def update[V](cr: ChangeResultF[V, Change], changeMapFn: Change => (UcChangeDomain, Change))(implicit l: AppliedLens[UseCase, V]): UcUpdateResult =
    cr.flatMapF((newValue, changes) => {
      val update1 = l.set(newValue)
      val update2 = correctStepsAndLabelsAfterUpdate(this, update1)
      val update3 = update2.respondToChanges(changes)
      addChangesToResult(update2, update3, changes.map(changeMapFn))
    })

  // TODO input-correction not sent back to client when state stays the same
  def updateTitle(input: String): UcUpdateResult = {
    implicit val lens = alens(FieldLenses.uc.title, this)
    val newTitle = InputCorrection.useCaseTitle(input)
    if (lens.get == newTitle) NoChange
    else update(newTitle @: TitleChanged(lens.get, newTitle), c => (UseCaseHeader, c))
  }
}
