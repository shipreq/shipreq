package com.beardedlogic.usecase
package lib

import scala.reflect.ClassTag
import scalaz.{Need, NonEmptyList}
import Types._
import db.UseCaseHeader
import change._
import field._
import text.FreeText
import util.{AppliedLens, BiMap}
import Changes._
import tree.TreeOps._

/** Narrows down the scope of a change. Paired with changes to indicate where (eg. which field) the change occurred. */
trait UcChangeDomain

// =====================================================================================================================

object UseCaseFns {

  def addChangesToResult(beforeTransform: UseCase, transformResult: UcUpdateResult, changes: NonEmptyList[(UcChangeDomain, Change)]): UcUpdateResult =
    transformResult match {
      case NoChange => Changed(beforeTransform, changes)
      case Changed(v,c) => Changed(v, c append changes)
      case f@ChangeFailure(_) => f
    }

  def keyByField(f: Field)(c: Change) = (f -> c)

  def isF[F <: Field](a: Field)(implicit m: ClassTag[F]): Boolean =
    m.runtimeClass.isAssignableFrom(a.getClass)

  def filter[F <: Field](fields: List[Field])(implicit m: ClassTag[F]): List[F] =
    fields.filter(isF[F]).asInstanceOf[List[F]]

  def filter[F <: Field](fieldValues: FieldValues)(implicit m: ClassTag[F]): Map[F, F#Value] =
    fieldValues.filterKeys(isF[F]).asInstanceOf[Map[F, F#Value]]

  // TODO Minimise computation with savedSteps + StepAndLabelBiMap

  def extractStepAndLabelMaps(ucn: UseCaseNumber, fieldValues: FieldValues): Iterable[Map[LocalStepId, LabelStr]] =
    fieldValues.map {
      case (f, v: StepFieldValue) => generateStepAndLabelMap(ucn, f, v.tree)
      case _ => Map.empty[LocalStepId, LabelStr]
    }

  def mergeStepAndLabelMaps(maps: Iterable[Map[LocalStepId, LabelStr]]): Map[LocalStepId, LabelStr] =
    (Map.empty[LocalStepId, LabelStr] /: maps)(_ ++ _)

  def generateStepAndLabelMap(ucn: UseCaseNumber, field: Field, tree: StepTree): Map[LocalStepId, LabelStr] =
    field match {
      case f: StepField => mapIdsToFullLabels(tree.nodes, f.rootLabelPrefix(ucn))
      case f => throw new IllegalStateException(s"Don't know how to generateStepAndLabelMap for field: $f")
    }

  def generateStepAndLabelBiMap(maps: Iterable[Map[LocalStepId, LabelStr]]): StepAndLabelBiMap =
    Need(BiMap(mergeStepAndLabelMaps(maps)))

  def generateStepAndLabelBiMap(ucn: UseCaseNumber, trees: (StepField, StepTree)*): StepAndLabelBiMap =
    generateStepAndLabelBiMap(trees.map {
      case (f, t) => generateStepAndLabelMap(ucn, f, t)
    })

  def generateStepAndLabelBiMap(ucn: UseCaseNumber, fieldValues: FieldValues): StepAndLabelBiMap =
    generateStepAndLabelBiMap(extractStepAndLabelMaps(ucn, fieldValues))

  def generateStepAndLabelBiMap(uc: UseCase): StepAndLabelBiMap =
    generateStepAndLabelBiMap(uc.number, uc.fieldValues)

  /**
   * When a use case is updated, sometimes the stepsAndLabels map needs to be updated, other times it can be reused.
   * This will compare two UCs and return a new UC that is guaranteed to have an up-to-date stepsAndLabels map.
   *
   * @param original The UC before the update (ie. with a correct stepsAndLabels map).
   * @param updated An updated UC with a possibly-incorrect stepsAndLabels map.
   * @return A UC with the updated UC state, and a correct stepsAndLabels map.
   */
  def correctStepsAndLabelsAfterUpdate(original: UseCase, updated: UseCase): UseCase = {

    def reusable = (original eq updated) || fieldValuesReusable

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

object UseCase {
  def as(number: UseCaseNumber, header: UseCaseHeader, fieldValues: Seq[(Field, Field#Value)], stepsAndLabels: StepAndLabelBiMap): UseCase =
    UseCase(number, header, fieldValues.map(_._1).toList, fieldValues.toMap, stepsAndLabels)
}

// =====================================================================================================================

case class UseCase(
  number: UseCaseNumber,
  header: UseCaseHeader,
  fields: List[Field],
  fieldValues: FieldValues,
  stepsAndLabels: StepAndLabelBiMap) {

  assume(fieldValues.keySet == fields.toSet, "There must be a field value for all fields.")

  import UseCaseFns._

  implicit protected def stepsAndLabelsImplicit = stepsAndLabels

  def toPrettyString: String = {
    def printFieldValue(v: Field#Value): String = v match {
      case sfv: StepFieldValue => sfv.toPrettyString
      case _ => v.toString
    }
    val line = "-" * 98
    val fieldsPP = fields.map(f => s"  F: $f\n  V: ${printFieldValue(fieldValues(f))}\n").mkString("\n")
    val snl = stepsAndLabels.value.ab.map {case (id, lbl) => "  %-16s <-- %s".format(lbl, id)}.toList.sorted.mkString("\n")
    (s">$line>\n"
      + s"Header: $header\n"
      + s"Fields:\n$fieldsPP\n"
      + s"StepsAndLabels (${stepsAndLabels.value.size}):\n$snl\n"
      + s"<$line<")
    .replace(FreeText.empty.toString, "FreeText.empty")
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

  def updateTitle(input: String): UcUpdateResult = {
    implicit val lens = alens(Lenses.ucTitleL, this)
    val newTitle = InputCorrection.useCaseTitle(input)
    if (lens.get == newTitle) NoChange
    else update(newTitle @: TitleChanged(lens.get, newTitle), c => (UseCaseHeader, c))
  }
}
