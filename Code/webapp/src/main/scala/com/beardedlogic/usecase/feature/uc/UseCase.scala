package com.beardedlogic.usecase
package feature.uc

import scala.reflect.ClassTag
import scalaz.Need
import db.UseCaseHeader
import lib.Types._
import lib.Misc.isCovar
import feature.Inspection
import util.BiMap
import field._
import text.FreeText
import step.StepTree
import step.TreeOps._

case class UcParsingCtx(ucn: UseCaseNumber, title: String, stepsAndLabels: StepAndLabelBiMap, rels: UseCaseRelations) {
  def getIdsToLabels: Map[LocalStepId, StepLabel] = stepsAndLabels.value.ab
  def getLabelsToIds: Map[StepLabel, LocalStepId] = stepsAndLabels.value.ba

  def update(uc: UseCase): UcParsingCtx = UcParsingCtx(uc, rels)
}
object UcParsingCtx {
  val Empty: UcParsingCtx = new UcParsingCtx((0:Short).tag[IsUseCaseNumber], "", EmptyStepAndLabelBiMap, UseCaseRelations.Empty) {
    override def toString = "UcParsingCtx.Empty"
  }

  def apply(uc: UseCase, rels: UseCaseRelations) =
    new UcParsingCtx(uc.number, uc.title, uc.stepsAndLabels, rels)
}

// =====================================================================================================================

object UseCaseFns {

  def reqId(n: UseCaseNumber) = s"UC-$n"
  def fullName(n: UseCaseNumber, title: String) = s"UC-$n: $title"

  def filterKeys[F <: Field](fieldValues: FieldValues)(implicit m: ClassTag[F]): Map[F, F#Value] =
    fieldValues.filterKeys(isCovar[F]).asInstanceOf[Map[F, F#Value]]

  // TODO Minimise computation with savedSteps + StepAndLabelBiMap

  def extractStepAndLabelMaps(ucn: UseCaseNumber, fieldValues: FieldValues): Iterable[Map[LocalStepId, StepLabel]] =
    fieldValues.map {
      case (f, v: StepFieldValue) => generateStepAndLabelMap(ucn, f, v.tree)
      case _ => Map.empty[LocalStepId, StepLabel]
    }

  def mergeStepAndLabelMaps(maps: Iterable[Map[LocalStepId, StepLabel]]): Map[LocalStepId, StepLabel] =
    (Map.empty[LocalStepId, StepLabel] /: maps)(_ ++ _)

  def generateStepAndLabelMap(ucn: UseCaseNumber, field: Field, tree: StepTree): Map[LocalStepId, StepLabel] =
    field match {
      case f: StepField => mapIdsToFullLabels(tree.nodes, f.rootLabelPrefix(ucn))
      case f => throw new IllegalStateException(s"Don't know how to generateStepAndLabelMap for field: $f")
    }

  def generateStepAndLabelBiMap(maps: Iterable[Map[LocalStepId, StepLabel]]): StepAndLabelBiMap =
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

import UseCaseFns._

// =====================================================================================================================

object UseCase {
  def as(number: UseCaseNumber, header: UseCaseHeader, fieldValues: Seq[(Field, Field#Value)], stepsAndLabels: StepAndLabelBiMap): UseCase =
    UseCase(number, header, fieldValues.map(_._1).toList, fieldValues.toMap, stepsAndLabels)

  implicit object ordering extends Ordering[UseCase] {
    def compare(x: UseCase, y: UseCase): Int = x.number compareTo y.number
  }
}

case class UseCase(
  number: UseCaseNumber,
  header: UseCaseHeader,
  fields: List[Field],
  fieldValues: FieldValues,
  stepsAndLabels: StepAndLabelBiMap) {

  assume(fieldValues.keySet == fields.toSet, "There must be a field value for all fields.")

  implicit protected def stepsAndLabelsImplicit = stepsAndLabels

  @inline final def title = header.title

  def regenerateStepsAndLabels: UseCase =
    copy(stepsAndLabels = generateStepAndLabelBiMap(this))

  /** Returns Scala code that can be used to recreate this use case. */
  def inspect: String = Inspection.ucShow.shows(this)

  /** Returns a textual view of the entire use case that includes internal IDs. */
  def devView: String = {
    def ppFV(v: Field#Value): String = v match {
      case sfv: StepFieldValue => ppSfv(sfv)
      case _ => v.toString
    }
    def ppSfv(sfv: StepFieldValue): String = {
      import sfv._
      val lines = s"StepFieldValue: $field, ${textmap.size} steps." +:
        textmap.map {case (id, t) => "    %-16s = %s".format(id, t.text)}.toList.sorted
      lines.mkString("\n")
    }
    val line = "-" * 98
    val fieldsPP = fields.map(f => s"  F: $f\n  V: ${ppFV(fieldValues(f))}\n").mkString("\n")
    val snl = stepsAndLabels.value.ab.map {case (id, lbl) => "  %-16s <-- %s".format(lbl, id)}.toList.sorted.mkString("\n")
    (s">$line>\n"
      + s"Header: $header\n"
      + s"Fields:\n$fieldsPP\n"
      + s"StepsAndLabels (${stepsAndLabels.value.size}):\n$snl\n"
      + s"<$line<")
    .replace(FreeText.empty.toString, "FreeText.empty")
  }

  /** Returns a textual view of the entire use case as the user would see it. */
  def userView: String = {
    def text(t: String) = "⟦" + t.replaceAll("\n","\n\t\t") + "⟧"
    def printF(f: Field, v: Field#Value): String = f match {
      case tf: TextField => "%-30s: %s".format(tf.defn.title, text(tf.castV(v).text))
      case sf: StepField => "%s\n%s".format(sf.getClass.getSimpleName, printSFV(sf.castV(v)))
      case fg: FlowGraphField => "<FlowGraph>"
    }
    def printSFV(sfv: StepFieldValue): String =
        sfv.textByLabels.map{case (l,t) => "  %-18s: %s".format(l, text(t))}.toList.sorted.mkString("\n")
    val line = "-" * 98
    val lineS = s">$line>"
    val lineE = s"<$line<"
    val fieldsStr = fields.map(f => printF(f, fieldValues(f))).mkString("\n")
    s"\n$lineS\n$fullName\n$lineS\n$fieldsStr\n$lineE"
  }

  override def toString = inspect

  def reqId = UseCaseFns.reqId(this)
  def fullName = UseCaseFns.fullName(number, title)
}
