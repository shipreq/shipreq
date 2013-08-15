package com.beardedlogic.usecase.lib.field

import com.beardedlogic.usecase.lib.StepTree
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.change._
import com.beardedlogic.usecase.lib.text.StepText
import com.beardedlogic.usecase.lib.tree.TreeOps._
import Changes._

object StepFieldValue {
  final def empty(field: StepField) = apply(field, StepTree.empty, Map.empty)

  def forTree(field: StepField, tree: StepTree) =
    apply(field, tree, tree.mapRecursive(n => (n.id -> StepText.empty(n.id))).toMap)
}

/**
 * The entire composite value of a step field.
 *
 * @param tree A tree of step nodes.
 * @param textmap Optional text values for nodes (ie. steps). If an entry doesn't exist for a node then that step is
 *                considered to have no text.
 */
case class StepFieldValue(field: StepField, tree: StepTree, textmap: Map[LocalIdStr, StepText]) extends ChangeResponder[StepFieldValue] {

  assume(textmap.keySet == tree.mapRecursive(_.id).toSet, "There must be a StepText for all steps.")

  override def respondToChange(c: Change)(implicit stepsAndLabels: StepAndLabelBiMap): ChangeResult[StepFieldValue, Change] = {

    def allowTitleChange_? = field.preferTitleInRoot_? && tree.nonEmpty

    def changeRootToTitle(before: String, after: String) = {
      val lens = alens(FieldLenses.sfv.stepText, (this, tree(0).id))
      val curText = lens.get.mainClause.text
      if (curText.isEmpty || curText == before)
        lens.get.updateMainClause(after).map(lens.set)
      else
        NoChange
    }

    def delegateChangeToStepTexts = {
      // Delegate msg to all StepText instances
      var newTextmap = textmap
      var changes = List.empty[Change]
      for ((id, curVal) <- textmap)
        curVal.respondToChange(c) match {
          case Changed(newVal, h) =>
            newTextmap += (id -> newVal)
            changes ++= h.list
          case NoChange =>
        }

      // Copy if changed
      ChangeResult <~(copy(textmap = newTextmap), changes)
    }

    c match {
      case TitleChanged(before, after) if allowTitleChange_? => changeRootToTitle(before, after)
      case _ => delegateChangeToStepTexts
    }
  }

  def getNormalisedText(id: LocalIdStr)(implicit savedSteps: SavedSteps): TextWithNormalisedRefs =
    textmap.get(id).map(_.textWithNormalisedRefs).getOrElse("".hasNormalisedRefs)

  def withNewStep(newTree: StepTree, stepId: LocalIdStr) =
    copy(tree = newTree, textmap = textmap + (stepId -> StepText.empty(stepId)))

  /**
   * Makes a copy with a new tree. New steps will have empty text added to the textmap; old, removed.
   */
  def withNewTree(newTree: StepTree) = {
    var newTextmap = Map.empty[LocalIdStr, StepText]
    for (n <- newTree) newTextmap += (n.id -> textmap.get(n.id).getOrElse(StepText.empty(n.id)))
    copy(tree = newTree, textmap = newTextmap)
  }

  def textByLabels(implicit stepsAndLabels: StepAndLabelBiMap): Map[LabelStr, String] =
    for ((id,t) <- textmap) yield (stepsAndLabels.get.ab(id), t.text)

  def toPrettyString: String = {
    val lines = s"StepFieldValue: $field, ${textmap.size} steps." +:
      textmap.map {case (id, t) => "    %-16s = %s".format(id, t.text)}.toList.sorted
    lines.mkString("\n")
  }
}
