package com.beardedlogic.usecase.feature.uc.field

import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.feature.uc.{UcParsingCtx, Lenses}
import com.beardedlogic.usecase.feature.uc.change._
import com.beardedlogic.usecase.feature.uc.step.StepTree
import com.beardedlogic.usecase.feature.uc.step.TreeOps._
import com.beardedlogic.usecase.feature.uc.text.{StepTextUpdater, StepText}
import Changes._

object StepFieldValue {
  final def empty(field: StepField) = apply(field, StepTree.empty, Map.empty)

  def forTree(field: StepField, tree: StepTree) =
    apply(field, tree, tree.mapRecursive(n => (n.id -> StepText.empty)).toMap)
}

/**
 * The entire composite value of a step field.
 *
 * @param tree A tree of step nodes.
 * @param textmap Optional text values for nodes (ie. steps). If an entry doesn't exist for a node then that step is
 *                considered to have no text.
 */
case class StepFieldValue(field: StepField, tree: StepTree, textmap: Map[LocalStepId, StepText]) extends ChangeResponder[StepFieldValue] {

  assume(textmap.keySet == tree.mapRecursive(_.id).toSet, "There must be a StepText for all steps.")

  override def respondToChange(c: Change)(implicit ctx: UcParsingCtx): ChangeResult[StepFieldValue, Change] =
    respondToChangeByDelegation(c)
    .andThen(this, _.respondToChangeInternally(c))

  private def respondToChangeInternally(c: Change)(implicit ctx: UcParsingCtx): ChangeResult[StepFieldValue, Change] = {
    def allowTitleChange_? = field.preferTitleInRoot_? && tree.nonEmpty
    def changeRootToTitle(before: String, after: String) = {
      val id = tree(0).id
      val lens = alens(Lenses.sfvStepTextInstL, (this, id))
      val t = lens.get
      val curText = t.mainClause.text
      if (curText.isEmpty || curText == before)
        StepTextUpdater(field, id, t).updateMainClause(after).mapValue(lens.set)
      else
        NoChange
    }
    c match {
      case TitleChanged(before, after) if allowTitleChange_? => changeRootToTitle(before, after)
      case _ => NoChange
    }
  }

  private def respondToChangeByDelegation(c: Change)(implicit ctx: UcParsingCtx): ChangeResult[StepFieldValue, Change] = {
    // Delegate msg to all StepText instances
    var newTextmap = textmap
    var changes = List.empty[Change]
    for ((id, curVal) <- textmap)
      StepTextUpdater(field, id, curVal).respondToChange(c) match {
        case Changed(newVal, h) =>
          newTextmap += (id -> newVal)
          changes ++= h.list
        case NoChange =>
      }

    // Copy if changed
    ChangeResult(copy(textmap = newTextmap), changes)
  }

  def getNormalisedText(id: LocalStepId)(implicit savedSteps: SavedSteps): NormalisedText =
    textmap.get(id).map(_.normalisedText).getOrElse("".tag[IsNormalised])

  def withNewStep(newTree: StepTree, stepId: LocalStepId) =
    copy(tree = newTree, textmap = textmap + (stepId -> StepText.empty))

  /**
   * Makes a copy with a new tree. New steps will have empty text added to the textmap; old, removed.
   */
  def withNewTree(newTree: StepTree) = {
    var newTextmap = Map.empty[LocalStepId, StepText]
    for (n <- newTree) newTextmap += (n.id -> textmap.get(n.id).getOrElse(StepText.empty))
    copy(tree = newTree, textmap = newTextmap)
  }

  def textByLabels(implicit stepsAndLabels: StepAndLabelBiMap): Map[StepLabel, String] =
    for ((id,t) <- textmap) yield (stepsAndLabels.value.ab(id), t.text)
}
