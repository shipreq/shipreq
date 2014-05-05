package shipreq.webapp.feature.uc.field

import shipreq.webapp.lib.Types._
import shipreq.webapp.feature.uc.{SavedSteps, StepAndLabelBiMap, UcParsingCtx, Lenses}
import shipreq.webapp.feature.uc.change._
import shipreq.webapp.feature.uc.step.StepTree
import shipreq.webapp.feature.uc.step.TreeOps._
import shipreq.webapp.feature.uc.text.{StepTextUpdater, StepText}
import shipreq.webapp.util.AppliedLens
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
case class StepFieldValue(field: StepField, tree: StepTree, textmap: Map[LocalStepId, StepText]) {

  assume(textmap.keySet == tree.mapRecursive(_.id).toSet, "There must be a StepText for all steps.")

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

// =====================================================================================================================

class StepFieldValueChangeResponder(field: StepField) extends SeqChangeResponder[StepFieldValue] {

  override def respondToChange(sfv: StepFieldValue, c: Change)(implicit ctx: UcParsingCtx) =
    respondToChangeByDelegation(sfv, c)
    .andThen(sfv, respondToChangeInternally(_, c))

  private def respondToChangeInternally(sfv: StepFieldValue, c: Change)(implicit ctx: UcParsingCtx): R = {

    def allowTitleChange_? =
      field.preferTitleInRoot_? && sfv.tree.nonEmpty

    def changeRootToTitle(oldTitle: String, newTitle: String) = {
      import NormalCourseFieldConsts.titleToMainClause
      val id = sfv.tree(0).id
      val lens = AppliedLens(Lenses.sfvStepTextInstL, (sfv, id))
      val step = lens.get
      val curMainClause = step.mainClause.text
      if (curMainClause.isEmpty || curMainClause == titleToMainClause(oldTitle))
        new StepTextUpdater(field, id).updateMainClause(step, titleToMainClause(newTitle)).mapValue(lens.set)
      else
        NoChange
    }
    c match {
      case TitleChanged(before, after) if allowTitleChange_? => changeRootToTitle(before, after)
      case _ => NoChange
    }
  }

  private def respondToChangeByDelegation(sfv: StepFieldValue, c: Change)(implicit ctx: UcParsingCtx): R = {
    // Delegate msg to all StepText instances
    var newTextmap = sfv.textmap
    var changes = List.empty[Change]
    for ((id, curVal) <- sfv.textmap)
      new StepTextUpdater(field, id).respondToChange(curVal, c) match {
        case Changed(newVal, h) =>
          newTextmap += (id -> newVal)
          changes ++= h.list
        case NoChange =>
      }

    // Copy if changed
    ChangeResult(sfv.copy(textmap = newTextmap), changes)
  }
}