package com.beardedlogic.usecase.lib.change

import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.{StepTree, StepNode}

object Changes {

  /**
   * Indicates a change in the use case title.
   *
   * @param before The title pre-change.
   * @param after The title post-change.
   */
  case class TitleChanged(before: String, after: String) extends Change

  /**
   * Indicates that a text field's value has changed.
   *
   * No contextual info required because TextFields only have a single text value, and changes are automatically
   * paired to change-source (ie. the TextField).
   */
  case object TextChanged extends Change

  /**
   * Indicates that a step's text has changed.
   *
   * @param id The ID of the step node.
   */
  case class StepTextChanged(id: LocalIdStr) extends Change

  /**
   * Indicates that the label of one or more existing steps, has changed.
   *
   * Consequentially, any step references will need to be verified and possibly updated.
   */
  sealed trait ExistingStepLabelsChanged extends Change

  case class TailStepAdded(node: StepNode) extends Change

  case class StepAdded(precedingNodeId: LocalIdStr, node: StepNode) extends ExistingStepLabelsChanged

  case class StepRemoved(node: StepNode) extends ExistingStepLabelsChanged

  case class StepIndentIncreased(node: StepNode, oldTree: StepTree) extends ExistingStepLabelsChanged

  case class StepIndentDecreased(node: StepNode, oldTree: StepTree) extends ExistingStepLabelsChanged

  /**
   * Indicates that a step's flow-from list has changed.
   *
   * Example: If the text of step 1.7 changes from `"Blah"` or `"Blah ⬅ 1.0.2"` to `"Blah ⬅ 1.3, 1.4"`
   * then this message will be broadcast:
   * {{{
   * FlowToChange( [1.3, 1.4], 1.7 )
   * }}}
   *
   * @param fromIds The IDs of all steps that now flow to the target.
   * @param toId The ID of the step that issued the change, the step to which the from-steps now flow.
   */
  case class FlowFromChange(fromIds: Set[LocalIdStr], toId: LocalIdStr) extends Change

  /**
   * Indicates that a step's flow-to list has changed.
   *
   * Example: If the text of step 1.7 changes from `"Blah"` or `"Blah ➡ 1.0.2"` to `"Blah ➡ 1.3, 1.4"`
   * then this message will be broadcast:
   * {{{
   * FlowToChange( 1.7, [1.3, 1.4] )
   * }}}
   *
   * @param fromId The ID of the step that issued the change, the step from which steps now flow out.
   * @param toIds The IDs of all steps that the source step now flows to.
   */
  case class FlowToChange(fromId: LocalIdStr, toIds: Set[LocalIdStr]) extends Change

}
