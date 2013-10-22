package com.beardedlogic.usecase.feature.uc
package change

import scalaz.NonEmptyList
import com.beardedlogic.usecase.lib.Types._
import field.{StepField, TextField}
import step.{StepTree, StepNode}

sealed trait Change {
  val asOnlyChange = NonEmptyList(this)
  def @:[V](newValue: V) = Changed(newValue, asOnlyChange)
}

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
  case class TextChanged(f: TextField) extends Change

  /**
   * Indicates that a step's text has changed.
   *
   * @param id The ID of the step node.
   */
  case class StepTextChanged(f: StepField, id: LocalStepId) extends Change

  /**
   * Indicates that the label of one or more existing steps, has changed.
   *
   * Consequentially, any step references will need to be verified and possibly updated.
   */
  sealed trait ExistingStepLabelsChanged extends Change

  case class TailStepAdded(f: StepField, node: StepNode) extends Change

  case class StepAdded(f: StepField, precedingNodeId: LocalStepId, node: StepNode) extends ExistingStepLabelsChanged

  case class StepRemoved(f: StepField, node: StepNode) extends ExistingStepLabelsChanged

  case class StepIndentIncreased(f: StepField, node: StepNode, oldTree: StepTree) extends ExistingStepLabelsChanged

  case class StepIndentDecreased(f: StepField, node: StepNode, oldTree: StepTree) extends ExistingStepLabelsChanged

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
  case class FlowFromChange(fromIds: Set[LocalStepId], toId: LocalStepId) extends Change

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
  case class FlowToChange(fromId: LocalStepId, toIds: Set[LocalStepId]) extends Change

}
