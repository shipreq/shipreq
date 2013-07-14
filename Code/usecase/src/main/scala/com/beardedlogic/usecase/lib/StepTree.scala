package com.beardedlogic.usecase.lib

import net.liftweb.util.Helpers.nextFuncName
import TypeTags._
import tree._
import StepLabels.LabelMakers

/**
 * A full tree of steps.
 *
 * @param nodes The top-level steps.
 */
case class StepTree(override val nodes: List[StepNode]) extends TreeRoot[StepNode]

object StepTree {
  final val empty = apply(Nil)
}

/**
 * A step with a 0-n list of child steps.
 */
case class StepNode(id: LocalIdStr, level: Int, labelIndex: Int, children: List[StepNode] = Nil) extends TreeNode[StepNode] {

  // TODO Remove tight label maker coupling from StepNode
  // TODO Disable in prod
  require(level >= 0, s"Level (${level}) must be 0 or larger.")
//  require(level < LabelMakers.size, s"Level (${level}) must be less than ${LabelMakers.size}.")
//  require(labelIndex >= labelMaker.min, s"Label index (${labelIndex}) at level (${level}) must be ${labelMaker.min} or larger.")

  @inline final def labelMaker = LabelMakers(level)
  override final def label = labelMaker(labelIndex)

  // Manually specify else it will recurse forever because this is Traversable
  override def toString = s"StepNode($id, $level.$labelIndex, $children)"

  override def copy(id: LocalIdStr = this.id,
    level: Int = this.level,
    labelIndex: Int = this.labelIndex,
    children: List[StepNode] = this.children
    ) = StepNode(id, level, labelIndex, children)

  @inline final def labelId = id + "-l"
  @inline final def stepTextId = id + "-t"
}

object StepNodeBuilder extends TreeNodeBuilder[StepNode] {
  def apply(level: Int, labelIndex: Int, children: List[StepNode] = Nil) =
    StepNode(nextFuncName.asLocalId, level, labelIndex, children)
}
