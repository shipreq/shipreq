package com.beardedlogic.usecase
package test

import lib._
import TypeTags._
import tree.TreeNode
import StepLabels._

case class StepNodeWithText(
  id: String @@ LocalStepId,
  level: Int,
  labelIndex: Int,
  text: String,
  children: List[StepNodeWithText] = Nil) extends TreeNode[StepNodeWithText] {

  override def copy(id: String @@ LocalStepId = this.id,
    level: Int = this.level,
    labelIndex: Int = this.labelIndex,
    children: List[StepNodeWithText] = this.children
    ) = StepNodeWithText(id, level, labelIndex, text, children)

  @inline final def labelMaker = LabelMakers(level)
  override final def label = labelMaker(labelIndex)

  // Manually specify else it will recurse forever because this is Traversable
  override def toString = s"StepNodeWithText($id, $level.$labelIndex, $text, $children)"

  def toStepNode: StepNode = StepNode(id, level, labelIndex, children.map(_.toStepNode))
}

