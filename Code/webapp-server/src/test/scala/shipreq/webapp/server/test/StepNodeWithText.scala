package shipreq.webapp.server
package test

import shipreq.webapp.server.lib.Types.{StepLabel, LocalStepId}
import feature.uc.step.{StepNode, TreeNode}
import shipreq.webapp.base.data.StaticField.NormalAltStepTree.stepLabelsPerLevel

case class StepNodeWithText(
  id: LocalStepId,
  level: Int,
  labelIndex: Int,
  text: String,
  children: List[StepNodeWithText] = Nil) extends TreeNode[StepNodeWithText] {

  override def copy(id: LocalStepId = this.id,
    level: Int = this.level,
    labelIndex: Int = this.labelIndex,
    children: List[StepNodeWithText] = this.children
    ) = StepNodeWithText(id, level, labelIndex, text, children)

  @inline final def labelMaker = stepLabelsPerLevel(level)
  override final def label = StepLabel(labelMaker.labelTmp(labelIndex))

  // Manually specify else it will recurse forever because this is Traversable
  override def toString = s"StepNodeWithText($id, $level.$labelIndex, $text, $children)"

  def toStepNode: StepNode = StepNode(id, level, labelIndex, children.map(_.toStepNode))
}

