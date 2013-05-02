package com.beardedlogic.usecase.lib

import net.liftweb.util.Helpers._
import scala.annotation.tailrec

/**
 * @since 2/05/2013
 */
object StepTree {

  case class Step(text: String)

  case class StepNode(id: String,
                      level: Int,
                      label: String,
                      step: Step,
                      children: List[StepNode]) {
    def labelId = id + "-l"
    def stepTextId = id + "-t"
  }

  def NewStep = Step("")

  /**
   * Flattens a list of step nodes with children, into a single list that contains all recursive contents.
   */
  @tailrec def flattenNodes(nodes: List[StepNode], results: List[StepNode] = Nil): List[StepNode] = nodes match {
    case Nil    => results
    case h :: t => flattenNodes(h.children ::: t, results :+ h)
  }

  /**
   * Increments the position of a single node.
   *
   * Examples:
   *   1.0.1      --> 1.0.2
   *   1.3.a.iii  --> 1.3.a.iv
   *
   * NOTE: Top-level elements are not allowed and will cause an error.
   */
  def incrementPosition(n: StepNode) = {
    require(n.level > 0, "Top-level nodes not allowed.")
    val lm = StepLabels.LABEL_MAKERS(n.level - 1)
    val newLabel = lm(lm(n.label) + 1)
    n.copy(label = newLabel)
  }

  /**
   * Inserts a step (not a StepNode) into a node tree.
   *
   * Does not insert at the top level. If afterId points to 1.0, this will create 1.0.1 and not 2.0.
   *
   * @param step The step to insert.
   * @param afterId The ID of the node that should precede the new step.
   * @param nodes The node tree before insertion.
   * @return A tuple of 1) the new tree, 2) the new node (if inserted)
   */
  def insertStep(
    step: Step,
    afterId: String,
    nodes: List[StepNode],
    results: List[StepNode] = Nil,
    resultNode: Option[StepNode] = None): Tuple2[List[StepNode], Option[StepNode]] = nodes match {

    case Nil => (results, resultNode)

    // Found at top-level, add as first child
    case h :: t if h.id == afterId && h.level == 0 =>
      val n = StepNode(nextFuncName, h.level + 1, StepLabels.LABEL_MAKERS(0)(1), step, Nil)
      val c = n :: h.children.map(incrementPosition _)
      (results ::: h.copy(children = c) :: t, Some(n))

    // Found, add after
    case h :: t if h.id == afterId =>
      val n = StepNode(nextFuncName, h.level, h.label, step, Nil)
      (results ::: h :: (n :: t).map(incrementPosition _), Some(n))

    case h :: t =>
      val (c, n) = insertStep(step, afterId, h.children, Nil, resultNode)
      if (n.isDefined)
        // Found in children
        (results ::: h.copy(children = c) :: t, n)
      else
        // Keep searching
        insertStep(step, afterId, t, results :+ h, None)
  }
}