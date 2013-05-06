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
                      labelPrefix: Option[String],
                      labelIndex: Int,
                      step: Step,
                      children: List[StepNode]) {

    def this(id: String,
             level: Int,
             labelIndex: Int,
             step: Step,
             children: List[StepNode] = Nil) = this(id, level, None, labelIndex, step, children)

    def this(id: String,
             label: Tuple2[String, Int],
             step: Step,
             children: List[StepNode] = Nil) = this(id, 0, Some(label._1), label._2, step, children)

    import StepLabels.LABEL_MAKERS
    @inline def labelMaker = LABEL_MAKERS(level)

    require(level >= 0, s"Level (${level}) must be 0 or larger.")
    require(level < LABEL_MAKERS.size, s"Level (${level}) must be less than ${LABEL_MAKERS.size}.")
    require(labelIndex >= labelMaker.min, s"Label index (${labelIndex}) at level (${level}) must be ${labelMaker.min} or larger.")

    @inline def labelSuffix = labelMaker(labelIndex)
    val label = labelPrefix map (_ + labelSuffix) getOrElse labelSuffix

    /**
     * Increments the position of a single node.
     *
     * Examples:
     *   1.0.1      --> 1.0.2
     *   1.3.a.iii  --> 1.3.a.iv
     */
    @inline def incrementPosition() = copy(labelIndex = this.labelIndex + 1)

    @inline def labelId = id + "-l"
    @inline def stepTextId = id + "-t"
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

    // Found and has children. Add as first child.
    case h :: t if h.id == afterId && !h.children.isEmpty =>
      val n = new StepNode(nextFuncName, h.level + 1, 1, step)
      val c = n :: h.children.map(_.incrementPosition)
      (results ::: h.copy(children = c) :: t, Some(n))

    // Found. Add after.
    case h :: t if h.id == afterId =>
      val n = new StepNode(nextFuncName, h.level, h.labelIndex + 1, step)
      (results ::: h :: n :: t.map(_.incrementPosition), Some(n))

    // Not found. Check children then siblings.
    case h :: t =>
      val (c, n) = insertStep(step, afterId, h.children, Nil, resultNode)
      if (n.isDefined)
        (results ::: h.copy(children = c) :: t, n)
      else
        insertStep(step, afterId, t, results :+ h, None)
  }

  /**
   * Decreases the indent/level of a node in a tree.
   *
   * Examples:
   *   1.0.2.a      --> 1.0.3
   *   1.3.4.b.iii  --> 1.3.4.c
   */
  @inline def indentDecrease(id: String, nodes: List[StepNode]) = _indentDecrease(id, nodes, Nil, false)

  @tailrec private def _indentDecrease(id: String, nodes: List[StepNode], results: List[StepNode], found: Boolean): Tuple2[List[StepNode], Boolean] = nodes match {
    case Nil => (results, found)
    case h :: t => findChild(id, h.children) match {
      case Some(ChildAndSiblings(sibLeft, c, sibRight)) => // Found match in head node's children

        // Build node's new children
        var childIndex = 0
        val childLevel = h.level + 1
        val childLabelPrefix = c.children.headOption.map(_.labelPrefix) getOrElse None
        val newChildren = (c.children ::: sibRight).map { n =>
          childIndex += 1
          n.copy(level = childLevel, labelPrefix = childLabelPrefix, labelIndex = childIndex)
        }

        // Build final list
        val r =
          results :::
            h.copy(children = sibLeft) ::
            c.copy(level = c.level - 1, labelPrefix = h.labelPrefix, labelIndex = h.labelIndex + 1, children = newChildren) ::
            t.map(_.incrementPosition)
        (r, true)

      case None =>
        val (childrenResults, childrenFound) = indentDecrease(id, h.children)
        if (childrenFound)
          (results ::: h.copy(children = childrenResults) :: t, true)
        else
          _indentDecrease(id, t, results :+ h, found)
    }
  }

  private case class ChildAndSiblings(
    siblingsLeft: List[StepNode],
    child: StepNode,
    siblingsRight: List[StepNode])

  /**
   * Searches a list (non-recursively) for a particular node, and if found, returns it with its left and right
   * siblings.
   */
  @tailrec private def findChild(
    id: String,
    nodes: List[StepNode],
    siblingsLeft: List[StepNode] = Nil): Option[ChildAndSiblings] = nodes match {
    case Nil                  => None
    case h :: t if h.id == id => Some(ChildAndSiblings(siblingsLeft, h, t))
    case h :: t               => findChild(id, t, siblingsLeft :+ h)
  }
}