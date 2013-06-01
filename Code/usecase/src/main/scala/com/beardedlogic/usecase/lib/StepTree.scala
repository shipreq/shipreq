package com.beardedlogic.usecase.lib

import net.liftweb.util.Helpers._
import scala.annotation.tailrec
import TypeTags._

/**
 * @since 2/05/2013
 */
object StepTree {

  /**
   * Anything that has a list of children of itself can be considered a tree node. This adds normal collection-esque
   * support to tree nodes so that typical functions such as foreach, map, etc. are available.
   *
   * @since 1/06/2013
   */
  trait TreeNodeLike[T <: TreeNodeLike[T]] extends Traversable[T] {
    self: T =>
    val children: List[T]

    override def foreach[U](fn: T => U) {
      fn(this)
      children.foreach(_.foreach(fn))
    }
  }

  implicit class TreeNodeListExt[T <: TreeNodeLike[T]](val tree: List[T]) extends AnyVal {
    def foreachNode[U](fn: T => U) { tree.foreach(_.foreach(fn)) }
    def mapEachNode[R](fn: T => R): List[R] = tree.flatMap(_.map(fn))
  }

  // TODO Step.text not being used. Maybe Step itself is useless. Step node ids and an external map probably better.
  case class Step(text: String)

  case class StepNode(id: String @@ LocalStepId,
                      level: Int,
                      labelIndex: Int,
                      step: Step,
                      children: List[StepNode] = Nil)
    extends TreeNodeLike[StepNode] {

    import StepLabels.LabelMakers
    @inline def labelMaker = LabelMakers(level)

    require(level >= 0, s"Level (${level}) must be 0 or larger.")
    require(level < LabelMakers.size, s"Level (${level}) must be less than ${LabelMakers.size}.")
    require(labelIndex >= labelMaker.min, s"Label index (${labelIndex}) at level (${level}) must be ${labelMaker.min} or larger.")

    @inline def label = labelMaker(labelIndex)

    // Manually specify else it will recurse forever because this is Traversable
    override def toString = s"StepNode($id, $level.$labelIndex, $step, $children)"

    /**
     * Increments the position of this node.
     *
     * Examples:
     *   1.0.1      --> 1.0.2
     *   1.3.a.iii  --> 1.3.a.iv
     */
    @inline def incrementPosition() = copy(labelIndex = this.labelIndex + 1)

    /**
     * Decrements the position of this node.
     *
     * Examples:
     *   1.0.2      --> 1.0.1
     *   1.3.a.iv   --> 1.3.a.iii
     */
    @inline def decrementPosition() = copy(labelIndex = this.labelIndex - 1)

    @inline def labelId = id + "-l"
    @inline def stepTextId = id + "-t"

    def apply(childIndex: Int) = children(childIndex)

    def deepCopy(fn: (StepNode, List[StepNode]) => StepNode): StepNode = fn(this, deepCopyChildren(fn))
    @inline def deepCopyChildren(fn: (StepNode, List[StepNode]) => StepNode) = children.map { _.deepCopy(fn) }
  }

  def NewStep = Step("")

  /**
   * Generates a map of both node IDs to labels, and labels to node IDs.
   *
   * Example:
   *   1.E.1    -> FD93E
   *   1.E.1.a  -> F93A3
   *   FD93E    -> 1.E.1
   *   F93A3    -> 1.E.1.a
   */
  def mapIdsAndFullLabels(nodes: List[StepNode], prefix: String = ""): Map[String, String] = nodes match {
    case h :: t =>
      val lbl = prefix + h.label
      mapIdsAndFullLabels(t, prefix) ++
        mapIdsAndFullLabels(h.children, lbl + ".") +
        (h.id -> lbl) +
        (lbl -> h.id)

    case Nil => Map.empty
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
  @inline def stepInsert(step: Step, afterId: String @@ LocalStepId, nodes: List[StepNode]): Tuple2[List[StepNode], Option[StepNode]] =
    _stepInsert(step, afterId, nodes, Nil, None)

  @tailrec private def _stepInsert(
    step: Step,
    afterId: String @@ LocalStepId,
    nodes: List[StepNode],
    results: List[StepNode],
    resultNode: Option[StepNode]): Tuple2[List[StepNode], Option[StepNode]] = nodes match {

    case Nil => (results, resultNode)

    // Found.
    case h :: t if h.id == afterId =>
      if (h.level == 0 || h.children.nonEmpty) {
        // Add to found node's children
        val n = StepNode(newLocalStepId, h.level + 1, 1, step)
        val c = n :: h.children.map(_.incrementPosition)
        (results ::: h.copy(children = c) :: t, Some(n))
      } else {
        // Add at the same level
        val n = StepNode(newLocalStepId, h.level, h.labelIndex + 1, step, Nil)
        (results ::: h :: n :: t.map(_.incrementPosition), Some(n))
      }

    // Not found. Check children then siblings.
    case h :: t => stepInsert(step, afterId, h.children) match {
      case (c, n @ Some(_)) => (results ::: h.copy(children = c) :: t, n)
      case _                => _stepInsert(step, afterId, t, results :+ h, None)
    }
  }

  @inline final def newLocalStepId = nextFuncName.asLocalStepId

  /**
   * Removes a step and its children from a node tree.
   *
   * @return A tuple of 1) the new tree, 2) whether any changes were made.
   */
  @inline def stepRemove(id: String @@ LocalStepId, nodes: List[StepNode]): Tuple2[List[StepNode], Option[StepNode]] =
    _stepRemove(id, nodes, Nil, None)

  @tailrec private def _stepRemove(
    id: String @@ LocalStepId,
    nodes: List[StepNode],
    results: List[StepNode],
    found: Option[StepNode]): Tuple2[List[StepNode], Option[StepNode]] = nodes match {
    case Nil => (results, found)

    // Found.
    case h :: t if h.id == id =>
      (results ::: t.map(_.decrementPosition), Some(h))

    // Not found. Check children then siblings.
    case h :: t => stepRemove(id, h.children) match {
      case (newChildren, f @ Some(_)) => (results ::: h.copy(children = newChildren) :: t, f)
      case _                          => _stepRemove(id, t, results :+ h, None)
    }
  }

  /**
   * Decreases the indent/level of a node in a tree.
   *
   * Examples:
   *   1.0.2.a      --> 1.0.3
   *   1.3.4.b.iii  --> 1.3.4.c
   *
   * @return A tuple of 1) the new tree, 2) the new node (if any change was made).
   */
  @inline def indentDecrease(id: String @@ LocalStepId, nodes: List[StepNode]) = _indentDecrease(id, nodes, Nil, None)

  @tailrec private def _indentDecrease(
    id: String @@ LocalStepId,
    nodes: List[StepNode],
    results: List[StepNode],
    newNode: Option[StepNode]): Tuple2[List[StepNode], Option[StepNode]] = nodes match {
    case Nil => (results, newNode)
    case h :: t => findChild(id, h.children) match {
      case Some(ChildAndSiblings(sibLeft, c, sibRight)) => // Found match in head node's children

        // Build node's new children
        var childIndex = 0
        val childLevel = h.level + 1
        val levelChangeFn = levelChange(-1)
        val cL = c.children.map { n =>
          childIndex += 1
          n.copy(level = childLevel, labelIndex = childIndex, children = n.deepCopyChildren(levelChangeFn))
        }
        val newChildren = cL ::: sibRight.map { n =>
          childIndex += 1
          n.copy(level = childLevel, labelIndex = childIndex)
        }

        // Build final list
        val newChild = c.copy(
          level = c.level - 1,
          labelIndex = h.labelIndex + 1,
          children = newChildren)
        val newResults =
          results :::
            h.copy(children = sibLeft) ::
            newChild ::
            t.map(_.incrementPosition)
        (newResults, Some(newChild))

      // Not found. Check children then siblings.
      case None => indentDecrease(id, h.children) match {
        case (childrenResults, n @ Some(_)) => (results ::: h.copy(children = childrenResults) :: t, n)
        case _                              => _indentDecrease(id, t, results :+ h, None)
      }
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
    id: String @@ LocalStepId,
    nodes: List[StepNode],
    siblingsLeft: List[StepNode] = Nil): Option[ChildAndSiblings] = nodes match {
    case Nil                  => None
    case h :: t if h.id == id => Some(ChildAndSiblings(siblingsLeft, h, t))
    case h :: t               => findChild(id, t, siblingsLeft :+ h)
  }

  /**
   * Increases the indent/level of a node in a tree.
   *
   * Examples:
   *   1.0.2      --> 1.0.1.a
   *   1.3.4.b    --> 1.3.4.a.ii
   *
   * @return A tuple of 1) the new tree, 2) the new node (if any change was made).
   */
  @inline def indentIncrease(id: String @@ LocalStepId, nodes: List[StepNode]) = _indentIncrease(id, None, Nil, nodes)

  @tailrec private def _indentIncrease(
    id: String @@ LocalStepId,
    newNode: Option[StepNode],
    results: List[StepNode],
    nodes: List[StepNode]): Tuple2[List[StepNode], Option[StepNode]] = nodes match {
    case Nil => (results, newNode)

    case p :: c :: t if c.id == id =>
      val c2 = c.copy(
        level = c.level + 1,
        labelIndex = p.children.size + 1,
        children = c.deepCopyChildren(levelChange(1))
      )
      val p2 = p.copy(children = p.children :+ c2)
      (results ::: p2 :: t.map(_.decrementPosition), Some(c2))

    // Not found. Check children then siblings.
    case h :: t => indentIncrease(id, h.children) match {
      case (childrenResults, n @ Some(_)) => (results ::: h.copy(children = childrenResults) :: t, n)
      case _                              => _indentIncrease(id, None, results :+ h, t)
    }
  }

  private def levelChange(offset: Int) = (n: StepNode, ch: List[StepNode]) => {
    n.copy(children = ch, level = n.level + offset)
  }
}