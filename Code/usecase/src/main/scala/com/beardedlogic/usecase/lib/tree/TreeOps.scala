package com.beardedlogic.usecase.lib
package tree

import scala.annotation.tailrec
import Types._

/**
 * @since 2/05/2013
 */
object TreeOps {

  implicit def TreeRootToList[N <: TreeNodeLike[N]](tree: TreeRoot[N]): List[N] = tree.nodes
  implicit def TreeNodeToList[N <: TreeNodeLike[N]](tree: TreeNodeLike[N]): List[N] = tree.children

  /**
   * Generates a map of node IDs to labels.
   *
   * Example:
   * FD93E    -> 1.E.1
   * F93A3    -> 1.E.1.a
   */
  def mapIdsToFullLabels[T <: TreeNode[T]](nodes: List[T], prefix: String = ""): Map[LocalStepId, StepLabel] = nodes match {
    case h :: t =>
      val lbl = prefix + h.label
      mapIdsToFullLabels(t, prefix) ++
        mapIdsToFullLabels(h.children, lbl + ".") +
        (h.id -> lbl.asLabel)

    case Nil => Map.empty
  }

  /**
   * Inserts a step (not a StepNode) into a node tree.
   *
   * Does not insert at the top level. If afterId points to 1.0, this will create 1.0.1 and not 2.0.
   *
   * @param afterId The ID of the node that should precede the new step.
   * @param nodes The node tree before insertion.
   * @return A tuple of 1) the new tree, 2) the new node (if inserted)
   */
  @inline def stepInsert[T <: TreeNode[T]](
    afterId: LocalStepId,
    nodes: List[T],
    nodeBuilder: TreeNodeBuilder[T]): (List[T], Option[T]) = {

    @tailrec def iter(
      nodes: List[T],
      results: List[T],
      resultNode: Option[T]): (List[T], Option[T]) = nodes match {
      case Nil => (results, resultNode)

      // Found.
      case h :: t if h.id == afterId =>
        if (h.level == 0 || h.children.nonEmpty) {
          // Add to found node's children
          val n = nodeBuilder(h.level + 1, 1, Nil)
          val c = n :: h.children.map(_.incrementPosition)
          (results ::: h.copy(children = c) :: t, Some(n))
        } else {
          // Add at the same level
          val n = nodeBuilder(h.level, h.labelIndex + 1, Nil)
          (results ::: h :: n :: t.map(_.incrementPosition), Some(n))
        }

      // Not found. Check children then siblings.
      case h :: t => stepInsert(afterId, h.children, nodeBuilder) match {
        case (c, n@Some(_)) => (results ::: h.copy(children = c) :: t, n)
        case _              => iter(t, results :+ h, None)
      }
    }

    iter(nodes, Nil, None)
  }

  /**
   * Removes a step and its children from a node tree.
   *
   * @return A tuple of 1) the new tree, 2) whether any changes were made.
   */
  @inline def stepRemove[T <: TreeNode[T]](id: LocalStepId, nodes: List[T]): (List[T], Option[T]) =
    _stepRemove(id, nodes, Nil, None)

  @tailrec private def _stepRemove[T <: TreeNode[T]](
    id: LocalStepId,
    nodes: List[T],
    results: List[T],
    found: Option[T]): (List[T], Option[T]) = nodes match {
    case Nil => (results, found)

    // Found.
    case h :: t if h.id == id =>
      (results ::: t.map(_.decrementPosition), Some(h))

    // Not found. Check children then siblings.
    case h :: t => stepRemove(id, h.children) match {
      case (newChildren, f@Some(_)) => (results ::: h.copy(children = newChildren) :: t, f)
      case _                        => _stepRemove(id, t, results :+ h, None)
    }
  }

  /**
   * Decreases the indent/level of a node in a tree.
   *
   * Examples:
   * 1.0.2.a      --> 1.0.3
   * 1.3.4.b.iii  --> 1.3.4.c
   *
   * @return A tuple of 1) the new tree, 2) the new node (if any change was made).
   */
  @inline def indentDecrease[T <: TreeNode[T]](id: LocalStepId, nodes: List[T]) = _indentDecrease(id, nodes, Nil, None)

  @tailrec private def _indentDecrease[T <: TreeNode[T]](
    id: LocalStepId,
    nodes: List[T],
    results: List[T],
    newNode: Option[T]): (List[T], Option[T]) = nodes match {
    case Nil    => (results, newNode)
    case h :: t => findChild(id, h.children) match {
      case Some(ChildAndSiblings(sibLeft, c, sibRight)) => // Found match in head node's children

        // Build node's new children
        var childIndex = 0
        val childLevel = h.level + 1
        val levelChangeFn = levelChange[T](-1)
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
        case (childrenResults, n@Some(_)) => (results ::: h.copy(children = childrenResults) :: t, n)
        case _                            => _indentDecrease(id, t, results :+ h, None)
      }
    }
  }

  private case class ChildAndSiblings[T <: TreeNode[T]](
    siblingsLeft: List[T],
    child: T,
    siblingsRight: List[T])

  /**
   * Searches a list (non-recursively) for a particular node, and if found, returns it with its left and right
   * siblings.
   */
  @tailrec private def findChild[T <: TreeNode[T]](
    id: LocalStepId,
    nodes: List[T],
    siblingsLeft: List[T] = Nil): Option[ChildAndSiblings[T]] = nodes match {
    case Nil                  => None
    case h :: t if h.id == id => Some(ChildAndSiblings(siblingsLeft, h, t))
    case h :: t               => findChild(id, t, siblingsLeft :+ h)
  }

  /**
   * Increases the indent/level of a node in a tree.
   *
   * Examples:
   * 1.0.2      --> 1.0.1.a
   * 1.3.4.b    --> 1.3.4.a.ii
   *
   * @return A tuple of 1) the new tree, 2) the new node (if any change was made).
   */
  @inline def indentIncrease[T <: TreeNode[T]](id: LocalStepId, nodes: List[T]) = _indentIncrease(id, None, Nil, nodes)

  @tailrec private def _indentIncrease[T <: TreeNode[T]](
    id: LocalStepId,
    newNode: Option[T],
    results: List[T],
    nodes: List[T]): (List[T], Option[T]) = nodes match {
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
      case (childrenResults, n@Some(_)) => (results ::: h.copy(children = childrenResults) :: t, n)
      case _                            => _indentIncrease(id, None, results :+ h, t)
    }
  }

  private def levelChange[T <: TreeNode[T]](offset: Int) = (n: T, ch: List[T]) => {
    n.copy(children = ch, level = n.level + offset)
  }

  /**
   * Converts a tree of type-A nodes, into a tree of type-B nodes, maintaining the tree structure.
   * @param input A list of type-A nodes.
   * @param fn The function to convert a type-A to a type-B node.
   *           It should take arguments: node, level, labelIndex, children.
   * @return A list of type-B nodes.
   */
  def convertNodeTree[A <: TreeNodeLike[A], B <: TreeNodeLike[B]](
    input: List[A],
    fn: (A, Int, Int, List[B]) => B,
    startingIndexForLevel: Int => Int,
    level: Int = 0): List[B] = {

    @tailrec def iter(input: List[A], level: Int, labelIndex: Int, results: List[B]): List[B] = input match {
      case Nil    => results
      case h :: t =>
        val children = convertNodeTree(h.children, fn, startingIndexForLevel, level + 1)
        val node = fn(h, level, labelIndex, children)
        iter(t, level, labelIndex + 1, results :+ node)
    }

    iter(input, level, startingIndexForLevel(level), Nil)
  }
}
