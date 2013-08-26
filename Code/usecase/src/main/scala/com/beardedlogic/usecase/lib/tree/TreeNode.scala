package com.beardedlogic.usecase.lib
package tree

import Types._

/**
 * Builds on `TreeNodeLike` to add a node ID, and hierarchical position details.
 *
 * @since 3/06/2013
 */
trait TreeNode[T <: TreeNode[T]] extends TreeNodeLike[T] {
  self: T =>

  val id: LocalStepId
  val level: Int
  val labelIndex: Int

  def label: LabelStr

  def copy(id: LocalStepId = this.id,
    level: Int = this.level,
    labelIndex: Int = this.labelIndex,
    children: List[T] = this.children
    ): T

  /**
   * Increments the position of this node.
   *
   * Examples:
   * 1.0.1      --> 1.0.2
   * 1.3.a.iii  --> 1.3.a.iv
   */
  @inline final def incrementPosition() = copy(labelIndex = this.labelIndex + 1)

  /**
   * Decrements the position of this node.
   *
   * Examples:
   * 1.0.2      --> 1.0.1
   * 1.3.a.iv   --> 1.3.a.iii
   */
  @inline final def decrementPosition() = copy(labelIndex = this.labelIndex - 1)
}

/**
 * Interface for a function that can be used to create new tree nodes.
 * @tparam T The type of tree node.
 */
trait TreeNodeBuilder[T <: TreeNode[T]] {

  def apply(
    level: Int,
    labelIndex: Int,
    children: List[T]
    ): T
}
