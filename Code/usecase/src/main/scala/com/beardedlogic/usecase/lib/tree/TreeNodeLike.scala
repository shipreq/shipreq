package com.beardedlogic.usecase.lib.tree

/**
 * Anything that has a list of children of itself can be considered treenode-like. This adds normal collection-esque
 * support to tree nodes so that typical functions such as foreach, map, etc. are available.
 *
 * @since 1/06/2013
 */
trait TreeNodeLike[T <: TreeNodeLike[T]] extends Traversable[T] {
  self: T =>

  val children: List[T]

  def apply(childIndex: Int) = children(childIndex)

  override def foreach[U](fn: T => U) {
    fn(this)
    children.foreach(_.foreach(fn))
  }

  def deepCopy(fn: (T, List[T]) => T): T = fn(this, deepCopyChildren(fn))
  @inline final def deepCopyChildren(fn: (T, List[T]) => T) = children.map { _.deepCopy(fn) }
}
