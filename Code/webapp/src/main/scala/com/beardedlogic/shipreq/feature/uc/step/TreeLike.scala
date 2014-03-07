package shipreq.webapp.feature.uc.step

/**
 * Anything that has (recursive) child nodes can be considered tree-like.
 *
 * @since 11/07/2013
 */
trait TreeLike[+N <: TreeNodeLike[N]] {

  protected def nodes: List[N]

  def apply(childIndex: Int) = if (childIndex >= 0) nodes(childIndex) else nodes(nodes.size + childIndex)

  def foreachRecursive(fn: N => Any): Unit = nodes.foreach(_.foreachRecursive(fn))

  def mapRecursive[R](fn: N => R): List[R] = nodes.flatMap(_.mapRecursive(fn))

  def flattenRecursive: List[N] = mapRecursive(n => n)

  def sizeRecursive: Int = {
    var size = 0
    foreachRecursive(_ => size += 1)
    size
  }

  def toStreamRecursive: Stream[N] = nodes.toStream.flatMap(_.toStreamRecursive)

  def iteratorRecursive: Iterator[N] = toStreamRecursive.iterator
}

object TreeLike {
  def apply[N <: TreeNodeLike[N]](nodes: List[N]): TreeLike[N] = GenTreeLike(nodes)
}

/** A generic implementation for turning a List into a TreeLike. */
case class GenTreeLike[N <: TreeNodeLike[N]](override val nodes: List[N]) extends TreeLike[N]

/**
 * The root of a tree.
 * Not a node itself, but TreeLike and contains top-level nodes.
 *
 * @since 11/07/2013
 */
trait TreeRoot[+N <: TreeNodeLike[N]] extends TreeLike[N] {
  override def nodes: List[N]
  final def size = nodes.size
  final def isEmpty = nodes.isEmpty
  final def nonEmpty = nodes.nonEmpty
  final def head = nodes.head
  final def headOption = nodes.headOption
  final def tail = nodes.tail
  final def tailAsTreeLike: TreeLike[N] = TreeLike(tail)
}

/**
 * Anything that has a list of children, and is itself a value can be considered treenode-like.
 *
 * @since 1/06/2013
 */
trait TreeNodeLike[+N <: TreeNodeLike[N]] extends TreeLike[N] {
  self: N =>

  val children: List[N]
  override final protected def nodes: List[N] = children

  override def foreachRecursive(fn: N => Any) {
    fn(this)
    super.foreachRecursive(fn)
  }

  override def mapRecursive[R](fn: N => R): List[R] = fn(this) :: super.mapRecursive(fn)

  override def toStreamRecursive: Stream[N] = this #:: children.toStream.flatMap(_.toStreamRecursive)

  def deepCopy[R](fn: (N, List[R]) => R): R = {
    val copiedChildren = deepCopyChildren(fn)
    fn(this, copiedChildren)
  }

  def deepCopyChildren[R](fn: (N, List[R]) => R): List[R] = children.map(_ deepCopy fn)
}