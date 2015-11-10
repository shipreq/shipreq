package shipreq.base.util

import monocle._
import nyaya.prop.Prop
import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scalaz.{Applicative, Equal}
import scalaz.syntax.equal._
import ScalaExt._
import VectorTree._

/**
  * An ordered, rooted tree.
  *
  * {{{
  * (root)
  *   m in [0..n): (value m)
  *     m' in [0..n'): (value m.m')
  *       ...
  * }}}
  */
final case class VectorTree[+A](children: Children[A]) extends Parent[A] {

  override def getValue = None

  override def toString = prettyPrintLabeled()

  def isEmpty = children.isEmpty

  @inline def nonEmpty = !isEmpty

  def map[B](f: A => B): VectorTree[B] =
    VectorTree(children map (_ map f))

  def setChildren[B >: A](c: Children[B]): VectorTree[B] =
    VectorTree(c)

  def modifyChildren[B >: A](loc: ParentLocation)(f: Children[A] => Option[Children[B]]): Option[VectorTree[B]] = {
    def go(rem: ParentLocation, c: Children[A]): Option[Children[B]] =
      if (rem.isEmpty)
        f(c)
      else
        c.tryUpdateIndex(rem.head, n => go(rem.tail, n.children).map(Node(n.value, _)))
    go(loc, children) map VectorTree.apply
  }

  def modifyNode[B >: A](loc: Location)(f: Node[A] => Option[Node[B]]): Option[VectorTree[B]] = {
    def go(rem: ParentLocation): Node[A] => Option[Node[B]] = n =>
      if (rem.isEmpty)
        f(n)
      else
        n.children.tryUpdateIndexOrNull(rem.head, go(rem.tail)) match {
          case null => None
          case n2   => Some(Node(n.value, n2))
        }
    children.tryUpdateIndex(loc.head, go(loc.tail)) map VectorTree.apply
  }

  def foreach[U](f: (Location, A) => U): Unit =
    locAndValueIterator(f).foreach(_ => ())

  def locIterator: Iterator[Location] =
    locAndValueIterator((l, _) => l)

  def locAndValueIterator[B](f: (Location, A) => B): Iterator[B] =
    childrenIterator(Vector.empty, f)

  def append[B >: A](n: Node[B]): VectorTree[B] =
    VectorTree(children :+ n)

  /**
    * Inserts a node after an existing node in such a way that it would be at the level and position intuitively
    * expected when viewing the [[VectorTree]] as a flat list.
    */
  def insertAfter[B >: A](at: Location, n: Node[B]): Option[VectorTree[B]] =
    modifyChildren[B](at.init) { c =>
      val i = at.last
      if (c isIndexValid i) {
        val f = c(i)
        if (at.tail.isEmpty || f.children.nonEmpty) {
          // Add to found node's children
          val h2 = f.copy(children = n +: f.children)
          Some(c.updated(i, h2))
        } else
          // Add at the same level
          c.insert(i + 1, n)
      } else
        None
    }

  def remove(at: Location): Option[VectorTree[A]] =
    modifyChildren(at.init)(_ delete at.last)

  def canShiftLeft(at: Location): Boolean =
    at.length >= 2

  /**
    * Decreases the indent/level of a node.
    *
    * Examples:
    * 1.0.2.a      --> 1.0.3
    * 1.3.4.b.iii  --> 1.3.4.c
    */
  def shiftLeft(at: Location): Option[VectorTree[A]] =
    if (at.length < 2)
      None // Root level can't be decreased
    else {
      val w = at.whole
      val ip = w(at.length - 2)
      val ic = w(at.length - 1)
      modifyChildren(w.dropRight(2))(ps =>
        ps.getFlatMap(ip)(p =>
          p.children.getFlatMap(ic) { c =>
            val left  = p.children take ic
            val right = p.children.drop(ic + 1)
            val c2    = c.copy(children = c.children ++ right)
            val patch = p.copy(children = left) :: c2 :: Nil
            Some(ps.patch(ip, patch, 1))
          }
        )
      )
    }

  def canShiftRight(at: Location): Boolean =
    at.last > 0

  /**
    * Increases the indent/level of a node.
    *
    * Examples:
    * 1.0.2      --> 1.0.1.a
    * 1.3.4.b    --> 1.3.4.a.ii
    */
  def shiftRight(at: Location): Option[VectorTree[A]] =
    modifyChildren(at.init) { ps =>
      val ic = at.last
      val ip = ic - 1
      ps.getFlatMap(ip)(p =>
        ps.getFlatMap(ic) { c =>
          val p2 = p.copy(children = p.children :+ c)
          Some(ps.patch(ip, p2 :: Nil, 2))
        }
      )
    }

  def prettyPrintIndented(fmt: A => String = (_: A).toString,
                          indent: String = "  "): String =
    Util.quickSB(sb =>
      foreach { (loc, a) =>
        if (sb.nonEmpty)
          sb append '\n'
        for (_ <- 1 until loc.length)
          sb append indent
        sb append fmt(a)
      }
    )

  def prettyPrintLabeled(fmt: A => String = (_: A).toString,
                         label: Int => IndexLabel = (_: Int) => IndexLabel.NumericFrom0,
                         colSep: String = "       \t",
                         indent: String = "  "): String =
    Util.quickSB(sb =>
      foreach { (loc, a) =>
        if (sb.nonEmpty)
          sb append '\n'
        for ((ind, lvl) <- loc.iterator.zipWithIndex) {
          sb append label(lvl).label(ind)
          sb append '.'
        }
        sb append colSep
        for (_ <- 1 until loc.length)
          sb append indent
        sb append fmt(a)
      }
    )
}

// =====================================================================================================================

trait VectorTreeLowPri {
  private def equalityForChildren[A](n: Equal[Node[A]]): Equal[Children[A]] =
    Equal.equal((a, b) => a.corresponds(b)(n.equal))

  implicit def equalityForNode[A: Equal]: Equal[Node[A]] = {
    lazy val node: Equal[Node[A]] = Equal.equal((a, b) => (a.value ≟ b.value) && kids.equal(a.children, b.children))
    lazy val kids: Equal[Children[A]] = equalityForChildren(node)
    node
  }

  implicit def equalityForRoot[A: Equal]: Equal[VectorTree[A]] =
    equalityForChildren(equalityForNode[A]).contramap(_.children)
}

object VectorTree extends VectorTreeLowPri {

  type Children[+A] = Vector[Node[A]]

  type Location = NonEmptyVector[Int]

  type ParentLocation = Vector[Int]

  def Location(head: Int, tail: Int*): Location =
    NonEmptyVector.varargs(head, tail: _*)

  @inline def noChildren: Children[Nothing] =
    Vector.empty

  val empty: VectorTree[Nothing] =
    VectorTree(noChildren)

  // ===================================================================================================================

  sealed abstract class Parent[+A] {

    val children: Children[A]

    def getValue: Option[A]

    final def at(loc: Location): Option[Node[A]] =
      children.getOrNull(loc.head) match {
        case null => None
        case first =>
          val it = loc.tail.iterator
          @tailrec def go(cur: Node[A]): Option[Node[A]] =
            if (it.hasNext)
              cur.children.getOrNull(it.next()) match {
                case null => None
                case next => go(next)
              }
            else
              Some(cur)
          go(first)
      }

    final def getAtLocation(pos: Location): Option[A] = {
      val it = pos.iterator
      @tailrec
      def go(cur: Parent[A]): Option[A] = {
        val i = it.next()
        if (i >= 0 && i < cur.children.length) {
          val n = cur children i
          if (it.hasNext)
            go(n)
          else
            Some(n.value)
        } else
          None
      }
      go(this)
    }

    final def needAtLocation(pos: Location): A =
      getAtLocation(pos) getOrElse sys.error(s"Node not found at position ${pos.whole mkString "."}.")

    final def valueIterator: Iterator[A] =
      new AbstractIterator[A] {
        var queue = List.empty[Children[A]]
        var focus = children.iterator

        override def hasNext: Boolean =
          focus.hasNext

        override def next(): A = {
          val n = focus.next()
          if (n.children.nonEmpty)
            queue ::= n.children
          if (!focus.hasNext && queue.nonEmpty) {
            focus = queue.head.iterator
            queue = queue.tail
          }
          n.value
        }
      }

    final def childrenIterator[B](posInit: Vector[Int], f: (Location, A) => B): Iterator[B] =
      new AbstractIterator[B] {
        var index = 0
        var queue = List.empty[Iterator[B]]

        override def hasNext: Boolean =
          queue.nonEmpty || index < children.length

        override def next(): B =
          queue match {
            case Nil =>
              val n = children(index)
              val p = NonEmptyVector.end(posInit, index)
              val b = f(p, n.value)
              index += 1
              val i = n.childrenIterator(p.whole, f)
              if (i.hasNext)
                queue ::= i
              b

            case qh :: qt =>
              val b = qh.next()
              if (!qh.hasNext)
                queue = qt
              b
          }
      }

    final def dims: Dims = {
      var maxDepth = 0
      var maxLength = 0

      def go(depth: Int, p: Parent[A]): Unit =
        if (p.children.isEmpty) {

          if (depth > maxDepth)
            maxDepth = depth

        } else {
          val l = p.children.length
          if (l > maxLength)
            maxLength = l

          val d2 = depth + 1
          p.children foreach (go(d2, _))
        }

      go(0, this)

      Dims(maxLength, maxDepth)
    }
  }

  // ===================================================================================================================

  final case class Node[+A](value: A, children: Children[A]) extends Parent[A] {
    override def getValue = Some(value)

    def map[B](f: A => B): Node[B] =
      Node(f(value), children map (_ map f))

    def setChildren[B >: A](c: Children[B]): Node[B] =
      Node(value, c)
  }

  // ===================================================================================================================

  /**
    * Dimensions of a [[VectorTree]].
    *
    * @param maxLength Largest number of children per parent.
    * @param maxDepth Root is depth 0, root→children is depth 1, root→children→children is depth 2, etc.
    */
  case class Dims(maxLength: Int, maxDepth: Int) {
    def +(d: Dims): Dims =
      ++(d :: Nil)

    def ++(ds: TraversableOnce[Dims]): Dims =
      if (ds.isEmpty)
        this
      else {
        var ml = maxLength
        var md = maxDepth
        for (d <- ds) {
          if (d.maxLength > ml) ml = d.maxLength
          if (d.maxDepth > md) md = d.maxDepth
        }
        Dims(ml, md)
      }
  }

  implicit def dimsEquality: UnivEq[Dims] = UnivEq.derive

  def maxDimsProp(maxLengthInclusive: Int, maxDepthInclusive: Int): Prop[VectorTree[Any]] = {
    def checkDim(name: String, actual: Dims => Int, maxInc: Int) =
      Prop.atom[Dims]("VectorTree max " + name, d => {
        val a = actual(d)
        if (a <= maxInc)
          None
        else
          Some(s"$a exceeds limit of $maxInc.")
      })

    (checkDim("length", _.maxLength, maxLengthInclusive) ∧ checkDim("depth", _.maxDepth, maxDepthInclusive)).
      contramap[VectorTree[Any]](_.dims).rename("VectorTree max dimensions")
  }

  // ===================================================================================================================

  implicit def univEqForNode[A: UnivEq]: UnivEq[Node      [A]] = UnivEq.derive
  implicit def univEqForRoot[A: UnivEq]: UnivEq[VectorTree[A]] = UnivEq.derive

  def ptraversal[A, B]: PTraversal[VectorTree[A], VectorTree[B], A, B] =
    new PTraversal[VectorTree[A], VectorTree[B], A, B] {
      val ch = Optics.vectorPTraversal[Node[A], Node[B]] ^|->> nodePTraversal
      override def modifyF[F[_]](f: A => F[B])(va: VectorTree[A])(implicit F: Applicative[F]): F[VectorTree[B]] = {
        val fcb = ch.modifyF(f)(va.children)
        F.map(fcb)(VectorTree.apply)
      }
    }

  def traversal[A]: Traversal[VectorTree[A], A] =
    ptraversal[A, A]

  def nodePTraversal[A, B]: PTraversal[Node[A], Node[B], A, B] =
    new PTraversal[Node[A], Node[B], A, B] {
      val ch = Optics.vectorPTraversal[Node[A], Node[B]] ^|->> this
      override def modifyF[F[_]](f: A => F[B])(na: Node[A])(implicit F: Applicative[F]): F[Node[B]] = {
        val fb = f(na.value)
        val fcb = ch.modifyF(f)(na.children)
        F.apply2(fb, fcb)(Node.apply)
      }
    }

  def nodeTraversal[A]: Traversal[Node[A], A] =
    nodePTraversal[A, A]
}
