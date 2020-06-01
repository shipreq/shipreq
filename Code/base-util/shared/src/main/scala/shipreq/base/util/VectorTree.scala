package shipreq.base.util

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import monocle._
import nyaya.prop.Prop
import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scalaz.{Applicative, Equal}
import scalaz.syntax.equal._
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

//  override def toString = s"VectorTree($deepSize nodes)"

  def isEmpty = children.isEmpty

  @inline def nonEmpty = !isEmpty

  def map[B](f: A => B): VectorTree[B] =
    VectorTree(children map (_ map f))

  def setChildren[B >: A](c: Children[B]): VectorTree[B] =
    VectorTree(c)

  private def _modifyAt[B >: A, R](ploc: ParentLocation)
                                  (mod: (Children[A], Int, Node[A]) => Option[Children[B]])
                                  (result: (Children[B], Node[A]) => R): Option[R] =
    ploc match {
      case ParentLocation.Empty => None
      case ParentLocation.At(locNE) =>
        val loc = locNE.whole
        val last = loc.length - 1
        var old: Node[A] = null

        @tailrec
        def go[X](locInd: Int, ch: Children[A])(f: Children[B] => X): Option[X] = {
          val i = loc(locInd)
          if (ch isIndexValid i) {
            val n = ch(i)
            if (locInd == last) {
              old = n
              mod(ch, i, n) map f
            } else
              go(locInd + 1, n.children)(c2 => f(ch.updated(i, n.copy(children = c2))))
          } else
            None
        }

        go(0, children)(result(_, old))
      }

  /**
   * @return `None` if nothing was changed.
   */
  def modifyChildrenAt[B >: A](loc: ParentLocation)(f: Children[A] => Children[B]): Option[VectorTree[B]] =
    modifyChildrenAtA[B](loc)(c => Some(f(c)))

  /**
   * The `A` suffix means abortable.
   *
   * @param f Return `None` to abort (not delete).
   * @return `None` if nothing was changed.
   */
  def modifyChildrenAtA[B >: A](loc: ParentLocation)(f: Children[A] => Option[Children[B]]): Option[VectorTree[B]] =
    _modifyAt[B, VectorTree[B]](loc :+ 0 asParentLoc)((c, i, n) => f(c))((c, _) => VectorTree(c))

  def modifyNodeAt[B >: A](loc: Location)(f: Node[A] => Node[B]): Option[VectorTree[B]] =
    _modifyAt[B, VectorTree[B]](loc.asParentLoc)((c, i, n) => Some(c.updated(i, f(n))))((c, _) => VectorTree(c))

  def modifyValueAt[B >: A](loc: Location)(f: A => B): Option[VectorTree[B]] =
    modifyNodeAt[B](loc)(n => n.copy(value = f(n.value)))

  def remove(loc: Location): Option[VectorTree[A]] =
    _modifyAt(loc.asParentLoc)((c, i, _) => Some(c deleteOrNull i))((c, _) => VectorTree(c))

  /**
   * The `O` suffix means "old", denoting that the old value is returned.
   */
  def removeNodeO(loc: Location): Option[(VectorTree[A], Node[A])] =
    _modifyAt(loc.asParentLoc)((c, i, _) => Some(c deleteOrNull i))((c, o) => (VectorTree(c), o))

  def find[B](f: A => Boolean)(b: (Location, A) => B): Option[B] =
    locAndValueIterator((l, a) => if (f(a)) Some(b(l, a)) else None).firstDefined

  def findLoc(f: A => Boolean): Option[Location] =
    find(f)((l, _) => l)

  def findLocAndValue(f: A => Boolean): Option[(Location, A)] =
    find(f)((l, a) => (l, a))

  def findValue(f: A => Boolean): Option[A] =
    find(f)((_, a) => a)

  def foreach[U](f: (Location, A) => U): Unit =
    locAndValueIterator(f).foreach(_ => ())

  def locIterator: Iterator[Location] =
    locAndValueIterator((l, _) => l)

  def locAndValueIterator[B](f: (Location, A) => B): Iterator[B] =
    childrenIterator(ParentLocation.Empty, f)

  def subtreeLocAndValueIterator[B](rootIndices: IterableOnce[Int], f: (Location, A) => B): Iterator[B] =
    rootIndices.iterator.flatMap(i =>
      children(i).locAndValueIterator(NonEmptyVector one i, f))

  def append[B >: A](value: B): VectorTree[B] =
    appendN(leaf(value))

  def appendN[B >: A](n: Node[B]): VectorTree[B] =
    VectorTree(children :+ n)

  /**
    * Inserts a node after an existing node in such a way that it would be at the level and position intuitively
    * expected when viewing the [[VectorTree]] as a flat list.
    */
  def insertAfter[B >: A](at: Location, value: B): Option[VectorTree[B]] =
    insertAfterN(at, leaf(value))

  def insertAfterN[B >: A](at: Location, n: Node[B]): Option[VectorTree[B]] =
    modifyChildrenAtA[B](at.parent) { c =>
      val i = at.last
      if (c isIndexValid i) {
        val f = c(i)
        if (at.tail.isEmpty || f.children.nonEmpty) {
          // Add to found node's children
          val h2 = f.copy(children = n +: f.children)
          Some(c.updated(i, h2))
        } else
          // Add at the same level
          c.insertBefore(i + 1, n)
      } else
        None
    }

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
      modifyChildrenAtA(ParentLocation fromVector w.dropRight(2))(ps =>
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

  @inline def shiftLeftV(at: Location, f: Location => Validity): Option[VectorTree[A]] =
    shiftLeft(at)

  def shiftLeftIterator[B](f: (Location, A) => B): Iterator[B] =
    locAndValueIterator((_, _)).filter(p => canShiftLeft(p._1) is Allow).map(f.tupled)

  /**
    * Increases the indent/level of a node.
    *
    * Examples:
    * 1.0.2      --> 1.0.1.a
    * 1.3.4.b    --> 1.3.4.a.ii
    */
  def shiftRight(at: Location): Option[VectorTree[A]] = {
    val from = at.last
    _shiftRight(at.parent, from, from - 1)
  }

  def shiftRightV(at: Location, f: Location => Validity): Option[VectorTree[A]] = {
    assert(f(at) is Valid, s"Location $at is Invalid.")
    val parent = at.parent
    val from = at.last

    // find live child of parent to become the new parent
    (from - 1 to 0 by -1).iterator
      .map(parent :+ _)
      .filter(f(_) is Valid)
      .nextOption()
      .flatMap(np => _shiftRight(parent, from, np.last))
  }

  private def _shiftRight(parent: ParentLocation, from: Int, to: Int): Option[VectorTree[A]] =
    modifyChildrenAtA(parent)(ps =>
      ps.getFlatMap(to)(p =>
        ps.getFlatMap(from) { c =>
          val p2 = p.copy(children = p.children :+ c)
          val b = Vector.newBuilder[VectorTree.Node[A]]
          var i = 0
          for (p <- ps) {
            if (i == to)
              b += p2
            else if (i != from)
              b += p
            i += 1
          }
          Some(b.result())
        }
      )
    )

  def shiftRightIterator[B](f: (Location, A) => B): Iterator[B] =
    locAndValueIterator((_, _)).filter(p => canShiftRight(p._1) is Allow).map(f.tupled)

  lazy val maxDepthTree: VectorTree[Int] = {
    val leaf: Node[Int] =
      VectorTree.leaf(0)

    def go(p: Parent[A]): Node[Int] =
      if (p.children.isEmpty)
        leaf
      else {
        var m = 0
        val c2 = p.children.map { n =>
          val n2 = go(n)
          m = m max n2.value
          n2
        }
        Node(m + 1, c2)
      }

    VectorTree(children map go)
  }

  def lastLoc: Option[Location] = {
    val i = children.length - 1
    if (i == -1)
      None
    else
      Some(children(i).lastLoc(NonEmptyVector one i))
  }

  def filter(f: A => NodeFilter): VectorTree[A] =
    filterN(n => f(n.value))

  def filterN(f: Node[A] => NodeFilter): VectorTree[A] =
    if (children.isEmpty)
      this
    else
      VectorTree(_filterChildren(f))

  /**
   * Partition locations.
   *
   * @return A map of current locations to would-be locations in a new vector tree.
   *         Locations of removed items exist but map to dud (but unique and incremental) locations that contain a -1.
   */
  def partLocs(f: A => NodeFilter): Map[Location, PartialLocation] =
    partLocsN(n => f(n.value))

  def partLocsN(f: Node[A] => NodeFilter): Map[Location, PartialLocation] = {
    var m = Map.empty[Location, PartialLocation]
    import ParentLocation.{Empty => curLoc}
    _partLocs(f, (a, b) => m = m.updated(a, b))(
      cur = new IncLoc(curLoc),
      bad = new IncLoc(curLoc :+ -1),
      gud = new IncLoc(curLoc))
    m
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

  def Location(head: Int, tail: Int*): Location =
    NonEmptyVector.varargs(head, tail: _*)

  @inline implicit class LocationOps(private val loc: Location) extends AnyVal {
    @inline def parent: ParentLocation =
      ParentLocation.fromVector(loc.init)

    @inline def asParentLoc: ParentLocation =
      ParentLocation.At(loc)
  }

  sealed abstract class ParentLocation {
    def :+(i: Int): Location
    def isEmpty: Boolean
    def location: Option[Location]
  }

  object ParentLocation {
    case object Empty extends ParentLocation {
      override def :+(i: Int) = NonEmptyVector one i
      override def isEmpty    = true
      override def location   = None
    }
    case class At(loc: Location) extends ParentLocation {
      override def :+(i: Int) = loc :+ i
      override def isEmpty    = false
      override def location   = Some(loc)
    }

    implicit def univEq: UnivEq[ParentLocation] = UnivEq.derive

    def fromVector(v: Vector[Int]): ParentLocation =
      NonEmptyVector.maybe(v, Empty: ParentLocation)(At)

    // Avoid putting toVector on ParLoc which encourages Vector prepending which uses another array chuck.
    // toVector is only useful in codecs.
    val isoVector: Iso[Vector[Int], ParentLocation] =
      Iso(fromVector) {
        case ParentLocation.Empty => Vector.empty
        case ParentLocation.At(l) => l.whole
      }
  }

  /**
   * Partial location.
   *
   * The value may be a normal [[Location]] that points to a tree node,
   * or it may include a -1 value to indicate that a node used to exist at a similar location but is now removed.
   */
  final case class PartialLocation(value: Location, validity: Validity) {
    assert(
      value.whole.count(_ < 0) == (validity match {
        case Invalid => 1
        case Valid   => 0
      }),
      s"Incorrect validity in $this.")
    assert(value.last >= 0, s"Last node must be valid: $this.")

    def total: Option[Location] =
      validity match {
        case Valid   => Some(value)
        case Invalid => None
      }
  }
  object PartialLocation {
    implicit def univEq: UnivEq[PartialLocation] = UnivEq.derive

    implicit val ordering: Ordering[PartialLocation] =
      new Ordering[PartialLocation] {
        val byElems = Ordering.Implicits.seqOrdering[Vector, Int]
        override def compare(x: PartialLocation, y: PartialLocation): Int =
          if (x.validity ==* y.validity)
            byElems.compare(x.value.whole, y.value.whole)
          else
            x.validity match {
              case Valid => -1
              case _     =>  1
            }
      }

    def detect(value: Location): PartialLocation =
      apply(value, Invalid when value.exists(_ < 0))
  }

  sealed abstract class NodeFilter
  object NodeFilter {
    case object DiscardNodeAndChildren extends NodeFilter
    case object KeepNode               extends NodeFilter
    case object KeepNodeAndChildren    extends NodeFilter
    implicit def univEq: UnivEq[NodeFilter] = UnivEq.derive
  }

  val root: Location =
    NonEmptyVector one 0

  @inline def noChildren: Children[Nothing] =
    Vector.empty

  val empty: VectorTree[Nothing] =
    VectorTree(noChildren)

  def leaf[A](value: A): Node[A] =
    Node(value, noChildren)

  def single[A](value: A): VectorTree[A] =
    VectorTree(Vector1(leaf(value)))

  def canShiftLeft(at: Location): Permission =
    Allow when (at.length >= 2)

  @inline def canShiftLeftV(at: Location, f: Location => Validity): Permission =
    canShiftLeft(at)

  def canShiftRight(at: Location): Permission =
    Allow when (at.last > 0)

  def canShiftRightV(at: Location, f: Location => Validity): Permission = {
    val p = at.parent
    Allow when (0 until at.last).exists(i => f(p :+ i) is Valid)
  }

  @tailrec
  def lastLoc(n: Parent[Any], loc: Location): Location = {
    val i = n.children.length - 1
    if (i == -1)
      loc
    else
      lastLoc(n.children(i), loc :+ i)
  }

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

    final def needAt(pos: Location): Node[A] =
      at(pos) getOrElse ErrorMsg(s"Node not found at position ${pos.whole mkString "."}.").throwException()

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
      getAtLocation(pos) getOrElse ErrorMsg(s"Node not found at position ${pos.whole mkString "."}.").throwException()

    final def valueIterator: Iterator[A] =
      new AbstractIterator[A] {
        var queue: List[Children[A]] = Nil
        var focus: Iterator[Node[A]] =
          Parent.this match {
            case _: VectorTree[A] => children.iterator
            case n: Node[A]       => Iterator.single(n)
          }

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

    def deepSize: Int = {
      var i = 0
      valueIterator.foreach(_ => i += 1)
      i
    }

    final def childrenIterator[B](parent: ParentLocation, f: (Location, A) => B): Iterator[B] =
      new AbstractIterator[B] {
        var index = 0
        var queue = List.empty[Iterator[B]]

        override def hasNext: Boolean =
          queue.nonEmpty || index < children.length

        override def next(): B =
          queue match {
            case Nil =>
              val n = children(index)
              val p = parent :+ index
              val b = f(p, n.value)
              index += 1
              val i = n.childrenIterator(p.asParentLoc, f)
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

    protected final def _filterChildren(f: Node[A] => NodeFilter): Children[A] = {
      val res = Vector.newBuilder[Node[A]]
      children.foreach(n => f(n) match {
        case NodeFilter.DiscardNodeAndChildren => ()
        case NodeFilter.KeepNodeAndChildren    => res += n
        case NodeFilter.KeepNode               => res += n.filterChildrenN(f)
      })
      res.result()
    }

    protected def _partLocs(f: Node[A] => NodeFilter, set: (Location, PartialLocation) => Unit)
                           (cur: IncLoc, bad: IncLoc, gud: IncLoc): Unit = {
      children.foreach { n =>
        val curLoc = cur.next()
        val hasChildren = n.children.nonEmpty

        f(n) match {

          case NodeFilter.KeepNode =>
            val newLoc = gud.next()
            set(curLoc, PartialLocation(newLoc, Valid))
            if (hasChildren)
              n._partLocs(f, set)(
                cur = new IncLoc(curLoc),
                bad = new IncLoc(newLoc :+ -1),
                gud = new IncLoc(newLoc))

          case NodeFilter.DiscardNodeAndChildren =>
            val newLoc = PartialLocation(bad.next(), Invalid)
            set(curLoc, newLoc)
            if (hasChildren)
              n._partLocs(alwaysDiscard, set)(
                cur = new IncLoc(curLoc),
                bad = new IncLoc(newLoc.value),
                gud = null) // Using alwaysDiscard; null is safe.

          case NodeFilter.KeepNodeAndChildren =>
            val newLoc = gud.next()
            set(curLoc, PartialLocation(newLoc, Valid))
            if (hasChildren)
              n._partLocs(alwaysKeep, set)(
                cur = new IncLoc(curLoc),
                bad = null, // Using alwaysKeep; null is safe.
                gud = new IncLoc(newLoc))
        }
      }
    }
  }

  private val alwaysDiscard: Any => NodeFilter =
    _ => NodeFilter.DiscardNodeAndChildren

  private val alwaysKeep: Any => NodeFilter =
    _ => NodeFilter.KeepNodeAndChildren

  // ===================================================================================================================

  final case class Node[+A](value: A, children: Children[A]) extends Parent[A] {
    override def getValue = Some(value)

    def map[B](f: A => B): Node[B] =
      Node(f(value), children map (_ map f))

    def setChildren[B >: A](c: Children[B]): Node[B] =
      Node(value, c)

    def locAndValueIterator[B](currentLocation: Location, f: (Location, A) => B): Iterator[B] =
      Iterator.single(f(currentLocation, value)) ++ childrenIterator(currentLocation.asParentLoc, f)

    def lastLoc(loc: Location): Location =
      VectorTree.lastLoc(this, loc)

    def filterChildren(f: A => NodeFilter): Node[A] =
      filterChildrenN(n => f(n.value))

    def filterChildrenN(f: Node[A] => NodeFilter): Node[A] =
      if (children.isEmpty)
        this
      else
        Node(value, _filterChildren(f))
  }

  private final class IncLoc(parent: ParentLocation) {
    def this(loc: Location) = this(loc.asParentLoc)
    private[this] var i = -1
    def next(): Location = {
      i += 1
      parent :+ i
    }
  }

  // ===================================================================================================================

  /**
    * Dimensions of a [[VectorTree]].
    *
    * @param maxLength Largest number of children per parent.
    * @param maxDepth Root is depth 0, root->children is depth 1, root->children->children is depth 2, etc.
    */
  case class Dims(maxLength: Int, maxDepth: Int) {
    def +(d: Dims): Dims =
      ++(d :: Nil)

    def ++(ds: IterableOnce[Dims]): Dims =
      if (ds.iterator.isEmpty)
        this
      else {
        var ml = maxLength
        var md = maxDepth
        for (d <- ds.iterator) {
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

//  def nodeAt[A](loc: Location): Optional[VectorTree[A], Node[A]] =
//    Optional[VectorTree[A], Node[A]](_.at(loc))(n => s => s.modifyNode(loc)(_ => n) getOrElse s)
}
