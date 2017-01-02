package shipreq.webapp.base.data

import japgolly.microlibs.nonempty.NonEmptyVector
import nyaya.prop.CycleDetector
import monocle.Lens
import monocle.macros.{GenLens, Lenses}
import scala.annotation.tailrec
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.base.util.TaggedTypes.TaggedInt

sealed trait TagId extends TaggedInt
final case class TagGroupId     (value: Int) extends TagId with TaggedInt
final case class ApplicableTagId(value: Int) extends TagId with TaggedInt

sealed trait Tag {
  val id     : TagId
  val name   : String
  val desc   : Option[String]
  val live   : Live
  def keyO   : Option[HashRefKey]
  def tagType: TagType
}

/**
 * FR-246: BA shall be able to specify that a grouping cannot be applied.
 *         e.g. “Priority” shouldn't be applicable but its children should.
 */
@Lenses
final case class TagGroup(id           : TagGroupId,
                          name         : String,
                          desc         : Option[String],
                          mutexChildren: MutexChildren,
                          live         : Live) extends Tag {
  override def keyO = None
  override def tagType = TagType.Group
}

@Lenses
final case class ApplicableTag(id  : ApplicableTagId,
                               name: String,
                               desc: Option[String],
                               key : HashRefKey,
                               live: Live) extends Tag {
  override def keyO = Some(key)
  override def tagType = TagType.Applicable
}

/**
 * FR-253: BA shall be able to specify that a grouping's children are mutually-exclusive (like an enum or sum-type).
 * FR-254: BA shall be able to track when two or more enum-groupings (FR-253) (or its children) are applied to the same req.
 */
sealed trait MutexChildren extends IsoBool[MutexChildren] {
  override final def companion = MutexChildren
}
case object MutexChildren extends MutexChildren with IsoBool.Object[MutexChildren] {
  override def positive = this
  override def negative = Not
  case object Not extends MutexChildren
}


sealed abstract class TagType(val name: String) { type Data <: Tag }
object TagType {
  case object Group      extends TagType("Tag Group") { override type Data = TagGroup }
  case object Applicable extends TagType("Tag")       { override type Data = ApplicableTag }

  implicit def equality: UnivEq[TagType] = UnivEq.derive

  val values = NonEmptyVector[TagType](Group, Applicable)
}

object Tag {
  object IdAccess extends ObjDataId[Tag.type, Tag, TagId] {
    override def id(d: Tag) = d.id
    override val unapplyData: AnyRef => Option[Tag] = {case r: Tag => Some(r); case _ => None}
  }

  val name = Lens((_: Tag).name)(n => {
    case TagGroup(a, _, b, c, d)      => TagGroup(a, n, b, c, d)
    case ApplicableTag(a, _, b, c, d) => ApplicableTag(a, n, b, c, d)
  })

  val live = Lens((_: Tag).live)(n => {
    case TagGroup(a, b, c, d, _)      => TagGroup(a, b, c, d, n)
    case ApplicableTag(a, b, c, d, _) => ApplicableTag(a, b, c, d, n)
  })

  val filterLive: Tag => Boolean =
    _.live :: Live

  object CycleDetectors {
    val multimap =
      CycleDetector.Directed.multimap[Vector, TagId, Int](_.value, Vector.empty)
    // val tagTree = multimap.contramap((_: TagTree).mapValues(_.children))

    val tagTree =
      CycleDetector[TagTree, TagId](
        _.keysIterator,
        CycleDetector.Directed.check[TagTree, TagId, Int](
          _.get(_).fold[Iterator[TagId]](Iterator.empty)(_.children.iterator),
          _.value))
  }

  implicit def equalityTG: UnivEq[TagGroup]      = UnivEq.derive
  implicit def equalityAT: UnivEq[ApplicableTag] = UnivEq.derive
  implicit def equality  : UnivEq[Tag]           = UnivEq.derive
}

// =====================================================================================================================
// TagTree ⊂ TagInTree

object TagTree {
  def empty: TagTree = IMap.empty(_.id)

  def prettyPrint(tt: TagTree): String = {
    def lookup(id: TagId) = tt.underlyingMap(id)
    val rootIds = tt.values.foldLeft(tt.keySet)(_ -- _.children)
    val roots = rootIds.toStream.map(lookup).sortBy(_.tag.name)
    "TagTree\n" +
    nyaya.util.Util.asciiTree(roots)(_.children.map(lookup),
      t => s"${t.tag.name} (${t.id.value})${if (t.tag.live ==* Dead) " DEAD" else ""}",
      "  ")
  }

  def topLevelIds(tt: TagTree): Set[TagId] = {
    val allChildren = tt.values.foldLeft(UnivEq.emptySet[TagId])((q, t) => t.children.foldLeft(q)(_ + _))
    tt.keySet -- allChildren
  }

  implicit object TagTreeMMTree extends MMTree[TagId, TagTree] {

    override def modChildren(id: TagId, f: Vector[TagId] => Vector[TagId]): TagTree => TagTree =
      _.mod(id, _ modChildren f)

    override def removeChild(parent: TagId, child: TagId): TagTree => TagTree =
      _.mod(parent, _ removeChild child)

    override def keySet(t: TagTree): Set[TagId] =
      t.keySet

    override val cycleDetector =
      Tag.CycleDetectors.tagTree
  }
}

final case class TagInTree(tag: Tag, children: TagInTree.Children) {
  import TagInTree.Children

  @inline def id: TagId = tag.id

  def modChildren(f: Children => Children): TagInTree = {
    val c = f(children)
    if (c eq children) this else TagInTree(tag, c)
  }

  def removeChild(id: TagId): TagInTree =
    modChildren(c => if (hasChild(id)) c.filterNot(_ ==* id) else c)

  def hasChild(id: TagId): Boolean =
    children contains id

  def lookupChildren(implicit tt: TagTree): Stream[TagInTree] =
    children.toStream.map(tt.need)

  /** @return Itself and all reachable children. */
  def transitiveChildren(implicit tt: TagTree): Set[TagId] =
    TagInTree.transitiveChildren(lookupChildren, Set(id))
}

object TagInTree {
  type Relations = MMTree.Relations[TagId]
  type Parents   = MMTree.Parents  [TagId]
  type Children  = MMTree.Children [TagId]

  val noRelations: Relations =
    MMTree.Relations.empty

  implicit def equality: UnivEq[TagInTree] = UnivEq.derive

  val filterLive: TagInTree => Boolean =
    _.tag.live :: Live

  val tag      = GenLens[TagInTree](_.tag)
  val children = GenLens[TagInTree](_.children)
  val live     = tag ^|-> Tag.live

  /** @return Itself and all reachable children. */
  @tailrec def transitiveChildren(queue: Stream[TagInTree], seen: Set[TagId])(implicit tt: TagTree): Set[TagId] =
    if (queue.isEmpty)
      seen
    else {
      val focus = queue.head
      val id = focus.id
      if (seen contains id)
        transitiveChildren(queue.tail, seen)
      else
        transitiveChildren(queue.tail append focus.lookupChildren, seen + id)
    }
}

// =====================================================================================================================

final case class FlatTag(tag: Tag, depth: Int, parentPath: Vector[TagId], status: FlatTag.Status) {
  @inline def id: TagId = tag.id

  def key: String =
    if (depth == 0)
      id.value.toString
    else {
      val sb = new StringBuilder
      parentPath.foreach { p =>
        sb append p.value
        sb append '.'
      }
      sb append id.value
      sb.toString()
    }

  def indentedName =
    s"${FlatTag.indentation(depth)}${tag.name}"
}

object FlatTag {
  sealed trait Status
  object Status {
    case object Good              extends Status
    case object Bad               extends Status
    case object BadParentGoodKids extends Status
    implicit def equality: UnivEq[Status] = UnivEq.derive
  }

  sealed trait FilterPolicy
  object FilterPolicy {
    case object OmitNothing               extends FilterPolicy
    case object OmitBadBranches           extends FilterPolicy
    case object OmitAnythingWithBadParent extends FilterPolicy
    implicit def equality: UnivEq[FilterPolicy] = UnivEq.derive
  }

  implicit def equality: UnivEq[FlatTag] = UnivEq.derive

  val indentation =
    Memo.int("\u00A0\u00A0" * _)

  def flatten(tt: TagTree) =
    flatRows(TagTree.topLevelIds(tt), tt.get(_).get) _

  def flatRows(topLvlIds: Set[TagId], lookup: TagId => TagInTree)
              (filter: Tag => Boolean, policy: FilterPolicy): Vector[FlatTag] = {
    import Status._
    import FilterPolicy._

    val omitAnythingWithBadParent = policy ==* OmitAnythingWithBadParent
    val omitNothing               = policy ==* OmitNothing

    def go(r: Vector[FlatTag], t: TagInTree, depth: Int, parentPath: Vector[TagId]): Vector[FlatTag] =
      if (filter(t.tag)) {
        var result = r :+ FlatTag(t.tag, depth, parentPath, Good)
        // Append children directly
        if (t.children.nonEmpty) {
          val nextDepth = depth + 1
          val nextPP = parentPath :+ t.id
          t.children.foreach(id => result = go(result, lookup(id), nextDepth, nextPP))
        }
        result
      } else if (omitAnythingWithBadParent)
        r
      else {
        // Process children separately
        var cs = Vector.empty[FlatTag]
        if (t.children.nonEmpty) {
          val nextDepth = depth + 1
          val nextPP = parentPath :+ t.id
          cs = t.children.foldLeft(cs)((q, id) => go(q, lookup(id), nextDepth, nextPP))
        }
        val goodKids = cs.exists(_.status == Good)

        def result(s: Status) = (r :+ FlatTag(t.tag, depth, parentPath, s)) ++ cs
        if (goodKids)
          result(BadParentGoodKids)
        else if (omitNothing)
          result(Bad)
        else
          r
      }

    val topLvl = topLvlIds.toStream.map(lookup).sortBy(_.tag.name)
    topLvl.foldLeft(Vector.empty[FlatTag])(go(_, _, 0, Vector.empty))
  }
}
