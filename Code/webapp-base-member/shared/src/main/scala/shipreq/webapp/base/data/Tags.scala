package shipreq.webapp.base.data

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import nyaya.prop.CycleDetector
import monocle.Lens
import monocle.macros.{GenLens, Lenses}
import scala.annotation.tailrec
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.webapp.base.util.Must._

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

  val values = AdtMacros.adtValues[TagType]
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
    _.live is Live

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
    val roots = MutableArray(rootIds.iterator.map(lookup)).sortBy(_.tag.name).array
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
    _.tag.live is Live

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

object Tags {
  val empty = apply(TagTree.empty)
  implicit def univEq: UnivEq[Tags] = UnivEq.derive
}

@Lenses
final case class Tags(tree: TagTree) {

  def atagValidate(id: ApplicableTagId): Option[String] =
    tree.get(id) match {
      case Some(tit) => tit.tag match {
        case _: ApplicableTag => None
        case t: TagGroup      => Some(s"$t is not an ApplicableTag.")
      }
      case None               => Some(s"$id not found.")
    }

  def atag(id: ApplicableTagId): ApplicableTag =
    tree.need(id).tag match {
      case a: ApplicableTag => a
      case t: TagGroup      => mustNotHappen(s"$t is not an ApplicableTag.")
    }

  def atagIterator(): Iterator[ApplicableTag] =
    tree.valuesIterator.map(_.tag).filterSubType[ApplicableTag]

  lazy val deadATagIds: Set[ApplicableTagId] =
    atagIterator().filter(_.live is Dead).map(_.id).toSet

  def live(id: TagId): Live =
    tree.need(id).tag.live
}