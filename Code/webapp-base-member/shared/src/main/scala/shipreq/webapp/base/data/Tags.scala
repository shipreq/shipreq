package shipreq.webapp.base.data

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import nyaya.prop.CycleDetector
import monocle.Lens
import monocle.macros.{GenLens, Lenses}
import nyaya.util.Multimap
import scala.annotation.tailrec
import scala.collection.mutable
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.webapp.base.util.Must._

sealed trait TagId extends TaggedInt
final case class TagGroupId     (value: Int) extends TagId with TaggedInt
final case class ApplicableTagId(value: Int) extends TagId with TaggedInt

sealed trait Tag {
  val id     : TagId
  def name   : String
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
final case class TagGroup(id         : TagGroupId,
                          name       : String,
                          desc       : Option[String],
                          exclusivity: Exclusivity,
                          live       : Live) extends Tag {
  override def keyO = None
  override def tagType = TagType.Group
}

@Lenses
final case class ApplicableTag(id    : ApplicableTagId,
                               key   : HashRefKey,
                               colour: Option[Colour],
                               desc  : Option[String],
                               live  : Live) extends Tag {
  override def name = key.value
  override def keyO = Some(key)
  override def tagType = TagType.Applicable
}

object ApplicableTag {
  def v1(id  : ApplicableTagId,
         name: String,
         desc: Option[String],
         key : HashRefKey,
         live: Live): ApplicableTag = {
    val _ = name // removed
    apply(
      id     = id,
      key    = key,
      colour = None,
      desc   = desc,
      live   = live,
    )
  }
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
    val roots = MutableArray(rootIds.iterator.map(lookup)).sortBy(_.tag.name.toLowerCase).iterator.toArray // TODO .array should work
    "TagTree\n" +
    nyaya.util.Util.asciiTree(roots)(_.children.map(lookup),
      t => {
        val id = t.id.value
        val isDead = t.tag.live is Dead
        val isMutex = t.tag match {
          case t: TagGroup      => t.exclusivity is Exclusive
          case _: ApplicableTag => false
        }
        val name = t.tag match {
          case t: TagGroup      => t.name
          case t: ApplicableTag => "#" + t.key.value
        }
        s"$name (#$id)${if (isDead) " [DEAD]" else ""}${if (isMutex) " [EXCLUSIVE]" else ""}"
      },
      "  ")
  }

  private[data] def topLevelIds(tt: TagTree): Set[TagId] = {
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
  import FlatTag.FilterPolicy

  def validateApplicableTag(id: ApplicableTagId): Option[String] =
    tree.get(id) match {
      case Some(tit) => tit.tag match {
        case _: ApplicableTag => None
        case t: TagGroup      => Some(s"$t is not an ApplicableTag.")
      }
      case None               => Some(s"$id not found.")
    }

  def needApplicableTag(id: ApplicableTagId): ApplicableTag =
    tree.need(id).tag match {
      case a: ApplicableTag => a
      case t: TagGroup      => mustNotHappen(s"$t is not an ApplicableTag.")
    }

  def applicableTagIterator(): Iterator[ApplicableTag] =
    tree.valuesIterator.map(_.tag).filterSubType[ApplicableTag]

  def needTagGroup(id: TagGroupId): TagGroup =
    tree.need(id).tag match {
      case t: TagGroup      => t
      case a: ApplicableTag => mustNotHappen(s"$a is not a TagGroup.")
    }

  lazy val deadApplicableTagIds: Set[ApplicableTagId] =
    applicableTagIterator().filter(_.live is Dead).map(_.id).toSet

  lazy val exclusiveGroups: ApplicableTagId => Set[TagGroupId] = {
    val results = mutable.HashMap.empty[ApplicableTagId, Set[TagGroupId]]

    val get: ApplicableTagId => Set[TagGroupId] =
      results.get(_) match {
        case Some(ids) => ids
        case None      => Set.empty
      }

    def goDown(id: TagId, exclusiveParents: Set[TagGroupId]): Unit = {
      val t = tree.need(id)
      if (t.tag.live is Live) {

        val nextExclusiveParents: Set[TagGroupId] =
          t.tag match {

            case a: ApplicableTag =>
              if (exclusiveParents.nonEmpty)
                results.update(a.id, exclusiveParents ++ get(a.id))
              exclusiveParents

            case g: TagGroup =>
              g.exclusivity match {
                case Exclusive     => exclusiveParents + g.id
                case NonExclusive => exclusiveParents
              }
          }

        t.children.foreach(goDown(_, nextExclusiveParents))
      }
    }

    topLevelIds.foreach(goDown(_, Set.empty))

    get
  }

  val tagFilter: FilterDead => Tag => Boolean = {
    case HideDead => Tag.filterLive
    case ShowDead => _ => true
  }

  def flatRows(isGood: Tag => Boolean, policy: FilterPolicy): Vector[FlatTag] =
    FlatTag.flatRows(topLevelIds, tree.get(_).get)(isGood, policy)

  def flatRows(fd: FilterDead): Vector[FlatTag] =
    fd match {
      case HideDead => flatRowsLive
      case ShowDead => flatRowsUnfiltered
    }

  lazy val flatRowsLive =
    flatRows(Tag.filterLive, FilterPolicy.OmitAnythingWithBadParent)

  lazy val flatRowsUnfiltered =
    flatRows(_ => true, FlatTag.FilterPolicy.OmitNothing)

  def live(id: TagId): Live =
    tree.need(id).tag.live

  def prettyPrint: String =
    TagTree.prettyPrint(tree)

  lazy val topLevelIds: Set[TagId] =
    TagTree.topLevelIds(tree)

  lazy val directChildren: Multimap[TagId, Vector, TagId] =
    Multimap(tree.mapValues(_.children))

  def directApplicableChildren(id: TagId): Vector[ApplicableTagId] =
    directChildren(id).iterator.filterSubType[ApplicableTagId].toVector

  def directTagGroupChildren(id: TagId): Vector[TagGroupId] =
    directChildren(id).iterator.filterSubType[TagGroupId].toVector

  lazy val directParents: Multimap[TagId, Set, TagGroupId] =
    Multimap(
      directChildren.reverseM[Set]
        .iterator
        .map(_.map2(_.iterator.filterSubType[TagGroupId].toSet)) // only TagGroups can have children - soft restriction atm
        .filter(_._2.nonEmpty)
        .toMap
    )

  def parents(subject: TagId): MMTree.Parents[TagId] =
    MMTree.Relations.deriveParents(subject, directChildren.m)

  def parentsOption(subject: Option[TagId]): MMTree.Parents[TagId] =
    subject match {
      case Some(s) => parents(s)
      case None    => Map.empty
    }

  def relations(subject: TagId): MMTree.Relations[TagId] =
    MMTree.Relations(
      children = directChildren(subject),
      parents = parents(subject),
    )

  def relationsOption(subject: Option[TagId]): MMTree.Relations[TagId] =
    subject match {
      case Some(s) => relations(s)
      case None    => MMTree.Relations.empty
    }

  def recursiveIterator(fd: FilterDead): RecursiveTagIterator =
    recursiveIterator(topLevelIds, fd)

  def recursiveIterator(roots: Iterable[TagId], fd: FilterDead): RecursiveTagIterator =
    new RecursiveTagIterator(this, tagFilter(fd), roots, 0, None)
}

final class RecursiveTagIterator(tags      : Tags,
                                 filter    : Tag => Boolean,
                                 ids       : Iterable[TagId],
                                 val level : Int,
                                 val parent: Option[TagGroup]) {

  def isEmpty =
    applicableTagIterator().isEmpty && orderedGroups.isEmpty

  private lazy val orderedGroups =
    MutableArray(
      ids.iterator
        .filterSubType[TagGroupId]
        .map(tags.needTagGroup)
        .filter(filter)
    )
      .sortBy(_.name.toLowerCase)

  def tagGroupIterator(): Iterator[TagGroup] =
    orderedGroups.iterator

  def applicableTagIterator(): Iterator[ApplicableTag] =
    ids.iterator
      .filterSubType[ApplicableTagId]
      .map(tags.needApplicableTag)
      .filter(filter)

  def applicableTagIdIterator(): Iterator[ApplicableTagId] =
    applicableTagIterator().map(_.id)

  def nextLevel(g: TagGroup): RecursiveTagIterator =
    new RecursiveTagIterator(tags, filter, tags.directChildren(g.id), level + 1, Some(g))

  def nextLevelNonEmpty(g: TagGroup): Option[RecursiveTagIterator] = {
    val n = nextLevel(g)
    Option.unless(n.isEmpty)(n)
  }
}