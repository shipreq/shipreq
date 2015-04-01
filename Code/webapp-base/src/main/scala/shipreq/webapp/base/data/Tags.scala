package shipreq.webapp.base.data

import japgolly.nyaya.CycleDetector
import monocle.Lens
import monocle.macros.GenLens
import scala.annotation.tailrec
import scalaz.{Memo, Equal}
import scalaz.Isomorphism._
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import shipreq.base.util.{Must, UnivEq, IMap}
import shipreq.base.util.TaggedTypes.TaggedLong
import shipreq.webapp.base.TypeclassDerivation._

// =====================================================================================================================
// A single tag. No relationships.

import Tag.Id
import DataImplicits._

sealed trait Tag {
  val id: Id
  val name: String
  val desc: Option[String]
  val alive: Alive
  def keyO: Option[HashRefKey]
  def tagType: TagType
}

/**
 * FR-246: BA shall be able to specify that a grouping cannot be applied.
 *         e.g. “Priority” shouldn't be applicable but its children should.
 */
final case class TagGroup(id           : TagGroup.Id,
                          name         : String,
                          desc         : Option[String],
                          mutexChildren: MutexChildren,
                          alive        : Alive) extends Tag {
  override def keyO = None
  override def tagType = TagType.Group
}

final case class ApplicableTag(id   : ApplicableTag.Id,
                               name : String,
                               desc : Option[String],
                               key  : HashRefKey,
                               alive: Alive) extends Tag {
  override def keyO = Some(key)
  override def tagType = TagType.Applicable
}

/**
 * FR-253: BA shall be able to specify that a grouping's children are mutually-exclusive (like an enum or sum-type).
 * FR-254: BA shall be able to track when two or more enum-groupings (FR-253) (or its children) are applied to the same req.
 */
sealed trait MutexChildren
case object MutexChildren extends MutexChildren with (Boolean <=> MutexChildren) {
  @inline implicit def equality = UnivEq.force[MutexChildren]
  override val from             = equality.equal(MutexChildren, _: MutexChildren)
  override val to               = if (_: Boolean) MutexChildren else Not
  case object Not extends MutexChildren
}

// =====================================================================================================================
// Tag meta

sealed abstract class TagType(val name: String) { type Data <: Tag }
object TagType {
  case object Group      extends TagType("Tag Group") { override type Data = TagGroup }
  case object Applicable extends TagType("Tag")       { override type Data = ApplicableTag }

  implicit val equality: UnivEq[TagType] = { import AutoDerive._; deriveUnivEq }

  val values = List[TagType](Group, Applicable)
}

object Tag {
  sealed trait Id extends TaggedLong

  object IdAccess extends ObjDataIdM[Tag.type, Tag, Id] {
    override def id(d: Tag) = d.id
    override val unapplyData: AnyRef => Option[Tag] = {case r: Tag => Some(r); case _ => None}
    override def mkId(l: Long) = ApplicableTag.Id(l) // This is declared as being for testing only
    override def setId(t: Tag, i: Id) = t match { // TODO Ideally this should be hidden from non-test code
      case x: TagGroup      => x.copy(id = TagGroup     .Id(i.value))
      case x: ApplicableTag => x.copy(id = ApplicableTag.Id(i.value))
    }
  }

  val name = Lens((_: Tag).name)(n => {
    case TagGroup(a, _, b, c, d)      => TagGroup(a, n, b, c, d)
    case ApplicableTag(a, _, b, c, d) => ApplicableTag(a, n, b, c, d)
  })

  val alive = Lens((_: Tag).alive)(n => {
    case TagGroup(a, b, c, d, _)      => TagGroup(a, b, c, d, n)
    case ApplicableTag(a, b, c, d, _) => ApplicableTag(a, b, c, d, n)
  })

  val filterAlive: Tag => Boolean =
    _.alive ≟ Alive

  object CycleDetectors {
    val multimap =
      CycleDetector.Directed.multimap[Vector, Id, Long](_.value, Vector.empty)
    // val tagTree = multimap.contramap((_: TagTree).mapValues(_.children))

    val tagTree =
      CycleDetector[TagTree, Id](
        _.keys.toStream,
        CycleDetector.Directed.check[TagTree, Id, Long](_.get(_).fold(Stream.empty[Id])(_.children.toStream), _.value))
  }

  import AutoDerive._
  implicit val equalityTG: UnivEq[TagGroup]      = deriveUnivEq
  implicit val equalityAT: UnivEq[ApplicableTag] = deriveUnivEq
  implicit val equality  : UnivEq[Tag]           = deriveUnivEq
}

object TagGroup {
  final case class Id(value: Long) extends Tag.Id with TaggedLong
}
object ApplicableTag {
  final case class Id(value: Long) extends Tag.Id with TaggedLong
}

// =====================================================================================================================
// Many tags

object TagTree {
  def empty: TagTree = IMap.empty(_.id)

  def prettyPrint(tt: TagTree): String = {
    def lookup(id: Id) = tt.underlyingMap(id)
    val rootIds = tt.values.foldLeft(tt.keySet)(_ -- _.children)
    val roots = rootIds.toStream.map(lookup).sortBy(_.tag.name)
    "TagTree\n" +
    japgolly.nyaya.util.Util.asciiTree(roots)(_.children.map(lookup),
      t => s"${t.tag.name} (${t.id.value})${if (t.tag.alive ≟ Dead) " DEAD" else ""}",
      "  ")
  }

  object FlatRow {
    sealed trait Status
    object Status {
      case object Good              extends Status
      case object Bad               extends Status
      case object BadParentGoodKids extends Status
      @inline implicit def equality: UnivEq[Status] = UnivEq.force
    }

    sealed trait FilterPolicy
    object FilterPolicy {
      case object OmitNothing               extends FilterPolicy
      case object OmitBadBranches           extends FilterPolicy
      case object OmitAnythingWithBadParent extends FilterPolicy
      @inline implicit def equality: UnivEq[FilterPolicy] = UnivEq.force
    }

    implicit val equality: UnivEq[FlatRow] = deriveUnivEq
  }

  import FlatRow.{FilterPolicy, Status}

  final case class FlatRow(tag: Tag, depth: Int, parentPath: Vector[Id], status: Status) {
    @inline final def id: Id = tag.id

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
      s"${indentation(depth)}${tag.name}"
  }

  val indentation =
    Memo.immutableHashMapMemo[Int, String]("\u00A0\u00A0" * _)

  def topLevelIds(tt: TagTree): Set[Id] = {
    val allChildren = tt.values.foldLeft(UnivEq.emptySet[Id])((q, t) => t.children.foldLeft(q)(_ + _))
    tt.keySet -- allChildren
  }

  def flatten(tt: TagTree) =
    flatRows(topLevelIds(tt), tt.get(_).get) _

  def flatRows(topLvlIds: Set[Id], lookup: Id => TagInTree)
              (filter: Tag => Boolean, policy: FilterPolicy): Vector[FlatRow] = {
    import Status._
    import FilterPolicy._

    val omitAnythingWithBadParent = policy ≟ OmitAnythingWithBadParent
    val omitNothing               = policy ≟ OmitNothing

    def go(r: Vector[FlatRow], t: TagInTree, depth: Int, parentPath: Vector[Id]): Vector[FlatRow] =
      if (filter(t.tag)) {
        var result = r :+ FlatRow(t.tag, depth, parentPath, Good)
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
        var cs = Vector.empty[FlatRow]
        if (t.children.nonEmpty) {
          val nextDepth = depth + 1
          val nextPP = parentPath :+ t.id
          cs = t.children.foldLeft(cs)((q, id) => go(q, lookup(id), nextDepth, nextPP))
        }
        val goodKids = cs.exists(_.status == Good)

        def result(s: Status) = (r :+ FlatRow(t.tag, depth, parentPath, s)) ++ cs
        if (goodKids)
          result(BadParentGoodKids)
        else if (omitNothing)
          result(Bad)
        else
          r
      }

    val topLvl = topLvlIds.toStream.map(lookup).sortBy(_.tag.name)
    topLvl.foldLeft(Vector.empty[FlatRow])(go(_, _, 0, Vector.empty))
  }
}

final case class TagInTree(tag: Tag, children: Vector[Id]) {
  @inline def id: Id = tag.id

  def modChildren(f: Vector[Id] => Vector[Id]): TagInTree = {
    val c = f(children)
    if (c eq children) this else TagInTree(tag, c)
  }

  def removeChild(id: Id): TagInTree =
    modChildren(c => if (hasChild(id)) c.filterNot(_ ≟ id) else c)

  def hasChild(id: Id): Boolean =
    children contains id

  def lookupChildren(implicit tt: TagTree): Stream[Must[TagInTree]] =
    children.toStream.map(tt.apply)

  /** @return Itself and all reachable children. */
  def transitiveChildren(implicit tt: TagTree): Must[Set[Id]] =
    TagInTree.transitiveChildren(lookupChildren, Set(id))
}

object TagInTree {
  implicit val equality: UnivEq[TagInTree] = deriveUnivEq

  val filterAlive: TagInTree => Boolean =
    _.tag.alive ≟ Alive

  val tag      = GenLens[TagInTree](_.tag)
  val children = GenLens[TagInTree](_.children)

  /** @return Itself and all reachable children. */
  @tailrec def transitiveChildren(queue: Stream[Must[TagInTree]], seen: Set[Id])(implicit tt: TagTree): Must[Set[Id]] =
    if (queue.isEmpty)
      Must.Exists(seen)
    else queue.head match {
      case Must.Exists(focus) =>
        val id = focus.id
        if (seen contains id)
          transitiveChildren(queue.tail, seen)
        else
          transitiveChildren(queue.tail append focus.lookupChildren, seen + id)
      case f: Must.Failed => f
  }
}