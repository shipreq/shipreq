package shipreq.webapp.base.data

import monocle.Lens
import monocle.macros.Lenser
import scalaz.Equal
import scalaz.Isomorphism._
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import shapeless.TypeClass.deriveConstructors
import shapeless.contrib.scalaz.Instances._
import japgolly.nyaya.CycleDetector
import shipreq.base.util.IMap
import shipreq.base.util.TaggedTypes.TaggedLong

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
final case class TagGroup(id: Id,
                          name: String,
                          desc: Option[String],
                          mutexChildren: MutexChildren,
                          alive: Alive) extends Tag {
  override def keyO = None
  override def tagType = TagType.Group
}

final case class ApplicableTag(id: Id,
                               name: String,
                               desc: Option[String],
                               key: HashRefKey,
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
  implicit val equality = Equal.equalA[MutexChildren]
  override val from     = equality.equal(MutexChildren, _: MutexChildren)
  override val to       = if (_: Boolean) MutexChildren else Not
  case object Not extends MutexChildren
}

// =====================================================================================================================
// Tag meta

sealed abstract class TagType(val key: String, val name: String) { type Data <: Tag }
object TagType {
  case object Group      extends TagType("G", "Tag Group") { override type Data = TagGroup }
  case object Applicable extends TagType("A", "Tag")       { override type Data = ApplicableTag }
  val values = List[TagType](Group, Applicable)
  val byKey  = IMap.empty((_: TagType).key).addAll(values: _*)
}

object Tag {
  final case class Id(value: Long) extends TaggedLong

  object IdAccess extends ObjDataIdM[Tag.type, Tag, Id] {
    override def id(d: Tag) = d.id
    override def mkId(l: Long) = Id(l)
    override def setId(t: Tag, i: Id) = t match {
      case x: TagGroup      => x.copy(id = i)
      case x: ApplicableTag => x.copy(id = i)
    }
  }

  implicit val equalityTG = deriveEqual[TagGroup]
  implicit val equalityAT = deriveEqual[ApplicableTag]

  //implicit val equality   = deriveEqual[Tag]
  implicit object Equality extends Equal[Tag] {
    override val equalIsNatural =
      Equal[ApplicableTag].equalIsNatural &&
      Equal[TagGroup     ].equalIsNatural
    override def equal(a: Tag, b: Tag): Boolean = a match {
      case x: ApplicableTag  => b match {case y: ApplicableTag  => x ≟ y; case _ => false}
      case x: TagGroup       => b match {case y: TagGroup       => x ≟ y; case _ => false}
    }
  }


  val _name = Lens((_: Tag).name)(n => {
    case TagGroup(a, _, b, c, d)      => TagGroup(a, n, b, c, d)
    case ApplicableTag(a, _, b, c, d) => ApplicableTag(a, n, b, c, d)
  })

  val _alive = Lens((_: Tag).alive)(n => {
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
      implicit val equality: Equal[Status] = Equal.equalA[Status]
    }

    sealed trait FilterPolicy
    object FilterPolicy {
      case object OmitNothing               extends FilterPolicy
      case object OmitBadBranches           extends FilterPolicy
      case object OmitAnythingWithBadParent extends FilterPolicy
      implicit val equality: Equal[FilterPolicy] = Equal.equalA[FilterPolicy]
    }

    implicit val equality = deriveEqual[FlatRow]
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
  }

  def topLevelIds(tt: TagTree): Set[Id] = {
    val allChildren = tt.values.foldLeft(Set.empty[Id])((q, t) => t.children.foldLeft(q)(_ + _))
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
  @inline final def id: Id = tag.id

  def modChildren(f: Vector[Id] => Vector[Id]): TagInTree = {
    val c = f(children)
    if (c eq children) this else TagInTree(tag, c)
  }

  def removeChild(id: Id): TagInTree =
    modChildren(c => if (hasChild(id)) c.filterNot(_ ≟ id) else c)

  def hasChild(id: Id): Boolean =
    children contains id
}

object TagInTree {
  implicit val equality = deriveEqual[TagInTree]

  private[this] def l = Lenser[TagInTree]
  val _tag      = l(_.tag)
  val _children = l(_.children)
}