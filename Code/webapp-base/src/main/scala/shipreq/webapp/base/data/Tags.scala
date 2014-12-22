package shipreq.webapp.base.data

import monocle.Lens
import monocle.macros.Lenser
import scalaz.Equal
import scalaz.Isomorphism._
import scalaz.syntax.equal._
import shipreq.base.util.IMap
import shipreq.base.util.TaggedTypes.TaggedLong

// =====================================================================================================================
// Tag meta

object Tag {
  final case class Id(value: Long) extends TaggedLong

  object IdAccess extends ObjDataId[Tag.type, Tag, Id] {
    override def id(d: Tag) = d.id
    override def mkId(l: Long) = Id(l)
    override def setId(t: Tag, i: Id) = t match {
      case x: TagGroup      => x.copy(id = i)
      case x: ApplicableTag => x.copy(id = i)
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

  sealed abstract class Type(val key: String, val name: String) { type Data <: Tag}
  object Type {
    case object Group      extends Type("G", "Tag Group") { override type Data = TagGroup }
    case object Applicable extends Type("A", "Tag")       { override type Data = ApplicableTag }
    val values = List[Type](Group, Applicable)
    val byKey  = IMap.empty((_: Type).key).addAll(values: _*)
  }
}

// =====================================================================================================================
// A single tag. No relationships.

import Tag.Id
import DataImplicits._

sealed trait Tag {
  val id: Id
  val name: String
  val desc: Option[String]
  val alive: Alive
  def keyO: Option[RefKey]
}

/**
 * FR-246: BA shall be able to specify that a grouping cannot be applied.
 *         e.g. “Priority” shouldn't be applicable but its children should.
 */
final case class TagGroup(id: Id,
                          name: String,
                          desc: Option[String],
                          enum: IsEnumLike,
                          alive: Alive) extends Tag {
  override def keyO = None
}

final case class ApplicableTag(id: Id,
                               name: String,
                               desc: Option[String],
                               key: RefKey,
                               alive: Alive) extends Tag {
  override def keyO = Some(key)
}

/**
 * FR-253: BA shall be able to specify that a grouping's children are mutually-exclusive (like an enum or sum-type).
 * FR-254: BA shall be able to track when two or more enum-groupings (FR-253) (or its children) are applied to the same req.
 */
sealed trait IsEnumLike
case object IsEnumLike extends IsEnumLike with (Boolean <=> IsEnumLike) {
  implicit val equal = Equal.equalA[IsEnumLike]
  override def from = _ == IsEnumLike
  override def to = b => if (b) IsEnumLike else NotEnumLike
}
case object NotEnumLike extends IsEnumLike

// =====================================================================================================================
// Many tags

object TagTree {
  def empty: TagTree = IMap.empty(_.tag.id)
}

case class TagInTree(tag: Tag, children: Vector[Id]) {
  def modChildren(f: Vector[Id] => Vector[Id]): TagInTree = {
    val c = f(children)
    if (c eq children) this else TagInTree(tag, c)
  }

  def removeChild(id: Id): TagInTree =
    modChildren(c => if (hasChild(id)) c.filterNot(_ == id) else c)

  def hasChild(id: Id): Boolean =
    children contains id
}

object TagInTree {
  private[this] def l = Lenser[TagInTree]
  val _tag      = l(_.tag)
  val _children = l(_.children)
}

// =====================================================================================================================
// Types for sending values over the wire

object TagProtocol {

  trait TreeMod[T] {
    def modChildren(id: Id, f: Vector[Id] => Vector[Id]): T => T
    def removeChild(parent: Id, child: Id): T => T
    def keySet(t: T): Set[Id]
  }

  implicit object TagTreeMod extends TreeMod[TagTree] {
    override def modChildren(id: Id, f: Vector[Id] => Vector[Id]): TagTree => TagTree =
      _.mod(id, _ modChildren f)

    override def removeChild(parent: Id, child: Id): TagTree => TagTree =
      _.mod(parent, _ removeChild child)

    override def keySet(t: TagTree): Set[Id] =
      t.keySet
  }

  /**
   * A tag's relations from its own point of view.
   *
   * @param parents Each key is a parent of the subject tag.
   *                Each value is the sibling before which the subject tag should be inserted. (None ⇒ append.)
   * @param children An ordered list of the subject tag's children.
   */
  final case class PovRelations(parents: Map[Id, Option[Id]], children: Vector[Id]) {
    // TODO ↑ this could use some props too

    def apply[T](tt: T, id: Id)(implicit T: TreeMod[T]): T = {
      var t = tt

      // Add children
      t = T.modChildren(id, _ => children)(t)

      // Add parents
      for ((parent, pos) <- parents)
        t = T.modChildren(parent, sibs =>
          pos match {
            case None    => if (sibs contains id) sibs else sibs :+ id
            case Some(b) => reposition(sibs, id, before = b)
          }
        )(t)

      // Remove old parents
      val oldParents = T.keySet(t) - id -- parents.keySet
      for (p <- oldParents)
        t = T.removeChild(p, id)(t)

      t
    }
  }

  private def reposition[A: Equal](v: Vector[A], ins: A, before: A): Vector[A] =
    v.foldLeft(Vector.empty[A])((q, e) => {
      val q2 = if (e ≟ before) q :+ ins else q
      if (e ≟ ins) q2 else q2 :+ e
    })

  /** A tag and its world from its own point of view. */
  final case class PovTag(tag: Tag, rels: PovRelations) {
    @inline def id = tag.id
  }

  object PovTag {
    private[this] def l = Lenser[PovTag]
    val _tag = l(_.tag)

    object IdAccess extends ObjDataId[PovTag.type, PovTag, Id] {
      override def id(d: PovTag) = d.id
      override def mkId(l: Long) = Id(l)
      override def setId(t: PovTag, i: Id) = _tag.modify(Tag.IdAccess.setId(_, i))(t)
    }
  }

  sealed trait Values

  final case class TagGroupValues(name: String,
                                  desc: Option[String],
                                  enum: IsEnumLike) extends Values

  final case class ApplicableTagValues(name: String,
                                       desc: Option[String],
                                       key: RefKey) extends Values
}