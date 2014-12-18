package shipreq.webapp.base.data

import monocle.{SimpleLens, Lenser}
import scalaz.Equal
import scalaz.Isomorphism._
import scalaz.syntax.equal._
import shipreq.base.util.IMap
import shipreq.base.util.TaggedTypes.TaggedLong

// =====================================================================================================================
// A single tag. No relationships.

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

  val _name = SimpleLens[Tag](_.name)((t, n) => t match {
    case TagGroup(a, _, b, c, d)      => TagGroup(a, n, b, c, d)
    case ApplicableTag(a, _, b, c, d) => ApplicableTag(a, n, b, c, d)
  })

  val _alive = SimpleLens[Tag](_.alive)((t, n) => t match {
    case TagGroup(a, b, c, d, _)      => TagGroup(a, b, c, d, n)
    case ApplicableTag(a, b, c, d, _) => ApplicableTag(a, b, c, d, n)
  })
}

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

  /**
   * A tag's relations from its own point of view.
   *
   * @param parents Each key is a parent of the subject tag.
   *                Each value is the sibling before which the subject tag should be inserted. (None ⇒ append.)
   * @param children An ordered list of the subject tag's children.
   */
  final case class PovRelations(parents: Map[Id, Option[Id]], children: Vector[Id]) {
    // TODO ↑ this could use some props too

    def apply(tt: TagTree, id: Id): TagTree = {
      var t = tt

      // Add children
      t = t.mod(id, _.modChildren(_ => children))

      // Add parents
      for ((parent, pos) <- parents)
        t = t.mod(parent, _.modChildren(sibs =>
          pos match {
            case None    => if (sibs contains id) sibs else sibs :+ id
            case Some(b) => reposition(sibs, id, before = b)
          }
        ))

      // Remove old parents
      val oldParents = tt.keySet - id -- parents.keySet
      for (p <- oldParents)
        t = t.mod(p, _.removeChild(id))

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
      override def setId(t: PovTag, i: Id) = _tag.modify(t, Tag.IdAccess.setId(_, i))
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