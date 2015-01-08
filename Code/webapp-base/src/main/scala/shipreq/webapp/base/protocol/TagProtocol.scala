package shipreq.webapp.base.protocol

import monocle.macros.Lenser
import scala.collection.GenTraversable
import scalaz.{\/, Equal}
import scalaz.std.AllInstances._
import shapeless.TypeClass.deriveConstructors
import shapeless.contrib.scalaz.Instances._
import shipreq.base.util.Util
import shipreq.base.util.ScalaExt._
import shipreq.prop.{CycleFree, CycleDetector}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.delta.{Partition, RemoteDeltaP}
import Tag.Id

object TagProtocol {

  @inline implicit final def tcTagPov = PovTag.IdAccess

  trait TreeMod[T] {
    def modChildren(id: Id, f: Vector[Id] => Vector[Id]): T => T
    def removeChild(parent: Id, child: Id): T => T
    def keySet(t: T): Set[Id]
    def cycleDetector: CycleDetector[T, Id]
  }

  implicit object TagTreeMod extends TreeMod[TagTree] {
    override def modChildren(id: Id, f: Vector[Id] => Vector[Id]): TagTree => TagTree =
      _.mod(id, _ modChildren f)

    override def removeChild(parent: Id, child: Id): TagTree => TagTree =
      _.mod(parent, _ removeChild child)

    override def keySet(t: TagTree): Set[Id] =
      t.keySet

    override val cycleDetector =
      Tag.CycleDetectors.tagTree
  }

  /**
   * A tag's relations from its own point of view.
   *
   * @param parents Each key is a parent of the subject tag.
   *                Each value is the sibling before which the subject tag should be inserted. (None ⇒ append.)
   * @param children An ordered list of the subject tag's children.
   */
  final case class PovRelations(parents: Map[Id, Option[Id]], children: Vector[Id]) {

    // For testing
    def allReferencedIds: Set[Id] =
      parents.keySet ++
      parents.values.filter(_.isDefined).map(_.get).toSet ++
      children.toSet
  }

  object PovRelations {
    implicit val equality = Equal.equalA[PovRelations]

    def safeApply1[T](rels: PovRelations, id: Id, tt: T)(implicit T: TreeMod[T]): (Id, Id) \/ CycleFree[T] =
      T.cycleDetector cycleFree trustedApply1(rels, id, tt)

    def safeApplyN[T](rels: GenTraversable[(Id, PovRelations)], tt: T)(implicit T: TreeMod[T]): (Id, Id) \/ CycleFree[T] =
      T.cycleDetector cycleFree trustedApplyN(rels, tt)

    def trustedApplyN[T](rels: GenTraversable[(Id, PovRelations)], tt: T)(implicit T: TreeMod[T]): T =
      rels.foldLeft(tt) { case (t, (id, r)) => trustedApply1(r, id, t) }

    def trustedApply1[T](rels: PovRelations, id: Id, tt: T)(implicit T: TreeMod[T]): T = {
      var t = tt

      // Add children
      t = T.modChildren(id, _ => rels.children)(t)

      // Add parents
      for ((parent, pos) <- rels.parents)
        t = T.modChildren(parent, Util.reposition(_, id, pos))(t)

      // Remove old parents
      val oldParents = T.keySet(t) - id -- rels.parents.keySet
      for (p <- oldParents)
        t = T.removeChild(p, id)(t)

      t
    }

    def derive(id: Id, tree: Map[Id, Vector[Id]]): PovRelations = {
      val children = tree.getOrElse(id, Vector.empty)

      val parents = tree
        .filter(_._2 contains id)
        .foldLeft(Map.empty[Tag.Id, Option[Tag.Id]]) {
          case (m, (parent, sibs)) => m + (parent -> Util.position(sibs, id))
        }

      PovRelations(parents, children)
    }
  }

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
    }
  }

  sealed trait Values

  final case class TagGroupValues(name: String,
                                  mutexChildren: MutexChildren,
                                  desc: Option[String]) extends Values

  final case class ApplicableTagValues(name: String,
                                       key: HashRefKey,
                                       desc: Option[String]) extends Values

  implicit val tagGroupValueEquality      = deriveEqual[TagGroupValues]
  implicit val applicableTagValueEquality = deriveEqual[ApplicableTagValues]

  object PartitionFns extends Partition.Fns[Partition.Tags.type] {
    def rev(p: Project): Rev =
      p.tags.rev

    def update(p: Project, rev: Rev, ds: RemoteDeltaP[Partition.Tags.type]): Project = {
      var t = p.tags.data

      // Delete tags
      for (id <- ds.del)
        t = t.mapUnderlying(_.mapValues(_ removeChild id) - id)

      // Insert/update
      // (Separate phases ∵ all ids must exist before updating structure)
      t = t.addAll(ds.upd.map(u => TagInTree(u.tag, Vector.empty)): _*)
      t = PovRelations.trustedApplyN(ds.upd.map(_.tmap2(_.id, _.rels)), t)

      // Done
      p.copy(tags = RevAnd(rev, t))
    }
  }

}