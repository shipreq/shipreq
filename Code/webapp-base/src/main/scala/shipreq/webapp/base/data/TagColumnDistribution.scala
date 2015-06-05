package shipreq.webapp.base.data

import shipreq.base.util.Must
import shipreq.base.util.ScalaExt._
import shipreq.base.util.UnivEq.{immutableHashMapMemo => memo}
import shipreq.webapp.base.data.DataImplicits._

abstract class TagColumnDistribution[A] {
  val inColumn: CustomField.Tag.Id => A
  def usedInColumns: A
  def notUsedInColumns: A

  def map[B](f: A => B): TagColumnDistribution[B] =
    new TagColumnDistribution.Mapped(this, f)
}

object TagColumnDistribution {

  def apply(p: Project, tagColumnFilter: CustomField.Tag => Boolean) =
    new TagIds(p, tagColumnFilter)

  // ===================================================================================================================
  final class TagIds(p: Project, tagColumnFilter: CustomField.Tag => Boolean) extends TagColumnDistribution[Must[Set[ApplicableTagId]]] {
    // Traversing the tag tree for used columns is better than calculating the full
    // transitive closure at O(V²) space and O(V²+VE) time.
    private[this] implicit val tagTree = p.tags.data

    override val inColumn =
      memo((fid: CustomField.Tag.Id) =>
        p.customField(fid).flatMap(field =>
          tagTree(field.tagId)
            .flatMap(_.transitiveChildren)
            .map(_.filterT[ApplicableTagId].toSet)))

    override lazy val usedInColumns =
      Must.foldMapMF(p.customTagFields filter tagColumnFilter map (_.id))(inColumn)

    override lazy val notUsedInColumns =
      usedInColumns.map(s =>
        tagTree.vstream(_.tag.id)
          .filterT[ApplicableTagId]
          .filterNot(s.contains)
          .toSet)

    lazy val tags =
      map(_ flatMap p.atags[Set])
  }

  // ===================================================================================================================
  final class Mapped[A, B](u: TagColumnDistribution[A], f: A => B) extends TagColumnDistribution[B] {
    override      val inColumn          = f compose u.inColumn
    override lazy val usedInColumns     = f(u.usedInColumns)
    override lazy val notUsedInColumns  = f(u.notUsedInColumns)
    override      def map[C](g: B => C) = new Mapped(u, g compose f)
  }
}