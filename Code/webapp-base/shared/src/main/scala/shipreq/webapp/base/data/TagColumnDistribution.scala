package shipreq.webapp.base.data

import shipreq.base.util.Memo
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data.DataImplicits._

// TODO s/column/tagField/
abstract class TagColumnDistribution[A] {
  def all: A
  val inColumn: CustomField.Tag.Id => A
  def usedInColumns: A
  def notUsedInColumns: A

  def map[B](f: A => B): TagColumnDistribution[B] =
    new TagColumnDistribution.Mapped(this, f)
}

object TagColumnDistribution {

  def apply(p: ProjectConfig, tagColumnFilter: CustomField.Tag => Boolean) =
    new TagIds(p, tagColumnFilter)

  // ===================================================================================================================
  final class TagIds(p: ProjectConfig, tagColumnFilter: CustomField.Tag => Boolean) extends TagColumnDistribution[Set[ApplicableTagId]] {
    // Traversing the tag tree for used columns is better than calculating the full
    // transitive closure at O(V²) space and O(V²+VE) time.
    private[this] implicit val tagTree = p.tags

    lazy val allStream =
      tagTree.vstream(_.tag.id).filterT[ApplicableTagId]

    override lazy val all =
      allStream.toSet

    override val inColumn =
      Memo { (fid: CustomField.Tag.Id) =>
        val field = p.customField(fid)
        val tag = tagTree.need(field.tagId)
        tag.transitiveChildren.filterT[ApplicableTagId].toSet
      }

    override lazy val usedInColumns = {
      val tagIds = p.customTagFields filter tagColumnFilter map (_.id)
      tagIds.foldLeft(Set.empty[ApplicableTagId])(_ | inColumn(_))
    }

    override lazy val notUsedInColumns =
      allStream.filterNot(usedInColumns.contains).toSet

    lazy val tags: TagColumnDistribution[Set[ApplicableTag]] =
      map(_ map p.atag)
  }

  // ===================================================================================================================
  final class Mapped[A, B](u: TagColumnDistribution[A], f: A => B) extends TagColumnDistribution[B] {
    override lazy val all               = f(u.all)
    override      val inColumn          = f compose u.inColumn
    override lazy val usedInColumns     = f(u.usedInColumns)
    override lazy val notUsedInColumns  = f(u.notUsedInColumns)
    override      def map[C](g: B => C) = new Mapped(u, g compose f)
  }
}