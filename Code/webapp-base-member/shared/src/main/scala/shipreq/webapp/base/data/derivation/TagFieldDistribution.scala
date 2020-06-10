package shipreq.webapp.base.data.derivation

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.DataImplicits._

/**
 * Works out which tags are allocated to fields and which are not.
 *
 * For example,
 * if tags {status, WIP, done, priority, high, low, v1.0} exist, and custom tag fields {status, priority} exist,
 * then this will determine that
 * {WIP, done} belong in the status field,
 * {high, low} belong in the priority field, and
 * {v1.0} doesn't go into any custom tag field (i.e. will appear in the general Tags field).
 *
 * @tparam A Representation of a tag. Could be the ID, the Tag itself, or other.
 */
abstract class TagFieldDistribution[A] {
  def all: A
  val inTagGroup: TagGroupId => A
  val inField: CustomField.Tag.Id => A
  def usedInFields: A
  def notUsedInFields: A

  def map[B](f: A => B): TagFieldDistribution[B] =
    new TagFieldDistribution.Mapped(this, f)
}

object TagFieldDistribution {

  def apply(p: ProjectConfig, tagFieldFilter: CustomField.Tag => Boolean) =
    new TagIds(p, tagFieldFilter)

  // ===================================================================================================================
  final class TagIds(p: ProjectConfig, tagFieldFilter: CustomField.Tag => Boolean) extends TagFieldDistribution[Set[ApplicableTagId]] {
    // Traversing the tag tree for used columns is better than calculating the full
    // transitive closure at O(V²) space and O(V²+VE) time.
    private[this] implicit val tagTree = p.tags.tree

    override lazy val all =
      tagTree.valuesIterator.map(_.tag.id).filterSubType[ApplicableTagId].toSet

    override val inTagGroup: TagGroupId => Set[ApplicableTagId] =
      Memo { tagGroupId =>
        tagTree.need(tagGroupId).transitiveChildren.iterator.filterSubType[ApplicableTagId].toSet
      }

    override val inField =
      Memo { (fid: CustomField.Tag.Id) =>
        val field = p.fields.custom(fid)
        inTagGroup(field.tagId)
      }

    override lazy val usedInFields = {
      val tagIds = p.fields.customTagFields filter tagFieldFilter map (_.id)
      tagIds.foldLeft(Set.empty[ApplicableTagId])(_ | inField(_))
    }

    override lazy val notUsedInFields =
      all -- usedInFields

    lazy val tags: TagFieldDistribution[Set[ApplicableTag]] =
      map(_ map p.tags.needApplicableTag)
  }

  // ===================================================================================================================
  final class Mapped[A, B](u: TagFieldDistribution[A], f: A => B) extends TagFieldDistribution[B] {
    override lazy val all               = f(u.all)
    override      val inTagGroup       = f compose u.inTagGroup
    override      val inField          = f compose u.inField
    override lazy val usedInFields     = f(u.usedInFields)
    override lazy val notUsedInFields  = f(u.notUsedInFields)
    override      def map[C](g: B => C) = new Mapped(u, g compose f)
  }
}