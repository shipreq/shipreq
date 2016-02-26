package shipreq.webapp.client.data

import shipreq.base.util.{MutableArray, Memo, Util}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import DataImplicits._

object DataLogic {

  // ===================================================================================================================
  // Tags

  // The [Tags] field:
  // 1. never displays tags allocated to live tag-columns.
  // 2. doesn't display tags allocated to visible, dead tag-columns.
  def tagFieldDist(pc           : ProjectConfig,
                   fd           : FilterDead,
                   deadTagFilter: CustomField.Tag.Id => Boolean): TagColumnDistribution.TagIds =
    fd match {
      case HideDead => pc.liveTagColumnDistribution
      case ShowDead => pc.deadTagColumnDistribution(deadTagFilter)
    }

  /**
    * Set of tags associated with a requirement.
    */
  final case class ReqTags(other: Set[ApplicableTagId], deadTagsInLiveText: Set[ApplicableTagId]) {
    def all: Set[ApplicableTagId] =
      other | deadTagsInLiveText

    def exists(f: Set[ApplicableTagId] => Boolean): Boolean =
      f(other) || f(deadTagsInLiveText)
  }

  type TagLookup = ReqId => ReqTags

  def tagLookup(p: Project, fd: FilterDead): TagLookup = {
    val reqTags = p.reqTags
    val tagsInText = p.atomScan.tagRefs

    fd match {
      case HideDead =>
        val deadTags = p.config.deadATagIds
        Memo { id =>
          val inText = tagsInText(id).live
          val liveTags = (reqTags(id) | inText) &~ deadTags // Dead tags on live reqs are ignored unless in text
          ReqTags(liveTags, deadTagsInLiveText = inText & deadTags)
        }

      case ShowDead =>
        // [deadTagsInLiveText = Set.empty] is technically wrong but when (FilterDead == ShowDead) putting everything
        // in `other` is more efficient and achieves the same result (confirmed in LogicTest.filterDead.tagComprehensive)
        Memo(id => ReqTags(reqTags(id) | tagsInText(id).all, deadTagsInLiveText = Set.empty))
    }
  }

  def generalTags(dist: TagColumnDistribution.TagIds, lookup: TagLookup): ReqId => Set[ApplicableTagId] = {
    val tagsUsedInColumns = dist.usedInColumns
    id => lookup(id).all &~ tagsUsedInColumns
  }

  def customFieldTags(dist: TagColumnDistribution.TagIds, lookup: TagLookup, fid: CustomField.Tag.Id): ReqId => Set[ApplicableTagId] = {
    val legal = dist inColumn fid
    id => lookup(id).all & legal
  }

  val normaliseStringForSorting: EndoFn[String] =
    _.toLowerCase

  type TagOrder = Map[ApplicableTagId, Int]

  def tagOrderByName(tags: TagTree): TagOrder =
    Util.mapToOrder(
      MutableArray(tags.valuesIterator.map(_.tag).filterT[ApplicableTag])
        .sortBySchwartzian(_.key.value |> normaliseStringForSorting)
        .map(_.id)
        .iterator)

  def tagOrderByPos(tags: TagTree): TagOrder =
    Util.mapToOrder(
      FlatTag.flatten(tags)(_ => true, FlatTag.FilterPolicy.OmitNothing)
        .iterator
        .map(_.id)
        .filterT[ApplicableTagId])

  // ===================================================================================================================
  // Implications

  def impValueFilter(pc: ProjectConfig, fd: FilterDead): Req => Boolean =
    fd.filterFnA((_: Req).live(pc.customReqTypes))

  def customFieldImps(p: Project, filter: Req => Boolean): CustomField.Implication => ReqId => Set[Pubid] =
    f => {
      // (source of implication for this column) → (all it transitively implies)
      val srcs: List[(Pubid, Set[ReqId])] =
        p.reqs.reqsByType(f.reqTypeId).iterator
          .filter(filter)
          .map(r => (r.pubid, p.implicationSrcToTgtTC(r.id)))
          .toList
      id => srcs.iterator.filter(_._2 contains id).map(_._1).toSet
    }

  // ===================================================================================================================
  // Misc

//  def lookupCustomField[I <: CustomFieldId, D <: CustomField, O](pc: ProjectConfig, f: D => O)(implicit d: DataIdAux[D, I]): I => O =
//    id => f(pc.customField(id))

  def pubidSortKeyFn(pc: ProjectConfig): Pubid => (Int, Int) = {
    val reqTypeOrder = pc.reqTypeOrder
    p => (reqTypeOrder(p.reqTypeId), p.pos.value)
  }
}
