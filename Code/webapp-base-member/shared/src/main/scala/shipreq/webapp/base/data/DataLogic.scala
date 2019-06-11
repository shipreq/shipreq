package shipreq.webapp.base.data

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import scala.annotation.tailrec
import shipreq.base.util.Util
import shipreq.base.util.Digraph.BiDir
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.text.Atom
import DataImplicits._

object DataLogic {

  // ===================================================================================================================
  // Tags

  // The [Tags] field:
  // 1. never displays tags allocated to live tag-columns.
  // 2. doesn't display tags allocated to visible, dead tag-columns.
  def tagFieldDist(pc           : ProjectConfig,
                   fd           : FilterDead,
                   deadTagFilter: CustomField.Tag.Id => Boolean): TagFieldDistribution.TagIds =
    fd match {
      case HideDead => pc.liveTagFieldDistribution
      case ShowDead => pc.deadTagFieldDistribution(deadTagFilter)
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
    val reqTags = p.content.reqTags
    val tagsInText = p.atomScan.tagRefs

    fd match {
      case HideDead =>
        val deadTags = p.config.tags.deadATagIds
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

  def generalTags(dist: TagFieldDistribution.TagIds, lookup: TagLookup): ReqId => Set[ApplicableTagId] = {
    val tagsUsedInFields = dist.usedInFields
    id => lookup(id).all &~ tagsUsedInFields
  }

  def customFieldTags(dist: TagFieldDistribution.TagIds, lookup: TagLookup, fid: CustomField.Tag.Id): ReqId => Set[ApplicableTagId] = {
    val legal = dist inField fid
    id => lookup(id).all & legal
  }

  val normaliseStringForSorting: EndoFn[String] =
    _.toLowerCase

  type TagOrder = Map[ApplicableTagId, Int]

  def tagOrderByName(tags: TagTree): TagOrder =
    MutableArray(tags.valuesIterator.map(_.tag).filterSubType[ApplicableTag])
      .sortBySchwartzian(_.key.value |> normaliseStringForSorting)
      .map(_.id)
      .iterator
      .mapToOrder

  def tagOrderByPos(tags: Tags): TagOrder =
    tags.flatRowsUnfiltered
      .iterator
      .map(_.id)
      .filterSubType[ApplicableTagId]
      .mapToOrder

  // ===================================================================================================================
  // Implications

  def impValueFilter(pc: ProjectConfig, fd: FilterDead): Req => Boolean =
    fd.filterFnBy((_: Req).live(pc.reqTypes))

  def customFieldImps(p: Project, filter: Req => Boolean): CustomField.Implication.Id => ReqId => Set[Pubid] =
    Memo { fid =>
      // (source of implication for this column) → (all it transitively implies)
      val f = p.config.fields.custom(fid)
      val srcs: List[(Pubid, Set[ReqId])] =
        p.content.reqs.reqsByType(f.reqTypeId).iterator
          .filter(filter)
          .map(r => (r.pubid, p.implicationSrcToTgtTC(r.id)))
          .toList
      id => srcs.iterator.filter(_._2 contains id).map(_._1).toSet
    }

  final case class ImpRequiredResult(goodImpGraph: Implications.BiDir,
                                     badIds      : Set[ReqId],
                                     badImpGraph : Implications.BiDir)

  def requiringImplication(reqTypes: ReqTypes,
                           imps    : Implications.BiDir,
                           reqs    : Requirements): ImpRequiredResult = {

    val reqTypesRequiringImp: Vector[ReqType] =
      reqTypes.all.whole.filter(_.imp is ImplicationRequired)

    @tailrec
    def go(maybeGood: Set[ReqId],
           goodImps : Implications.UniDir,
           bad      : Set[ReqId],
           badImps  : Implications.UniDir): (Implications.UniDir, Set[ReqId], Implications.UniDir) = {

      val (b, g) = maybeGood.partition(goodImps(_).isEmpty)
      if (b.isEmpty)
        (goodImps, bad, badImps)
      else {
        val bad2      = bad ++ b
        var badImps2  = badImps
        var goodImps2 = Implications.emptyUniDir
        for ((k, vs) <- goodImps.iterator)
          if (bad2.contains(k))
            badImps2 = badImps2.addvs(k, vs)
          else {
            val (bvs, gvs) = vs.partition(bad2.contains)
            badImps2  = badImps2.addvs(k, bvs)
            goodImps2 = goodImps2.setvs(k, gvs)
          }
        go(g, goodImps2, bad2, badImps2)
      }
    }

    val allLiveIdsRequiringImp: Set[ReqId] =
      reqTypesRequiringImp.iterator.flatMap(rt =>
        reqs.reqsByType(rt.reqTypeId)
          .iterator
          .filter(_.live(reqTypes) is Live)
          .map(_.id))
        .toSet

    val r = go(
      allLiveIdsRequiringImp,
      imps.backwards,
      UnivEq.emptySet,
      Implications.emptyUniDir)

    if (r._2.isEmpty)
      ImpRequiredResult(imps, r._2, Implications.emptyBiDir)
    else
      ImpRequiredResult(BiDir(r._1.reverse), r._2, BiDir(r._3.reverse))
  }

  // ===================================================================================================================
  // Issues

  final class IssueLookup(p: Project, fd: FilterDead) {
    import AtomScan._

    type Issues = Vector[Atom.AnyIssue]

    private val get = fd.ldStatAccessor[Issues]

    private def forLoc(loc: IssueLoc): Issues =
      get(p.atomScan.issues(loc))

    def forReq(id: ReqId): Issues =
      forLoc(InReq(id))

    def forReqCode(id: ReqCodeId): Issues =
      forLoc(InRCG(id))
  }

  def issueLookup(p: Project, fd: FilterDead): IssueLookup =
    new IssueLookup(p, fd)

  // ===================================================================================================================
  // Misc

//  def lookupCustomField[I <: CustomFieldId, D <: CustomField, O](pc: ProjectConfig, f: D => O)(implicit d: DataIdAux[D, I]): I => O =
//    id => f(pc.customField(id))

  def pubidSortKeyFn(pc: ProjectConfig): Pubid => (Int, Int) = {
    val reqTypeOrder = pc.reqTypes.order
    p => (reqTypeOrder(p.reqTypeId), p.pos.value)
  }
}
