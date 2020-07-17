package shipreq.webapp.base.data.derivation

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import nyaya.util.Multimap
import scala.collection.mutable
import shipreq.base.util.Digraph.BiDir
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Atom, Text}

final class DataLogic(p: Project) {
  import DataLogic._
  import FieldReqTypeRules._

  private val reqTypes = p.config.reqTypes

  val issueLookup: FilterDead => IssueLookup =
    FilterDead.memoLazy(new IssueLookup(p, _))

  val tagLookup: FilterDead => TagLookup = {
    val reqTags    = p.content.reqTags
    val deadTags   = p.config.tags.deadApplicableTagIds
    def tagsInText = p.atomScan.tagRefs

    def scanLiveText(b: ReqTags.Builder, reqId: ReqId): Unit =
      for (t <- tagsInText(reqId).live)
        t.loc match {
          case loc: Location.Text => b.addTagInLiveText(t.value, loc, Dead when deadTags.contains(t.value))
          case _                  => b.add(t.value, t.loc)
        }

    def addDefaultTags(b         : ReqTags.Builder,
                       fieldRules: FieldSetRules,
                       tagDist   : TagFieldDistribution.TagIds): Unit = {

      lazy val explicitTags = b.other.keySet ++ b.deadTagsInLiveText.keySet

      for (f <- p.config.liveCustomTagFields)
        fieldRules.tag(f.id) match {

          case Resolution.DefaultTo(default) =>
            val relevant = tagDist.inField(f.id)
            if (!explicitTags.exists(relevant.contains))
              b.add(default, Location.FieldDefault)

          case Resolution.Mandatory
             | Resolution.Optional
             | Resolution.NotApplicable =>
        }
    }

    FilterDead.memoLazy {

      case HideDead =>
        val tagDist    = p.config.liveTagFieldDistribution
        val fieldRules = p.config.fieldRules(HideDead)

        // Dead tags on live reqs are ignored unless in live text
        Memo { reqId =>
          val req = p.content.reqs.need(reqId)
          val b = new ReqTags.Builder(req, p)

          // Scan explicitly-added tags
          for (t <- reqTags(reqId))
            if (!deadTags.contains(t))
              b.add(t, Location.Tags)

          // Scan text
          scanLiveText(b, reqId)

          // Add default tags from fields
          addDefaultTags(b, fieldRules(req.reqTypeId), tagDist)

          b.result()
        }

      case ShowDead =>
        val tagDist    = p.config.deadTagFieldDistribution
        val fieldRules = p.config.fieldRules(ShowDead)

        Memo { reqId =>
          val req = p.content.reqs.need(reqId)
          val b = new ReqTags.Builder(req, p)

          // Scan explicitly-added tags
          for (t <- reqTags(reqId))
            b.add(t, Location.Tags)

          // Scan text
          scanLiveText(b, reqId)
          for (t <- tagsInText(reqId).dead)
            b.add(t.value, t.loc, allowNA = true)

          // Add default tags from fields
          addDefaultTags(b, fieldRules(req.reqTypeId), tagDist)

          b.result()
        }
    }
  }

  /** Note: If you want to filter dead tags, use DataLogic.tagFieldDist(...) */
  val tagFieldDist: FilterDead => TagFieldDistribution.TagIds =
    FilterDead.memoLazy(DataLogic.tagFieldDist(p.config, _, None))

  lazy val tagOrderByName: TagOrder =
    p.config.tags.tree
      .valuesIterator
      .map(_.tag)
      .filterSubType[ApplicableTag]
      .|>(MutableArray.apply)
      .sortBySchwartzian(_.key.value |> normaliseStringForSorting)
      .map(_.id)
      .iterator()
      .mapToOrder

  lazy val tagOrderingByName: Ordering[ApplicableTagId] =
    Ordering.by(tagOrderByName)

  lazy val tagOrderByPos: TagOrder =
    p.config.tags
      .flatRowsUnfiltered
      .iterator
      .map(_.id)
      .filterSubType[ApplicableTagId]
      .mapToOrder

  lazy val tagOrderingByPos: Ordering[ApplicableTagId] =
    Ordering.by(tagOrderByPos)

  // See https://shipreq.com/project/d6My#reqs/IV-26 for an explanation of this logic.
  val customFieldImps: FilterDead => CustomField.Implication.Id => CustomImpFieldValues =
    FilterDead.memoLazy { fd =>
      val filter = p.config.reqFilter(fd)

      def filterIds(it: Iterator[ReqId]): Iterator[ReqId] =
        fd.filterFn.iteratorBy(it)(p.content.reqs.need(_).live(reqTypes))

      Memo { fid =>
        val field = p.config.fields.custom(fid)
        val subjectType = field.reqTypeId
        val backwards = p.content.implications.backwards

        type MutableSet = mutable.Set[ReqId]
        type MutableMap = mutable.Map[ReqId, MutableSet]
        @inline def newSet(): MutableSet = mutable.Set.empty
        @inline def newMap(): MutableMap = mutable.Map.empty

        val state = newMap()

        def add(id: ReqId, fieldValue: ReqId): Unit = {
          state.getOrElseUpdate(id, newSet()) += fieldValue
        }

        def addBackwards(fieldValue: ReqId): Unit = {
          def go(id: ReqId): Unit = {
            val it = filterIds(backwards(id).iterator)
            while (it.hasNext) {
              val next = it.next()
              add(next, fieldValue)
              go(next)
            }
          }
          go(fieldValue)
        }

        def addForwards(fieldValue: ReqId, start: ReqId, internal: ReqId => Boolean): Unit = {
          def go(id: ReqId, allowSameType: Boolean): Unit = {
            @inline def hasDifferentType = p.content.reqs.need(id).reqTypeId != subjectType
            if (allowSameType || hasDifferentType) {
              // Ideally this should be optimised by avoid traversal of the same branches.
              // However even in JS, we can't even break 500 ns here so screw it.
              // It can be done later if it ever becomes necessary.

              add(id, fieldValue)

              val it = filterIds(backwards(id).iterator).filterNot(internal)
              while (it.hasNext) {
                val next = it.next()
                go(next, allowSameType = false)
              }
            }
          }
          go(start, allowSameType = true)
        }

        for (fieldValueReq <- p.content.reqs.reqsByType(field.reqTypeId).iterator.filter(filter)) {
          val fieldValue          = fieldValueReq.id
          val everythingBackwards = p.implicationTgtToSrcTC.nonRefl(fieldValue)
          val everythingForwards  = p.implicationSrcToTgtTC(fieldValue)

          addBackwards(fieldValue)

          for (id <- everythingForwards)
            addForwards(fieldValue, id, i => everythingBackwards.contains(i) || everythingForwards.contains(i))
        }

        val getReqIds: ReqId => Set[ReqId] =
          Memo { id =>
            state.get(id) match {
              case Some(s) =>
                val reqTypeId = p.content.reqs.need(id).reqTypeId
                val result: Set[ReqId] =
                  field.fieldReqTypeRules(reqTypeId) match {
                    case Resolution.Optional
                       | Resolution.Mandatory     => s.toSet
                    case Resolution.NotApplicable => Set.empty
                    case Resolution.DefaultTo(x)  => x.impossible
                  }
                state.update(id, null) // free memory
                result

              case None =>
                Set.empty
            }
          }

        val getPubids: ReqId => Set[Pubid] =
          getReqIds.andThen(_.map(p.content.reqs.need(_).pubid))

        CustomImpFieldValues(getReqIds, getPubids)
      }
    }

  val pubidSortKeyFn: Pubid => (Int, Int) = {
    val reqTypeOrder = reqTypes.order
    i => (reqTypeOrder(i.reqTypeId), i.pos.value)
  }

  lazy val pubidOrdering: Ordering[Pubid] =
    Ordering.by(pubidSortKeyFn)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object DataLogic {

  final case class CustomImpFieldValues(getReqIds: ReqId => Set[ReqId],
                                        getPubids: ReqId => Set[Pubid])

  private[this] val defaultOnly = Location.FieldDefault :: Nil

  // ===================================================================================================================
  // Tags

  // The [Tags] field:
  // 1. never displays tags allocated to live tag-columns.
  // 2. doesn't display tags allocated to visible, dead tag-columns.
  def tagFieldDist(pc           : ProjectConfig,
                   fd           : FilterDead,
                   deadTagFilter: Option[CustomField.Tag.Id => Boolean]): TagFieldDistribution.TagIds =
    fd match {
      case HideDead => pc.liveTagFieldDistribution
      case ShowDead => pc.deadTagFieldDistribution(deadTagFilter)
    }

  /**
    * Set of tags associated with a requirement.
    */
  final case class ReqTags(other             : Multimap[ApplicableTagId, List, LocationOf.Tag.InReq],
                           deadTagsInLiveText: Multimap[ApplicableTagId, List, Location.Text],
                           naTagsInLiveText  : Multimap[ApplicableTagId, List, Location.Text]) {

    // Computing eagerly because this is for a single req. Reqs never have a huge number of tags.
    val all: Set[ApplicableTagId] =
      other.keySet | deadTagsInLiveText.keySet | naTagsInLiveText.keySet

    @inline def exists(f: Set[ApplicableTagId] => Boolean): Boolean =
      f(all)

    private[data] def fieldDefaultApplied(scope: Set[ApplicableTagId]): Boolean = {
      all.iterator.filter(scope.contains).take(2).toList match {
        case one :: Nil => other(one) ==* defaultOnly
        case _          => false
      }
    }
  }

  object ReqTags {

    val emptyOther              = Multimap.empty[ApplicableTagId, List, LocationOf.Tag.InReq]
    val emptyDeadTagsInLiveText = Multimap.empty[ApplicableTagId, List, Location.Text]

    private[DataLogic] final class Builder(req: Req, p: Project) {

      private val nonApplicableTags: Set[ApplicableTagId] =
        p.config.naTags(req.reqTypeId).set

      private var _other              = emptyOther
      private var _deadTagsInLiveText = emptyDeadTagsInLiveText
      private var _naTagsInLiveText   = emptyDeadTagsInLiveText

      def other               = _other
      def deadTagsInLiveText  = _deadTagsInLiveText
      def naTagsInLiveText    = _naTagsInLiveText

      def add(id: ApplicableTagId, loc: LocationOf.Tag.InReq, allowNA: Boolean = false): Unit =
        if (allowNA || !nonApplicableTags.contains(id))
          _other = _other.add(id, loc)

      def addTagInLiveText(id: ApplicableTagId, loc: Location.Text, taglive: Live): Unit =
        if (taglive is Dead)
          _deadTagsInLiveText = _deadTagsInLiveText.add(id, loc)
        else if (nonApplicableTags.contains(id))
          _naTagsInLiveText = _naTagsInLiveText.add(id, loc)
        else
          _other = _other.add(id, loc)

      def result(): ReqTags =
        ReqTags(_other, _deadTagsInLiveText, _naTagsInLiveText)
    }
  }

  type TagOrder = Map[ApplicableTagId, Int]

  type TagLookup = ReqId => ReqTags

  def otherTags(dist: TagFieldDistribution.TagIds, lookup: TagLookup): ReqId => Set[ApplicableTagId] = {
    val tagsUsedInFields = dist.usedInFields
    id => lookup(id).all &~ tagsUsedInFields
  }

  def customFieldTags(dist: TagFieldDistribution.TagIds, lookup: TagLookup, fid: CustomField.Tag.Id): ReqId => Set[ApplicableTagId] = {
    val legal = dist inField fid
    id => lookup(id).all & legal
  }

  val normaliseStringForSorting: EndoFn[String] =
    _.toLowerCase

  // ===================================================================================================================
  // Implications

  final case class ImpRequiredResult(goodImpGraph: Implications.BiDir,
                                     badIds      : Set[ReqId],
                                     badImpGraph : Implications.BiDir)

  def requiringImplication(reqTypes: ReqTypes,
                           imps    : Implications.BiDir,
                           reqs    : Requirements): ImpRequiredResult = {

    val reqTypesRequiringImp: Vector[ReqType] =
      reqTypes.all.whole.filter(_.implication is Mandatory)

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

  final class IssueLookup private[DataLogic] (p: Project, fd: FilterDead) {

    type Issues = Vector[Atom.AnyIssue]

    private[this] val getReqIssues = fd.ldStatAccessor[Vector[LocAndValue[LocationOf.Text.InReq, Atom.AnyIssue]]]
    private[this] val getRcgIssues = fd.ldStatAccessor[Vector[Text.CodeGroupTitle.Issue]]

    val forReq: ReqId => Issues =
      Memo(id => getReqIssues(p.atomScan.issuesInReqs(id)).map(_.value))

    def forReqCodeGroup(id: ReqCodeGroupId): Issues =
      getRcgIssues(p.atomScan.issuesInRcgs(id))
  }

  // ===================================================================================================================
  // Misc

//  def lookupCustomField[I <: CustomFieldId, D <: CustomField, O](pc: ProjectConfig, f: D => O)(implicit d: DataIdAux[D, I]): I => O =
//    id => f(pc.customField(id))

}
