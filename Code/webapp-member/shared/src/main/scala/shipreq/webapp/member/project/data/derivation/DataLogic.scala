package shipreq.webapp.member.project.data.derivation

import japgolly.microlibs.utils.Memo
import scala.collection.mutable
import shipreq.base.util.Digraph.BiDir
import shipreq.base.util.ScalaExt._
import shipreq.webapp.member.project.data.DataImplicits._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text.{Atom, Text}

final class DataLogic(p: Project) {
  import DataLogic._
  import FieldReqTypeRules._

  private val reqTypes = p.config.reqTypes

  val issueLookup: FilterDead => IssueLookup =
    FilterDead.memoLazy(new IssueLookup(p, _))

  /** Note: If you want to filter dead tags, use DataLogic.tagFieldDist(...) */
  val tagFieldDist: FilterDead => TagFieldDistribution.TagIds =
    FilterDead.memoLazy(p.config.tagFieldDistribution)

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
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object DataLogic {

  final case class CustomImpFieldValues(getReqIds: ReqId => Set[ReqId],
                                        getPubids: ReqId => Set[Pubid])

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

  val normaliseStringForSorting: EndoFn[String] =
    _.toLowerCase

  // ===================================================================================================================
  // Implications

  final case class ImpRequiredResult(goodImpGraph: Implications.Graph.BiDir,
                                     badIds      : Set[ReqId],
                                     badImpGraph : Implications.Graph.BiDir)

  def requiringImplication(reqTypes: ReqTypes,
                           imps    : Implications.Graph.BiDir,
                           reqs    : Requirements): ImpRequiredResult = {

    val reqTypesRequiringImp: Vector[ReqType] =
      reqTypes.all.whole.filter(_.implication is Mandatory)

    @tailrec
    def go(maybeGood: Set[ReqId],
           goodImps : Implications.Graph.UniDir,
           bad      : Set[ReqId],
           badImps  : Implications.Graph.UniDir): (Implications.Graph.UniDir, Set[ReqId], Implications.Graph.UniDir) = {

      val (b, g) = maybeGood.partition(goodImps(_).isEmpty)
      if (b.isEmpty)
        (goodImps, bad, badImps)
      else {
        val bad2      = bad ++ b
        var badImps2  = badImps
        var goodImps2 = Implications.Graph.emptyUniDir
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
      Implications.Graph.emptyUniDir)

    if (r._2.isEmpty)
      ImpRequiredResult(imps, r._2, Implications.Graph.emptyBiDir)
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
