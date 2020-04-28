package shipreq.webapp.base.data.derivation

import scala.collection.immutable.ArraySeq
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Atom._

final class ReqCodeTrieScan(trie: ReqCode.Trie) {
  import ReqCode._

  private var _groups                = List.empty[CodeGroup]
  private var _liveGroups            = List.empty[LiveCodeGroup]
  private var _liveGroupsById        = Map.empty[ReqCodeGroupId, LiveCodeGroup]
  private val _localReqCodeRefs      = UnivEq.setBuilder[ReqCodeId]
  private val _localUseCaseStepRefs  = UnivEq.setBuilder[UseCaseStepId]

  private[this] val scan = AtomScan.scan {
    case a: ContentRef # CodeRef        => _localReqCodeRefs += a.value
    case a: ContentRef # UseCaseStepRef => _localUseCaseStepRefs += a.value
    case _                              => ()
  }

  trie.foreachValue { data =>

    data match {

      case d: ActiveGroup =>
        _liveGroupsById = _liveGroupsById.updated(d.id, d.group)
        _liveGroups ::= d.group
        _groups ::= d.group
        scan(d.group.title)

      case _ =>
        ()
    }

    for (g <- data.deadGroup) {
      _groups ::= g
      scan(g.title)
    }
  }

  val groups         = _groups
  val liveGroups     = _liveGroups
  val liveGroupsById = _liveGroupsById

  private[data] val localCodeRefs         = _localReqCodeRefs.result()
  private[data] val localUseCaseStepRefs  = _localUseCaseStepRefs.result()
}

object ReqCodeTrieScan {

  def deriveManifest(trie: ReqCode.Trie): ReqCodeManifest = {
    import ReqCode._

    var activeReqCodesByReqId = UnivEq.emptySetMultimap[ReqId, Value]
    var apReqCodesById        = Map.empty[ApReqCodeId, Value]
    var inactiveIdsByReqId    = UnivEq.emptySetMultimap[ReqId, ApReqCodeId]
    var reqCodeGroupsById     = Map.empty[ReqCodeGroupId, Value]
    val idSeq                  = ArraySeq.newBuilder[ReqCodeId]

    trie.foreachPathAndValue { (code, data) =>

      for (id <- data.ids) {
        idSeq += id
        id match {
          case i: ApReqCodeId    => apReqCodesById    = apReqCodesById   .updated(i, code)
          case i: ReqCodeGroupId => reqCodeGroupsById = reqCodeGroupsById.updated(i, code)
        }
      }

      inactiveIdsByReqId ++= data.reqInactive.m

      data match {
        case d: ActiveReq => activeReqCodesByReqId = activeReqCodesByReqId.add(d.reqId, code)
        case _            => ()
      }
    }

    ReqCodeManifest(
      apReqCodesById        = apReqCodesById,
      reqCodeGroupsById     = reqCodeGroupsById,
      activeReqCodesByReqId = activeReqCodesByReqId,
      inactiveIdsByReqId    = inactiveIdsByReqId,
      idSeq                 = idSeq.result(),
    )
  }
}