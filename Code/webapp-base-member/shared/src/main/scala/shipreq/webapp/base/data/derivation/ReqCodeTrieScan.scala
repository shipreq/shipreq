package shipreq.webapp.base.data.derivation

import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Atom._

final class ReqCodeTrieScan(trie: ReqCode.Trie) {
  import ReqCode._

  CC.inc("ReqCodesScan.instance")

  private var _activeReqCodesByReqId = UnivEq.emptySetMultimap[ReqId, Value]
  private var _apReqCodesById        = Map.empty[ApReqCodeId, Value]
  private var _groups                = List.empty[CodeGroup]
  private var _idList                = List.empty[ReqCodeId]
  private var _idSet                 = Set.empty[ReqCodeId]
  private var _inactiveIdsByReqId    = UnivEq.emptySetMultimap[ReqId, ApReqCodeId]
  private var _liveGroups            = List.empty[LiveCodeGroup]
  private var _liveGroupsById        = Map.empty[ReqCodeGroupId, LiveCodeGroup]
  private var _reqCodeGroupsById     = Map.empty[ReqCodeGroupId, Value]
  private val _localReqCodeRefs      = UnivEq.setBuilder[ReqCodeId]
  private val _localUseCaseStepRefs  = UnivEq.setBuilder[UseCaseStepId]

  val scan = AtomScan.scan {
    case a: ContentRef # CodeRef        => _localReqCodeRefs += a.value
    case a: ContentRef # UseCaseStepRef => _localUseCaseStepRefs += a.value
    case _                              => ()
  }

  trie.foreachPathAndValue { (code, data) =>

    for (id <- data.ids) {
      _idList ::= id
      _idSet += id
      id match {
        case i: ApReqCodeId    => _apReqCodesById    = _apReqCodesById   .updated(i, code)
        case i: ReqCodeGroupId => _reqCodeGroupsById = _reqCodeGroupsById.updated(i, code)
      }
    }

    _inactiveIdsByReqId ++= data.reqInactive.m

    data match {
      case d: ActiveReq =>
        _activeReqCodesByReqId = _activeReqCodesByReqId.add(d.reqId, code)

      case d: ActiveGroup =>
        _liveGroupsById = _liveGroupsById.updated(d.id, d.group)
        _liveGroups ::= d.group
        _groups ::= d.group
        scan(d.group.title)

      case _: Inactive =>
        ()
    }

    for (g <- data.deadGroup) {
      _groups ::= g
      scan(g.title)
    }
  }

  lazy val activeReqCodesByReqId = CC("ReqCodesScan.read.activeReqCodesByReqId")(_activeReqCodesByReqId)
  lazy val apReqCodesById        = CC("ReqCodesScan.read.apReqCodesById       ")(_apReqCodesById)
  lazy val groups                = CC("ReqCodesScan.read.groups               ")(_groups)
  lazy val inactiveIdsByReqId    = CC("ReqCodesScan.read.inactiveIdsByReqId   ")(_inactiveIdsByReqId)
  lazy val liveGroups            = CC("ReqCodesScan.read.liveGroups           ")(_liveGroups)
  lazy val liveGroupsById        = CC("ReqCodesScan.read.liveGroupsById       ")(_liveGroupsById)
  lazy val idList                = CC("ReqCodesScan.read.idList               ")(_idList)
  lazy val idSet                 = CC("ReqCodesScan.read.idSet                ")(_idSet)
  lazy val reqCodeGroupsById     = CC("ReqCodesScan.read.reqCodeGroupsById    ")(_reqCodeGroupsById)
  lazy val localCodeRefs         = CC("ReqCodesScan.read.localCodeRefs        ")(_localReqCodeRefs.result())
  lazy val localUseCaseStepRefs  = CC("ReqCodesScan.read.localUseCaseStepRefs ")(_localUseCaseStepRefs.result())
}
