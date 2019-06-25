package shipreq.benchmark.data

import scalaz.{\/, \/-, -\/}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text

object Migration {

  implicit def idsAreNowIntsInsteadOfLongs(l: Long): Int = l.toInt

  case class OldCodeGroup(title: Text.CodeGroupTitle.OptionalText)

  case class OldActiveData(id: ReqCodeId, target: OldCodeGroup \/ ReqId)

  def ReqCodeData(active: Option[OldActiveData],
                  lastGroup  : Option[OldCodeGroup],
                  oldGroups: Set[ReqCodeId],
                  reqInactive: ReqCode.ReqInactive): ReqCode.Data = {
    def dg: ReqCode.DeadGroup =
      if (oldGroups.isEmpty) None else
      lastGroup.map(o => DeadCodeGroup(ReqCodeGroupId(oldGroups.head.value), o.title))
    active match {
      case Some(OldActiveData(id, \/-(reqId))) => ReqCode.ActiveReq(ApReqCodeId(id.value), reqId, dg, reqInactive)
      case Some(OldActiveData(id, -\/(g))) => ReqCode.ActiveGroup(LiveCodeGroup(ReqCodeGroupId(id.value), g.title), reqInactive)
      case None => ReqCode.Inactive(dg, reqInactive)
    }
  }

  def ReqCodeActiveData(id: ReqCodeId, target: OldCodeGroup): OldActiveData =
    OldActiveData(id, -\/(target))

  def ReqCodeActiveData(id: ReqCodeId, target: ReqId): OldActiveData =
    OldActiveData(id, \/-(target))
}
