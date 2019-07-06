package shipreq.webapp.base.issue

import japgolly.univeq.UnivEq
import nyaya.prop._
import shipreq.webapp.base.data.{ReqCodeGroupId, ReqId}

final case class IssueStats(total     : Int,
                            inConfig  : Int,
                            inReq     : Int,
                            inRcg     : Int,
                            loose     : Int,
                            reqsUnique: Int) {

  val reqReappearances = inReq - reqsUnique

  IssueStats.props.assert(this)
}

object IssueStats {

  def fromIssues(is: Issues): IssueStats = {
    var inConfig   = 0
    var inReq      = 0
    var inRcg      = 0
    var loose      = 0
    var reqsUnique = 0
    var reqsSeen   = Set.empty[ReqId]

    def addInReq(id: ReqId): Unit = {
      inReq += 1
      if (!reqsSeen.contains(id)) {
        reqsSeen += id
        reqsUnique += 1
      }
    }

    @inline def addInRcg(rcgId: ReqCodeGroupId): Unit =
      inRcg += 1

    @inline def addInConfig(): Unit =
      inRcg += 1

    is.vector.foreach {
      case i: Issue.BlankCustomField      => addInReq(i.reqId)
      case i: Issue.BlankTitle            => addInReq(i.reqId)
      case i: Issue.BlankUseCaseStep      => addInReq(i.ucId)
      case i: Issue.ConflictingTags       => addInReq(i.reqId)
      case i: Issue.DeadIssueTagInRcg     => addInRcg(i.rcgId)
      case i: Issue.DeadIssueTagInReq     => addInReq(i.reqId)
      case i: Issue.DeadRefInRcg          => addInRcg(i.rcgId)
      case i: Issue.DeadRefInReq          => addInReq(i.reqId)
      case i: Issue.DeadTag               => addInReq(i.reqId)
      case i: Issue.EmptyCodeGroup        => addInRcg(i.rcgId)
      case i: Issue.ImplicationRequired   => addInReq(i.reqId)
      case i: Issue.IssueTagInRcg         => addInRcg(i.rcgId)
      case i: Issue.IssueTagInReq         => addInReq(i.reqId)
      case i: Issue.UninhabitableTagField => addInConfig()
    }

    IssueStats(
      total      = is.vector.length,
      inConfig   = inConfig,
      inReq      = inReq,
      inRcg      = inRcg,
      loose     = loose,
      reqsUnique = reqsUnique)
  }


  implicit def equality: UnivEq[IssueStats] =
    UnivEq.derive

  def props: Prop[IssueStats] = {
    def positiveI(name: String, f: IssueStats => Int) =
      Prop.test[IssueStats](s"$name must be positive", f(_) >= 0)

    positiveI("inConfig"        , _.inConfig        ) &
    positiveI("inReq"           , _.inReq           ) &
    positiveI("inRcg"           , _.inRcg           ) &
    positiveI("loose"           , _.loose           ) &
    positiveI("reqsUnique"      , _.reqsUnique      ) &
    positiveI("reqReappearances", _.reqReappearances) &
    Prop.test("total", i => i.total == (i.inConfig + i.inReq + i.inRcg + i.loose))
  }
}