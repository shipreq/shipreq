package shipreq.webapp.base.issue

import japgolly.univeq.UnivEq
import nyaya.prop._
import scala.annotation.nowarn
import shipreq.webapp.base.data.{LiveCodeGroup, ReqId}

final case class IssueStats(total     : Int,
                            inConfig  : Int,
                            inReq     : Int,
                            inRcg     : Int,
                            manual    : Int,
                            reqsUnique: Int) {

  val reqReappearances = inReq - reqsUnique

  IssueStats.props.assert(this)
}

object IssueStats {

  def fromIssues(is: Issues): IssueStats = {
    var inConfig   = 0
    var inReq      = 0
    var inRcg      = 0
    var manual     = 0
    var reqsUnique = 0
    var reqsSeen   = Set.empty[ReqId]

    def addInReq(id: ReqId): Unit = {
      inReq += 1
      if (!reqsSeen.contains(id)) {
        reqsSeen += id
        reqsUnique += 1
      }
    }

    @inline def addInRcg(@nowarn("cat=unused") rcg: LiveCodeGroup): Unit =
      inRcg += 1

    @inline def addInConfig(): Unit =
      inConfig += 1

    is.vector.foreach {
      case i: Issue.BlankCustomField             => addInReq(i.req.id)
      case i: Issue.BlankTitle                   => addInReq(i.req.id)
      case i: Issue.BlankUseCaseStep             => addInReq(i.step.useCaseId)
      case i: Issue.ConflictingTags              => addInReq(i.req.id)
      case i: Issue.DeadIssueTagInRcg            => addInRcg(i.rcg)
      case i: Issue.DeadIssueTagInReq            => addInReq(i.req.id)
      case i: Issue.DeadRefInRcg                 => addInRcg(i.rcg)
      case i: Issue.DeadRefInReq                 => addInReq(i.req.id)
      case i: Issue.DeadTag                      => addInReq(i.req.id)
      case i: Issue.EmptyCodeGroup               => addInRcg(i.rcg)
      case _: Issue.FieldDefaultTagDead          => addInConfig()
      case _: Issue.FieldDefaultTagNotApplicable => addInConfig()
      case _: Issue.FieldDefaultTagUnrelated     => addInConfig()
      case i: Issue.ImplicationRequired          => addInReq(i.req.id)
      case i: Issue.IssueTagInRcg                => addInRcg(i.rcg)
      case i: Issue.IssueTagInReq                => addInReq(i.req.id)
      case _: Issue.ManualIssue                  => manual += 1
      case _: Issue.NonApplicableField           => addInConfig()
      case i: Issue.NonApplicableTag             => addInReq(i.req.id)
      case _: Issue.UninhabitableTagField        => addInConfig()
    }

    IssueStats(
      total      = is.vector.length,
      inConfig   = inConfig,
      inReq      = inReq,
      inRcg      = inRcg,
      manual     = manual,
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
    positiveI("manual"          , _.manual          ) &
    positiveI("reqsUnique"      , _.reqsUnique      ) &
    positiveI("reqReappearances", _.reqReappearances) &
    Prop.test("total", i => i.total == (i.inConfig + i.inReq + i.inRcg + i.manual))
  }
}