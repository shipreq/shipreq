package shipreq.webapp.base.event

import shipreq.base.util._
import shipreq.webapp.base.data._
import ApplicableEventGen.ObserveFn
import ScalaExt._
import UtilMacros._

object EventStats {

  val (allNames, name) =
    valuesForAdtF[Event, String] {
      case _: AddStaticField        => "AddStaticField"
      case _: ApplyTemplate         => "ApplyTemplate"
      case _: CreateApplicableTag   => "CreateApplicableTag"
      case _: CreateCustomImpField  => "CreateCustomImpField"
      case _: CreateCustomIssueType => "CreateCustomIssueType"
      case _: CreateCustomReqType   => "CreateCustomReqType"
      case _: CreateCustomTagField  => "CreateCustomTagField"
      case _: CreateCustomTextField => "CreateCustomTextField"
      case _: CreateGenericReq      => "CreateGenericReq"
      case _: CreateReqCodeGroup    => "CreateReqCodeGroup"
      case _: CreateTagGroup        => "CreateTagGroup"
      case _: DeleteCustomField     => "DeleteCustomField"
      case _: DeleteCustomIssueType => "DeleteCustomIssueType"
      case _: DeleteCustomReqType   => "DeleteCustomReqType"
      case _: DeleteReqCodeGroups   => "DeleteReqCodeGroups"
      case _: DeleteReqs            => "DeleteReqs"
      case _: DeleteStaticField     => "DeleteStaticField"
      case _: DeleteTag             => "DeleteTag"
      case _: PatchImplicationSrc   => "PatchImplicationSrc"
      case _: PatchImplicationTgt   => "PatchImplicationTgt"
      case _: PatchReqCodes         => "PatchReqCodes"
      case _: PatchReqTags          => "PatchReqTags"
      case _: RepositionField       => "RepositionField"
      case _: RestoreContent        => "RestoreContent"
      case _: SetCustomTextField    => "SetCustomTextField"
      case _: SetGenericReqTitle    => "SetGenericReqTitle"
      case _: SetGenericReqType     => "SetGenericReqType"
      case _: UpdateApplicableTag   => "UpdateApplicableTag"
      case _: UpdateCustomImpField  => "UpdateCustomImpField"
      case _: UpdateCustomIssueType => "UpdateCustomIssueType"
      case _: UpdateCustomReqType   => "UpdateCustomReqType"
      case _: UpdateCustomTagField  => "UpdateCustomTagField"
      case _: UpdateCustomTextField => "UpdateCustomTextField"
      case _: UpdateReqCodeGroup    => "UpdateReqCodeGroup"
      case _: UpdateTagGroup        => "UpdateTagGroup"
    }
    .map1(_.sorted)

  val allNamesList = allNames.whole.toList

  private val maxNameLen = allNames.iterator.map(_.length).max

  private[EventStats] val reportLineFmt = s"| %-${maxNameLen}s | %7s | %3s |"
  private[EventStats] val reportLineHdr = reportLineFmt.format("EVENT", "GOOD", "BAD")
  private[EventStats] val reportLineSep = s"+-${"-" * maxNameLen}-+-${"-"*7}-+-${"-"*3}-+"

  val empty = EventStats(Nil, Nil)

  val observeFn: ObserveFn[EventStats] =
    _.add(_, _)
}

case class EventStats(ok: List[String], ko: List[String]) {
  import EventStats._

  def add(e: Event, r: ApplyEvent.Result): EventStats = {
    val n = name(e)
    r.fold(err => {
//        if (!err.contains("\n"))
//        if (n.contains("Patch"))
//          println(s"[Event Application Failure] $err\n$e\n")
        copy(ko = n :: ko)
      }, (_: Project) => copy(ok = n :: ok))
  }

  private def lookup(ss: List[String]): String => String =
    ss.foldLeft(Map.empty[String, Int])((m, s) => m.updated(s, 1 + m.getOrElse(s, 0)))
      .mapValuesNow(_.toString)
      .withDefaultValue("")
      .apply

  def report: String = {
    val mOK = lookup(ok)
    val mKO = lookup(ko)
    val content = allNamesList.map(s => reportLineFmt.format(s, mOK(s), mKO(s)))

    val (cOK, cKO) = (ok, ko).mapEach(_.size)
    val c = cOK + cKO
    def per(i: Int) = (i.toDouble / c.toDouble * 100).toInt.toString + "%"
    val total = reportLineFmt.format("Σ", cOK.toString, cKO.toString)
    val totalP = reportLineFmt.format("Σ%", per(cOK), per(cKO))

    val (dOK, dKO) = (ok, ko).mapEach(_.toSet.size)
    def distPer(i: Int) = (i.toDouble / allNames.length.toDouble * 100).toInt.toString + "%"
    val dist = reportLineFmt.format("Event type coverage", distPer(dOK), per(dKO))

    ( reportLineSep :: reportLineHdr ::
      reportLineSep :: content :::
      reportLineSep :: total :: totalP :: dist ::
      reportLineSep :: Nil
    ) mkString "\n"
  }
}
