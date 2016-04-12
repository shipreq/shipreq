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
      case _: AddUseCaseStep        => "AddUseCaseStep"
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
      case _: CreateUseCase         => "CreateUseCase"
      case _: DeleteCustomField     => "DeleteCustomField"
      case _: DeleteCustomIssueType => "DeleteCustomIssueType"
      case _: DeleteCustomReqType   => "DeleteCustomReqType"
      case _: DeleteReqCodeGroups   => "DeleteReqCodeGroups"
      case _: DeleteReqs            => "DeleteReqs"
      case _: DeleteStaticField     => "DeleteStaticField"
      case _: DeleteTag             => "DeleteTag"
      case _: DeleteUseCaseStep     => "DeleteUseCaseStep"
      case _: PatchImplicationSrc   => "PatchImplicationSrc"
      case _: PatchImplicationTgt   => "PatchImplicationTgt"
      case _: PatchReqCodes         => "PatchReqCodes"
      case _: PatchReqTags          => "PatchReqTags"
      case _: RepositionField       => "RepositionField"
      case _: RestoreContent        => "RestoreContent"
      case _: RestoreUseCaseStep    => "RestoreUseCaseStep"
      case _: SetCustomTextField    => "SetCustomTextField"
      case _: SetGenericReqTitle    => "SetGenericReqTitle"
      case _: SetGenericReqType     => "SetGenericReqType"
      case _: SetUseCaseTitle       => "SetUseCaseTitle"
      case _: ShiftUseCaseStepLeft  => "ShiftUseCaseStepLeft"
      case _: ShiftUseCaseStepRight => "ShiftUseCaseStepRight"
      case _: UpdateApplicableTag   => "UpdateApplicableTag"
      case _: UpdateCustomImpField  => "UpdateCustomImpField"
      case _: UpdateCustomIssueType => "UpdateCustomIssueType"
      case _: UpdateCustomReqType   => "UpdateCustomReqType"
      case _: UpdateCustomTagField  => "UpdateCustomTagField"
      case _: UpdateCustomTextField => "UpdateCustomTextField"
      case _: UpdateReqCodeGroup    => "UpdateReqCodeGroup"
      case _: UpdateTagGroup        => "UpdateTagGroup"
      case _: UpdateUseCaseStep     => "UpdateUseCaseStep"
    }
    .map1(_.sorted)

  val allNamesList = allNames.whole.toList

  private val maxNameLen = allNames.iterator.map(_.length).max

  private[EventStats] val reportLineFmt = s"| %-${maxNameLen}s | %7s | %4s |"
  private[EventStats] val reportLineHdr = reportLineFmt.format("EVENT", "GOOD", "BAD")
  private[EventStats] val reportLineSep = s"+-${"-" * maxNameLen}-+-${"-"*7}-+-${"-"*4}-+"

  val empty = new EventStats(Map.empty, Map.empty)

  val observeFn: ObserveFn[EventStats] =
    _.add(_, _)
}

class EventStats(val ok: Map[String, Int], val ko: Map[String, Int]) {
  import EventStats._

  type M = Map[String, Int]

  private def inc(m: M, s: String, i: Int = 1): M =
    m.updated(s, m.get(s).fold(i)(_ + i))

  private def append(big: M, small: M): M =
    small.foldLeft(big)((q, e) => inc(q, e._1, e._2))

  def add(e: Event, r: ApplyEvent.Result): EventStats = {
    val n = name(e)
    r.fold(err => {
//        if (!err.contains("\n"))
        if (n.contains("Reposi"))
          println(s"[Event Application Failure] $err\n$e\n")
        new EventStats(ok = ok, ko = inc(ko, n))
      }, (_: Project) =>
        new EventStats(ok = inc(ok, n), ko = ko))
  }

  def +(smaller: EventStats): EventStats =
    new EventStats(
      ok = append(ok, smaller.ok),
      ko = append(ko, smaller.ko))

  private def lookup(m: M): String => String =
    m.mapValuesNow(_.toString)
      .withDefaultValue("")
      .apply

  def report: String = {
    val mOK = lookup(ok)
    val mKO = lookup(ko)
    val content = allNamesList.map(s => reportLineFmt.format(s, mOK(s), mKO(s)))

    val (cOK, cKO) = (ok, ko).mapEach(_.valuesIterator.sum)
    val c = cOK + cKO
    def per(i: Int) = (i.toDouble / c.toDouble * 100).toInt.toString + "%"
    val total = reportLineFmt.format("Σ", cOK.toString, cKO.toString)
    val totalP = reportLineFmt.format("Σ%", per(cOK), per(cKO))

    val (dOK, dKO) = (ok, ko).mapEach(_.keys.size)
    def distPer(i: Int) = (i.toDouble / allNames.length.toDouble * 100).toInt.toString + "%"
    val dist = reportLineFmt.format("Distribution", distPer(dOK), per(dKO))

    ( reportLineSep :: reportLineHdr ::
      reportLineSep :: content :::
      reportLineSep :: total :: totalP :: dist ::
      reportLineSep :: Nil
    ) mkString "\n"
  }
}
