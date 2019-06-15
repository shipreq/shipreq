package shipreq.webapp.base.event

import japgolly.microlibs.adt_macros.AdtMacros._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import ApplicableEventGen.ObserveFn
import Event._

object EventStats {

  val (allNames, name) =
    valuesForAdtF[Event, String] {
      case _: ApplicableTagCreate    => "ApplicableTagCreate"
      case _: ApplicableTagUpdate    => "ApplicableTagUpdate"
      case _: ContentRestore         => "ContentRestore"
      case _: CustomIssueTypeCreate  => "CustomIssueTypeCreate"
      case _: CustomIssueTypeDelete  => "CustomIssueTypeDelete"
      case _: CustomIssueTypeRestore => "CustomIssueTypeRestore"
      case _: CustomIssueTypeUpdate  => "CustomIssueTypeUpdate"
      case _: CustomReqTypeCreate    => "CustomReqTypeCreate"
      case _: CustomReqTypeDelete    => "CustomReqTypeDelete"
      case _: CustomReqTypeRestore   => "CustomReqTypeRestore"
      case _: CustomReqTypeUpdate    => "CustomReqTypeUpdate"
      case _: FieldCustomDelete      => "FieldCustomDelete"
      case _: FieldCustomImpCreate   => "FieldCustomImpCreate"
      case _: FieldCustomImpUpdate   => "FieldCustomImpUpdate"
      case _: FieldCustomRestore     => "FieldCustomRestore"
      case _: FieldCustomTagCreate   => "FieldCustomTagCreate"
      case _: FieldCustomTagUpdate   => "FieldCustomTagUpdate"
      case _: FieldCustomTextCreate  => "FieldCustomTextCreate"
      case _: FieldCustomTextUpdate  => "FieldCustomTextUpdate"
      case _: FieldReposition        => "FieldReposition"
      case _: FieldStaticAdd         => "FieldStaticAdd"
      case _: FieldStaticRemove      => "FieldStaticRemove"
      case _: GenericReqCreate       => "GenericReqCreate"
      case _: GenericReqTitleSet     => "GenericReqTitleSet"
      case _: GenericReqTypeSet      => "GenericReqTypeSet"
      case _: ProjectNameSet         => "ProjectNameSet"
      case _: ProjectTemplateApply   => "ProjectTemplateApply"
      case _: CodeGroupCreate        => "CodeGroupCreate"
      case _: CodeGroupsDelete       => "CodeGroupsDelete"
      case _: CodeGroupUpdate        => "CodeGroupUpdate"
      case _: ReqCodesPatch          => "ReqCodesPatch"
      case _: ReqFieldCustomTextSet  => "ReqFieldCustomTextSet"
      case _: ReqImplicationsPatch   => "ReqImplicationsPatch"
      case _: ReqsDelete             => "ReqsDelete"
      case _: ReqTagsPatch           => "ReqTagsPatch"
      case _: SavedViewCreate        => "SavedViewCreate"
      case _: SavedViewDefaultSet    => "SavedViewDefaultSet"
      case _: SavedViewDelete        => "SavedViewDelete"
      case _: SavedViewUpdate        => "SavedViewUpdate"
      case _: TagDelete              => "TagDelete"
      case _: TagGroupCreate         => "TagGroupCreate"
      case _: TagGroupUpdate         => "TagGroupUpdate"
      case _: TagRestore             => "TagRestore"
      case _: UseCaseCreate          => "UseCaseCreate"
      case _: UseCaseStepCreate      => "UseCaseStepCreate"
      case _: UseCaseStepDelete      => "UseCaseStepDelete"
      case _: UseCaseStepRestore     => "UseCaseStepRestore"
      case _: UseCaseStepShiftLeft   => "UseCaseStepShiftLeft"
      case _: UseCaseStepShiftRight  => "UseCaseStepShiftRight"
      case _: UseCaseStepUpdate      => "UseCaseStepUpdate"
      case _: UseCaseTitleSet        => "UseCaseTitleSet"
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
