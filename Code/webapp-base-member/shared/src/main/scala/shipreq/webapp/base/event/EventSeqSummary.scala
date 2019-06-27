package shipreq.webapp.base.event

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.univeq._
import shipreq.webapp.base.data._

/** Summary of a sequence of events.
  *
  * Where there are CU and DR suffixes, CU = created/updated, DR = deleted/restored.
  */
final case class EventSeqSummary(
    customIssueTypesCU: Set[CustomIssueTypeId],
    customIssueTypesDR: Set[CustomIssueTypeId],
    customFieldTypesCU: Set[CustomFieldId],
    customFieldTypesDR: Set[CustomFieldId],
    customReqTypesCU  : Set[CustomReqTypeId],
    customReqTypesDR  : Set[CustomReqTypeId],
    tagGroupsCU       : Set[TagGroupId],
    tagGroupsDR       : Set[TagGroupId],
    applicableTagsCU  : Set[ApplicableTagId],
    applicableTagsDR  : Set[ApplicableTagId],
    staticFields      : Set[StaticField],
    genericReqs       : Set[GenericReqId],
    useCasesExclSteps : Set[UseCaseId],
    useCaseSteps      : Set[UseCaseStepId],
    reqCodeGroupsCU   : Set[ReqCodeGroupId],
    reqCodeGroupsDR   : Set[ReqCodeGroupId],
    contentLiveDeps   : Boolean,
    ) {

  private def showStrs(f: Set[_]): String =
    MutableArray(f.iterator.map(_.toString)).sort.mkString("[", ",", "]")

  private def showInts[A](f: Set[A])(g: A => Int): String =
    MutableArray(f.iterator.map(g(_))).sort.mkString("[", ",", "]")

  override def toString =
    s"""
       |EventSeqSummary(
       |  customIssueTypesCU = ${showInts(customIssueTypesCU)(_.value)},
       |  customIssueTypesDR = ${showInts(customIssueTypesDR)(_.value)},
       |  customFieldTypesCU = ${showInts(customFieldTypesCU)(_.value)},
       |  customFieldTypesDR = ${showInts(customFieldTypesDR)(_.value)},
       |  customReqTypesCU   = ${showInts(customReqTypesCU)(_.value)},
       |  customReqTypesDR   = ${showInts(customReqTypesDR)(_.value)},
       |  tagGroupsCU        = ${showInts(tagGroupsCU)(_.value)},
       |  tagGroupsDR        = ${showInts(tagGroupsDR)(_.value)},
       |  applicableTagsCU   = ${showInts(applicableTagsCU)(_.value)},
       |  applicableTagsDR   = ${showInts(applicableTagsDR)(_.value)},
       |  staticFields       = ${showStrs(staticFields)},
       |  genericReqs        = ${showInts(genericReqs)(_.value)},
       |  useCasesExclSteps  = ${showInts(useCasesExclSteps)(_.value)},
       |  useCaseSteps       = ${showInts(useCaseSteps)(_.value)},
       |  reqCodeGroupsCU    = ${showInts(reqCodeGroupsCU)(_.value)},
       |  reqCodeGroupsDR    = ${showInts(reqCodeGroupsDR)(_.value)},
       |  contentLiveDeps    = $contentLiveDeps){
       |  customTextFields   = $customTextFields,
       |  tagsChanged        = $tagsChanged}
     """.stripMargin

  val customFieldTypes: Set[CustomFieldId] =
    customFieldTypesCU ++ customFieldTypesDR

  val customTextFields: Set[CustomField.Text.Id] =
    customFieldTypes.collect {
      case f: CustomField.Text.Id => f
    }

  lazy val customReqTypes: Set[CustomReqTypeId] =
    customReqTypesCU ++ customReqTypesDR

  lazy val tagGroups: Set[TagGroupId] =
    tagGroupsCU ++ tagGroupsDR

  lazy val applicableTags: Set[ApplicableTagId] =
    applicableTagsCU ++ applicableTagsDR

  lazy val tags: Set[TagId] =
    applicableTags ++ tagGroups

  val tagsCU: Boolean =
    applicableTagsCU.nonEmpty || tagGroupsCU.nonEmpty

  val tagsDR: Boolean =
    applicableTagsDR.nonEmpty || tagGroupsDR.nonEmpty

  val tagsChanged: Boolean =
    tagsCU || tagsDR

  val fieldNamesChanged: Boolean =
    tagsChanged || customFieldTypes.nonEmpty || customReqTypesCU.nonEmpty

  lazy val reqCodeGroups: Set[ReqCodeGroupId] =
    reqCodeGroupsCU ++ reqCodeGroupsDR

  lazy val reqsExclUseCaseSteps: Set[ReqId] =
    genericReqs ++ useCasesExclSteps

  def withProject(p: Project): EventSeqSummary.WithProject =
    EventSeqSummary.WithProject(this, p)
}

object EventSeqSummary {
  
  def apply(events: TraversableOnce[Event]): EventSeqSummary = {
    val b = new MutableBuilder
    b ++= events
    b.result()
  }

  final case class WithProject(summary: EventSeqSummary, project: Project) {

    lazy val useCasesChangedBySteps: Set[UseCaseId] = {
      val stepIndex = project.content.reqs.useCases.stepIndex
      summary.useCaseSteps.map(stepIndex(_).useCaseId)
    }

    lazy val useCases: Set[UseCaseId] =
      summary.useCasesExclSteps ++ useCasesChangedBySteps

    lazy val reqsAffectedByReqTypeChanges: Set[ReqId] =
      summary.customReqTypesDR.flatMap(project.content.reqs.reqsByType(_).iterator.map(_.id))

    lazy val reqs: Set[ReqId] =
      summary.reqsExclUseCaseSteps ++ useCasesChangedBySteps ++ reqsAffectedByReqTypeChanges
  }

  implicit def exportSummaryToWithProject(w: WithProject) = w.summary

  // ===================================================================================================================

  private final class MutableBuilder {
    private var customIssueTypesCU = UnivEq.emptySet[CustomIssueTypeId]
    private var customIssueTypesDR = UnivEq.emptySet[CustomIssueTypeId]
    private var customFieldTypesCU = UnivEq.emptySet[CustomFieldId]
    private var customFieldTypesDR = UnivEq.emptySet[CustomFieldId]
    private var customReqTypesCU   = UnivEq.emptySet[CustomReqTypeId]
    private var customReqTypesDR   = UnivEq.emptySet[CustomReqTypeId]
    private var tagGroupsCU        = UnivEq.emptySet[TagGroupId]
    private var tagGroupsDR        = UnivEq.emptySet[TagGroupId]
    private var applicableTagsCU   = UnivEq.emptySet[ApplicableTagId]
    private var applicableTagsDR   = UnivEq.emptySet[ApplicableTagId]
    private var staticFields       = UnivEq.emptySet[StaticField]
    private var genericReqs        = UnivEq.emptySet[GenericReqId]
    private var useCasesExclSteps  = UnivEq.emptySet[UseCaseId]
    private var useCaseSteps       = UnivEq.emptySet[UseCaseStepId]
    private var reqCodeGroupsCU    = UnivEq.emptySet[ReqCodeGroupId]
    private var reqCodeGroupsDR    = UnivEq.emptySet[ReqCodeGroupId]
    private var contentLiveDeps    = false

    private val addReq: ReqId => Unit = {
      case i: GenericReqId => genericReqs       += i
      case i: UseCaseId    => useCasesExclSteps += i
    }

    private val tagDR: TagId => Unit = {
      case i: TagGroupId      => tagGroupsDR      += i
      case i: ApplicableTagId => applicableTagsDR += i
    }

    def ++=(events: TraversableOnce[Event]): Unit =
      events.foreach(+=)
    
    val += : Event => Unit = {

      case e: Event.ContentRestore =>
        e.reqs.foreach(addReq)
        reqCodeGroupsDR ++= e.codeGroups

      case e: Event.ReqsDelete =>
        e.reqs.foreach(addReq)
        reqCodeGroupsDR ++= e.codeGroups

      case e: Event.ReqImplicationsPatch =>
        addReq(e.id)
        e.patch.added.foreach(addReq)
        e.patch.removed.foreach(addReq)

      case e: Event.CustomReqTypeDelete =>
        customReqTypesDR += e.id
        contentLiveDeps = true

      case e: Event.CustomReqTypeRestore =>
        customReqTypesDR += e.id
        contentLiveDeps = true

      case e: Event.GenericReqTypeSet =>
        genericReqs += e.id
        contentLiveDeps = true

      case e: Event.ApplicableTagCreate    => applicableTagsCU += e.id
      case e: Event.ApplicableTagUpdate    => applicableTagsCU += e.id
      case e: Event.CodeGroupCreate        => reqCodeGroupsCU += e.id
      case e: Event.CodeGroupsDelete       => reqCodeGroupsDR ++= e.ids.whole
      case e: Event.CodeGroupUpdate        => reqCodeGroupsCU += e.id
      case e: Event.CustomIssueTypeCreate  => customIssueTypesCU += e.id
      case e: Event.CustomIssueTypeDelete  => customIssueTypesDR += e.id
      case e: Event.CustomIssueTypeRestore => customIssueTypesDR += e.id
      case e: Event.CustomIssueTypeUpdate  => customIssueTypesCU += e.id
      case e: Event.CustomReqTypeCreate    => customReqTypesCU += e.id
      case e: Event.CustomReqTypeUpdate    => customReqTypesCU += e.id
      case e: Event.FieldCustomDelete      => customFieldTypesDR += e.id
      case e: Event.FieldCustomImpCreate   => customFieldTypesCU += e.id
      case e: Event.FieldCustomImpUpdate   => customFieldTypesCU += e.id
      case e: Event.FieldCustomRestore     => customFieldTypesDR += e.id
      case e: Event.FieldCustomTagCreate   => customFieldTypesCU += e.id
      case e: Event.FieldCustomTagUpdate   => customFieldTypesCU += e.id
      case e: Event.FieldCustomTextCreate  => customFieldTypesCU += e.id
      case e: Event.FieldCustomTextUpdate  => customFieldTypesCU += e.id
      case e: Event.FieldStaticAdd         => staticFields += e.f
      case e: Event.FieldStaticRemove      => staticFields += e.f
      case e: Event.GenericReqCreate       => genericReqs += e.id
      case e: Event.GenericReqTitleSet     => genericReqs += e.id
      case e: Event.ProjectTemplateApply   => this ++= e.template.events
      case e: Event.ReqCodesPatch          => addReq(e.id)
      case e: Event.ReqFieldCustomTextSet  => addReq(e.id)
      case e: Event.ReqTagsPatch           => addReq(e.id)
      case e: Event.TagDelete              => tagDR(e.id)
      case e: Event.TagGroupCreate         => tagGroupsCU += e.id
      case e: Event.TagGroupUpdate         => tagGroupsCU += e.id
      case e: Event.TagRestore             => tagDR(e.id)
      case e: Event.UseCaseCreate          => useCasesExclSteps += e.id
      case e: Event.UseCaseStepCreate      => useCaseSteps += e.id
      case e: Event.UseCaseStepDelete      => useCaseSteps += e.id
      case e: Event.UseCaseStepRestore     => useCaseSteps += e.id
      case e: Event.UseCaseStepShiftLeft   => useCaseSteps += e.id
      case e: Event.UseCaseStepShiftRight  => useCaseSteps += e.id
      case e: Event.UseCaseStepUpdate      => useCaseSteps += e.id
      case e: Event.UseCaseTitleSet        => useCasesExclSteps += e.id

      case _: Event.FieldReposition
         | _: Event.ProjectNameSet
         | _: Event.SavedViewCreate
         | _: Event.SavedViewDefaultSet
         | _: Event.SavedViewDelete
         | _: Event.SavedViewUpdate        => ()
    }
    
    def result(): EventSeqSummary =
      EventSeqSummary(
        customIssueTypesCU = customIssueTypesCU,
        customIssueTypesDR = customIssueTypesDR,
        customFieldTypesCU = customFieldTypesCU,
        customFieldTypesDR = customFieldTypesDR,
        customReqTypesCU   = customReqTypesCU,
        customReqTypesDR   = customReqTypesDR,
        tagGroupsCU        = tagGroupsCU,
        tagGroupsDR        = tagGroupsDR,
        applicableTagsCU   = applicableTagsCU,
        applicableTagsDR   = applicableTagsDR,
        staticFields       = staticFields,
        genericReqs        = genericReqs,
        useCasesExclSteps  = useCasesExclSteps,
        useCaseSteps       = useCaseSteps,
        reqCodeGroupsCU    = reqCodeGroupsCU,
        reqCodeGroupsDR    = reqCodeGroupsDR,
        contentLiveDeps    = contentLiveDeps,
      )
  }

}