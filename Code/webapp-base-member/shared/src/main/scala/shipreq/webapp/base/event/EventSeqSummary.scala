package shipreq.webapp.base.event

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.univeq._
import shipreq.base.util.Util.mergeSets
import shipreq.webapp.base.data._

/** Summary of a sequence of events.
  *
  * Where there are CU and DR suffixes, CU = created/updated, DR = deleted/restored.
  */
final case class EventSeqSummary(
    customIssueTypes    : EventSeqSummary.CUDR[CustomIssueTypeId],
    customFieldImpTypes : EventSeqSummary.CUDR[CustomField.Implication.Id],
    customFieldTagTypes : EventSeqSummary.CUDR[CustomField.Tag.Id],
    customFieldTextTypes: EventSeqSummary.CUDR[CustomField.Text.Id],
    customReqTypes      : EventSeqSummary.CUDR[CustomReqTypeId],
    tagGroups           : EventSeqSummary.CUDR[TagGroupId],
    applicableTags      : EventSeqSummary.CUDR[ApplicableTagId],
    reqCodeGroups       : EventSeqSummary.CUDR[ReqCodeGroupId],
    staticFields        : EventSeqSummary.CUDR[StaticField],
    genericReqs         : EventSeqSummary.CUDR[GenericReqId],
    useCasesExclSteps   : EventSeqSummary.CUDR[UseCaseId],
    useCaseSteps        : EventSeqSummary.CUDR[UseCaseStepId],
    apReqCodes          : Boolean,
    contentLiveDeps     : Boolean,
    fieldReposition     : Boolean,
    ) {

  override def toString =
    s"""
       |EventSeqSummary(
       |  customIssueTypes     = ${customIssueTypes    .show(_.value)}
       |  customFieldImpTypes  = ${customFieldImpTypes .show(_.value)}
       |  customFieldTagTypes  = ${customFieldTagTypes .show(_.value)}
       |  customFieldTextTypes = ${customFieldTextTypes.show(_.value)}
       |  customReqTypes       = ${customReqTypes      .show(_.value)}
       |  tagGroups            = ${tagGroups           .show(_.value)}
       |  applicableTags       = ${applicableTags      .show(_.value)}
       |  reqCodeGroups        = ${reqCodeGroups       .show(_.value)}
       |  staticFields         = ${staticFields        .show(_.toString)}
       |  genericReqs          = ${genericReqs         .show(_.value)}
       |  useCasesExclSteps    = ${useCasesExclSteps   .show(_.value)}
       |  useCaseSteps         = ${useCaseSteps        .show(_.value)}
       |  apReqCodes           = $apReqCodes
       |  contentLiveDeps      = $contentLiveDeps
       |  fieldReposition      = $fieldReposition
       |  hasTags              = $hasTags)
     """.stripMargin

  val hasTagsCU: Boolean =
    applicableTags.hasCU || tagGroups.hasCU

  val hasTagsDR: Boolean =
    applicableTags.hasDR || tagGroups.hasDR

  val hasTags: Boolean =
    hasTagsCU || hasTagsDR

  lazy val allTags: Set[TagId] =
    mergeSets(applicableTags.all, tagGroups.all)

  lazy val allCustomFieldTypes: Set[CustomFieldId] =
    mergeSets(customFieldTextTypes.all, customFieldTagTypes.all, customFieldImpTypes.all)

  val fieldNamesChanged: Boolean =
    hasTags || customReqTypes.hasCU ||
      customFieldImpTypes.hasAny || customFieldTagTypes.hasAny || customFieldTextTypes.hasAny

  lazy val reqsExclUseCaseSteps: Set[ReqId] =
    mergeSets(genericReqs.all, useCasesExclSteps.all)

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
      summary.useCaseSteps.all.map(stepIndex(_).useCaseId)
    }

    lazy val useCases: Set[UseCaseId] =
      mergeSets(summary.useCasesExclSteps.all, useCasesChangedBySteps)

    lazy val reqsAffectedByReqTypeChanges: Set[ReqId] =
      summary.customReqTypes.DR.flatMap(project.content.reqs.reqsByType(_).iterator.map(_.id))

    lazy val reqs: Set[ReqId] =
      mergeSets(summary.reqsExclUseCaseSteps, useCasesChangedBySteps, reqsAffectedByReqTypeChanges)
  }

  implicit def exportSummaryToWithProject(w: WithProject) = w.summary

  // ===================================================================================================================

  final case class CUDR[A: UnivEq](created : Set[A],
                                   updated : Set[A],
                                   deleted : Set[A],
                                   restored: Set[A]) {

    override def toString = show(identity)

    def show(showValue: A => Any) = {
      def show(as: Set[A]) = MutableArray(as.iterator.map("" + showValue(_))).sort.mkString("{", ",", "}")
      s"CUDR(C=${show(created)}, U=${show(updated)}, D=${show(deleted)}, R=${show(restored)})"
    }

    val CU  = mergeSets(created, updated)
    val DR  = mergeSets(deleted, restored)
    val all = mergeSets(CU, DR)

    @inline def hasC   = created.nonEmpty
    @inline def hasU   = updated.nonEmpty
    @inline def hasD   = deleted.nonEmpty
    @inline def hasR   = restored.nonEmpty
    @inline def hasCU  = CU.nonEmpty
    @inline def hasDR  = DR.nonEmpty
    @inline def hasAny = all.nonEmpty
  }

  object CUDR {
    private val _empty = new CUDR(Set.empty, Set.empty, Set.empty, Set.empty)(UnivEq.force)
    def empty[A: UnivEq]: CUDR[A] = _empty.asInstanceOf[CUDR[A]]

    sealed trait Field
    object Field {
      case object Created extends Field
      case object Updated extends Field
      case object Deleted extends Field
      case object Restored extends Field
    }

    final class Mutable[A: UnivEq] {
      var created : Set[A] = Set.empty
      var updated : Set[A] = Set.empty
      var deleted : Set[A] = Set.empty
      var restored: Set[A] = Set.empty

      def add(f: Field, a: A): Unit =
        f match {
          case Field.Created  => created  += a
          case Field.Updated  => updated  += a
          case Field.Deleted  => deleted  += a
          case Field.Restored => restored += a
        }

      def result(): CUDR[A] =
        if (created.isEmpty && updated.isEmpty && deleted.isEmpty && restored.isEmpty)
          CUDR.empty
        else
          CUDR(
            created  = created,
            updated  = updated,
            deleted  = deleted,
            restored = restored)
    }
  }

  private final class MutableBuilder {
    private[this] val customIssueTypes     = new CUDR.Mutable[CustomIssueTypeId]
    private[this] val customFieldImpTypes  = new CUDR.Mutable[CustomField.Implication.Id]
    private[this] val customFieldTagTypes  = new CUDR.Mutable[CustomField.Tag.Id]
    private[this] val customFieldTextTypes = new CUDR.Mutable[CustomField.Text.Id]
    private[this] val customReqTypes       = new CUDR.Mutable[CustomReqTypeId]
    private[this] val tagGroups            = new CUDR.Mutable[TagGroupId]
    private[this] val applicableTags       = new CUDR.Mutable[ApplicableTagId]
    private[this] val reqCodeGroups        = new CUDR.Mutable[ReqCodeGroupId]
    private[this] val staticFields         = new CUDR.Mutable[StaticField]
    private[this] val genericReqs          = new CUDR.Mutable[GenericReqId]
    private[this] val useCasesExclSteps    = new CUDR.Mutable[UseCaseId]
    private[this] val useCaseSteps         = new CUDR.Mutable[UseCaseStepId]
    private var apReqCodes                 = false
    private var contentLiveDeps            = false
    private var fieldReposition            = false

    import CUDR.Field._

    private def customFieldType(f: CUDR.Field, id: CustomFieldId): Unit = id match {
      case i: CustomField.Implication.Id => customFieldImpTypes .add(f, i)
      case i: CustomField.Tag.Id         => customFieldTagTypes .add(f, i)
      case i: CustomField.Text.Id        => customFieldTextTypes.add(f, i)
    }

    private def req(f: CUDR.Field, id: ReqId): Unit = id match {
      case i: GenericReqId => genericReqs      .add(f, i)
      case i: UseCaseId    => useCasesExclSteps.add(f, i)
    }

    private def tag(f: CUDR.Field, id: TagId): Unit = id match {
      case i: TagGroupId      => tagGroups     .add(f, i)
      case i: ApplicableTagId => applicableTags.add(f, i)
    }

    def ++=(events: TraversableOnce[Event]): Unit =
      events.foreach(+=)

    val += : Event => Unit = {

      case e: Event.ContentRestore =>
        e.reqs.foreach(req(Restored, _))
        reqCodeGroups.restored ++= e.codeGroups

      case e: Event.ReqsDelete =>
        e.reqs.foreach(req(Deleted, _))
        reqCodeGroups.deleted ++= e.codeGroups

      case e: Event.ReqImplicationsPatch =>
        req(Updated, e.id)
        e.patch.added.foreach(req(Updated, _))
        e.patch.removed.foreach(req(Updated, _))

      case e: Event.CustomReqTypeDelete =>
        customReqTypes.deleted += e.id
        contentLiveDeps = true

      case e: Event.CustomReqTypeRestore =>
        customReqTypes.restored += e.id
        contentLiveDeps = true

      case e: Event.GenericReqTypeSet =>
        genericReqs.updated += e.id
        contentLiveDeps = true

      case e: Event.ReqCodesPatch =>
        req(Updated, e.id)
        apReqCodes = true

      case e: Event.ApplicableTagCreate    => applicableTags.created += e.id
      case e: Event.ApplicableTagUpdate    => applicableTags.updated += e.id
      case e: Event.CodeGroupCreate        => reqCodeGroups.created += e.id
      case e: Event.CodeGroupsDelete       => reqCodeGroups.deleted ++= e.ids.whole
      case e: Event.CodeGroupUpdate        => reqCodeGroups.updated += e.id
      case e: Event.CustomIssueTypeCreate  => customIssueTypes.created += e.id
      case e: Event.CustomIssueTypeDelete  => customIssueTypes.deleted += e.id
      case e: Event.CustomIssueTypeRestore => customIssueTypes.restored += e.id
      case e: Event.CustomIssueTypeUpdate  => customIssueTypes.updated += e.id
      case e: Event.CustomReqTypeCreate    => customReqTypes.created += e.id
      case e: Event.CustomReqTypeUpdate    => customReqTypes.updated += e.id
      case e: Event.FieldCustomDelete      => customFieldType(Deleted, e.id)
      case e: Event.FieldCustomImpCreate   => customFieldImpTypes.created += e.id
      case e: Event.FieldCustomImpUpdate   => customFieldImpTypes.updated += e.id
      case e: Event.FieldCustomRestore     => customFieldType(Restored, e.id)
      case e: Event.FieldCustomTagCreate   => customFieldTagTypes.created += e.id
      case e: Event.FieldCustomTagUpdate   => customFieldTagTypes.updated += e.id
      case e: Event.FieldCustomTextCreate  => customFieldTextTypes.created += e.id
      case e: Event.FieldCustomTextUpdate  => customFieldTextTypes.updated += e.id
      case _: Event.FieldReposition        => fieldReposition = true
      case e: Event.FieldStaticAdd         => staticFields.created += e.f
      case e: Event.FieldStaticRemove      => staticFields.deleted += e.f
      case e: Event.GenericReqCreate       => genericReqs.created += e.id
      case e: Event.GenericReqTitleSet     => genericReqs.updated += e.id
      case e: Event.ProjectTemplateApply   => this ++= e.template.events
      case e: Event.ReqFieldCustomTextSet  => req(Updated, e.id)
      case e: Event.ReqTagsPatch           => req(Updated, e.id)
      case e: Event.TagDelete              => tag(Deleted, e.id)
      case e: Event.TagGroupCreate         => tagGroups.created += e.id
      case e: Event.TagGroupUpdate         => tagGroups.updated += e.id
      case e: Event.TagRestore             => tag(Restored, e.id)
      case e: Event.UseCaseCreate          => useCasesExclSteps.created += e.id
      case e: Event.UseCaseStepCreate      => useCaseSteps.created += e.id
      case e: Event.UseCaseStepDelete      => useCaseSteps.deleted += e.id
      case e: Event.UseCaseStepRestore     => useCaseSteps.restored += e.id
      case e: Event.UseCaseStepShiftLeft   => useCaseSteps.updated += e.id // ?
      case e: Event.UseCaseStepShiftRight  => useCaseSteps.updated += e.id // ?
      case e: Event.UseCaseStepUpdate      => useCaseSteps.updated += e.id
      case e: Event.UseCaseTitleSet        => useCasesExclSteps.updated += e.id

      case _: Event.ProjectNameSet
         | _: Event.ManualIssueCreate
         | _: Event.ManualIssueDelete
         | _: Event.ManualIssueUpdate
         | _: Event.SavedViewCreate
         | _: Event.SavedViewDefaultSet
         | _: Event.SavedViewDelete
         | _: Event.SavedViewUpdate        => ()
    }

    def result(): EventSeqSummary =
      EventSeqSummary(
        customIssueTypes     = customIssueTypes    .result(),
        customFieldImpTypes  = customFieldImpTypes .result(),
        customFieldTagTypes  = customFieldTagTypes .result(),
        customFieldTextTypes = customFieldTextTypes.result(),
        customReqTypes       = customReqTypes      .result(),
        tagGroups            = tagGroups           .result(),
        applicableTags       = applicableTags      .result(),
        reqCodeGroups        = reqCodeGroups       .result(),
        staticFields         = staticFields        .result(),
        genericReqs          = genericReqs         .result(),
        useCasesExclSteps    = useCasesExclSteps   .result(),
        useCaseSteps         = useCaseSteps        .result(),
        apReqCodes           = apReqCodes,
        contentLiveDeps      = contentLiveDeps,
        fieldReposition      = fieldReposition,
      )
  }

}