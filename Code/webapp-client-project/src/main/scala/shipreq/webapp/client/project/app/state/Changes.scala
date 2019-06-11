package shipreq.webapp.client.project.app.state

import scalaz.Equal
import scalaz.syntax.equal._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import UnivEq.emptySet

case class Changes(ves: VerifiedEvent.NonEmptySeq, p1: Project, p2: Project) {
  private var _customReqTypes  : Set[CustomReqTypeId  ] = emptySet
  private var _customIssueTypes: Set[CustomIssueTypeId] = emptySet
  private var _customFieldTypes: Set[CustomFieldId    ] = emptySet
  private var _tagsChanged  = false
  private var _staticFields = false

  for (ve <- ves.values)
    ve.event match {
      case e: CustomIssueTypeCreate  => _customIssueTypes += e.id
      case e: CustomIssueTypeDelete  => _customIssueTypes += e.id
      case e: CustomIssueTypeRestore => _customIssueTypes += e.id
      case e: CustomIssueTypeUpdate  => _customIssueTypes += e.id
      case e: CustomReqTypeCreate    => _customReqTypes += e.id
      case e: CustomReqTypeDelete    => _customReqTypes += e.id // Affects GenericReq.live & ReqCodes
      case e: CustomReqTypeRestore   => _customReqTypes += e.id // Affects GenericReq.live & ReqCodes
      case e: CustomReqTypeUpdate    => _customReqTypes += e.id
      case _: ApplicableTagCreate
         | _: ApplicableTagUpdate
         | _: TagGroupCreate
         | _: TagGroupUpdate
         | _: TagDelete
         | _: TagRestore             => _tagsChanged = true
      case e: FieldCustomDelete      => _customFieldTypes += e.id
      case e: FieldCustomImpCreate   => _customFieldTypes += e.id
      case e: FieldCustomImpUpdate   => _customFieldTypes += e.id
      case e: FieldCustomRestore     => _customFieldTypes += e.id
      case e: FieldCustomTagCreate   => _customFieldTypes += e.id
      case e: FieldCustomTagUpdate   => _customFieldTypes += e.id
      case e: FieldCustomTextCreate  => _customFieldTypes += e.id
      case e: FieldCustomTextUpdate  => _customFieldTypes += e.id
      case e: FieldStaticAdd         => _staticFields = true
      case e: FieldStaticRemove      => _staticFields = true

      case _: ContentRestore
         | _: FieldReposition
         | _: GenericReqCreate
         | _: GenericReqTitleSet
         | _: GenericReqTypeSet
         | _: ProjectNameSet
         | _: CodeGroupCreate
         | _: CodeGroupsDelete
         | _: CodeGroupUpdate
         | _: ReqCodesPatch
         | _: ReqFieldCustomTextSet
         | _: ReqImplicationsPatch
         | _: ReqsDelete
         | _: ReqTagsPatch
         | _: SavedViewCreate
         | _: SavedViewDefaultSet
         | _: SavedViewDelete
         | _: SavedViewUpdate
         | _: UseCaseCreate
         | _: UseCaseStepCreate
         | _: UseCaseStepDelete
         | _: UseCaseStepRestore
         | _: UseCaseStepShiftLeft
         | _: UseCaseStepShiftRight
         | _: UseCaseStepUpdate
         | _: UseCaseTitleSet

         | _: ProjectTemplateApply => () // Always event #0 only - ignore
    }

  private def changed[A: Equal](f: Project => A): Boolean =
    f(p1) ≠ f(p2)

  private def compareMaps[Map, Id, Data: Equal](m: Project => Map)(ids: Map => Set[Id])(data: (Project, Map, Id) => Data): Set[Id] = {
    val m1       = m(p1)
    val m2       = m(p2)
    val ids1     = ids(m1)
    val ids2     = ids(m2)
    val common   = ids1 & ids2
    val newOrDel = (ids1 | ids2) &~ common
    val changes  = common.filter(id => data(p1, m1, id) ≠ data(p2, m2, id))
    changes | newOrDel
  }

  /** Excludes field position */
  val customFieldTypes =
    _customFieldTypes | compareMaps(_.config.fields.customFields)(_.keySet)((p,m,i) => m.need(i).live(p.config))

  val staticFields = _staticFields

  /** Excludes addition/removal of fields */
  val fieldOrder = changed(_.config.fields.order)

  val customReqTypes   = _customReqTypes
  val customIssueTypes = _customIssueTypes

  val tags: Set[TagId] =
    if (_tagsChanged)
      compareMaps(_.config.tags.tree)(_.keySet)((_, m, i) => m need i)
    else
      emptySet

  val fieldNames: Boolean =
    customFieldTypes.nonEmpty || customReqTypes.nonEmpty || tags.nonEmpty
}
