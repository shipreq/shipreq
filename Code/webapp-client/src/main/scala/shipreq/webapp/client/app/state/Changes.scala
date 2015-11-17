package shipreq.webapp.client.app.state

import scalaz.Equal
import scalaz.syntax.equal._
import shipreq.base.util.UnivEq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._

case class Changes(ves: VerifiedEvents, p1: Project, p2: Project) {
  private var _customReqTypes  : Set[CustomReqTypeId  ] = emptySet
  private var _customIssueTypes: Set[CustomIssueTypeId] = emptySet
  private var _customFieldTypes: Set[CustomFieldId    ] = emptySet
  private var _tagsChanged  = false
  private var _staticFields = false

  for (ve <- ves)
    ve.event match {
      case e: CreateCustomIssueType => _customIssueTypes += e.id
      case e: UpdateCustomIssueType => _customIssueTypes += e.id
      case e: DeleteCustomIssueType => _customIssueTypes += e.id
      case e: CreateCustomReqType   => _customReqTypes += e.id
      case e: UpdateCustomReqType   => _customReqTypes += e.id
      case e: DeleteCustomReqType   => _customReqTypes += e.id // Affects GenericReq.live & ReqCodes
      case _: CreateApplicableTag
         | _: UpdateApplicableTag
         | _: CreateTagGroup
         | _: UpdateTagGroup
         | _: DeleteTag             => _tagsChanged = true
      case e: CreateCustomTextField => _customFieldTypes += e.id
      case e: UpdateCustomTextField => _customFieldTypes += e.id
      case e: CreateCustomTagField  => _customFieldTypes += e.id
      case e: UpdateCustomTagField  => _customFieldTypes += e.id
      case e: CreateCustomImpField  => _customFieldTypes += e.id
      case e: UpdateCustomImpField  => _customFieldTypes += e.id
      case e: DeleteCustomField     => _customFieldTypes += e.id
      case e: DeleteStaticField     => _staticFields = true
      case e: AddStaticField        => _staticFields = true

      case _: AddUseCaseStep
         | _: CreateGenericReq
         | _: CreateReqCodeGroup
         | _: CreateUseCase
         | _: DeleteReqCodeGroups
         | _: DeleteReqs
         | _: DeleteUseCaseStep
         | _: PatchImplicationSrc
         | _: PatchImplicationTgt
         | _: PatchReqCodes
         | _: PatchReqTags
         | _: RepositionField
         | _: RestoreContent
         | _: SetCustomTextField
         | _: SetGenericReqTitle
         | _: SetGenericReqType
         | _: SetUseCaseTitle
         | _: ShiftUseCaseStepLeft
         | _: ShiftUseCaseStepRight
         | _: UpdateReqCodeGroup
         | _: UpdateUseCaseStep

         | _: ApplyTemplate         => () // Always event #0 only - ignore
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
      compareMaps(_.config.tags)(_.keySet)((_,m,i) => m need i)
    else
      emptySet

  val fieldNames: Boolean =
    customFieldTypes.nonEmpty || customReqTypes.nonEmpty || tags.nonEmpty
}
