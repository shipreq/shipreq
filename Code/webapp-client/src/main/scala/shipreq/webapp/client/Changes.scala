package shipreq.webapp.client

import shipreq.base.util.UnivEq.emptySet
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._

case class Changes(ves: VerifiedEvents, p1: Project, p2: Project) {

  private var _customReqTypes  : Set[CustomReqTypeId  ] = emptySet
  private var _customIssueTypes: Set[CustomIssueTypeId] = emptySet
  private var _customFieldTypes: Set[CustomFieldId    ] = emptySet
  private var _tags            : Set[TagId            ] = emptySet
  private var _fieldOrder   = false
  private var _staticFields = false

  for (ve <- ves)
    ve.event match {
      case e: CreateCustomIssueType => _customIssueTypes += e.id
      case e: UpdateCustomIssueType => _customIssueTypes += e.id
      case e: DeleteCustomIssueType => _customIssueTypes += e.id
      case e: CreateCustomReqType   => _customReqTypes += e.id
      case e: UpdateCustomReqType   => _customReqTypes += e.id
      case e: DeleteCustomReqType   => _customReqTypes += e.id
      case e: CreateApplicableTag   => _tags += e.id
      case e: UpdateApplicableTag   => _tags += e.id
      case e: CreateTagGroup        => _tags += e.id
      case e: UpdateTagGroup        => _tags += e.id
      case e: DeleteTag             => _tags += e.id
      case e: CreateCustomTextField => _customFieldTypes += e.id
      case e: UpdateCustomTextField => _customFieldTypes += e.id
      case e: CreateCustomTagField  => _customFieldTypes += e.id
      case e: UpdateCustomTagField  => _customFieldTypes += e.id
      case e: CreateCustomImpField  => _customFieldTypes += e.id
      case e: UpdateCustomImpField  => _customFieldTypes += e.id
      case e: DeleteCustomField     => _customFieldTypes += e.id
      case e: DeleteStaticField     => _staticFields = true
      case e: AddStaticField        => _staticFields = true
      case e: RepositionField       => _fieldOrder = true

      case e: CreateGenericReq      =>
      case e: PatchReqCodes         =>
      case e: PatchReqTags          =>
      case e: PatchImplicationSrc   =>
      case e: PatchImplicationTgt   =>
      case e: SetGenericReqTitle    =>
      case e: SetGenericReqType     =>
      case e: SetCustomTextField    =>
      case e: DeleteReq             =>
      case e: CreateReqCodeGroup    =>
      case e: UpdateReqCodeGroup    =>
      case e: DeleteReqCodeGroup    =>
    }

  /** Excludes field position */
  val customFieldTypes = _customFieldTypes

  val staticFields = _staticFields

  /** Excludes addition/removal of fields */
  val fieldOrder = _fieldOrder

  val customReqTypes   = _customReqTypes
  val customIssueTypes = _customIssueTypes
  val tags             = _tags

  val fieldNames: Boolean =
    customFieldTypes.nonEmpty || customReqTypes.nonEmpty || tags.nonEmpty
}
