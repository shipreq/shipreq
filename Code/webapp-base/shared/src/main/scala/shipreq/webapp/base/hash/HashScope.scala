package shipreq.webapp.base.hash

import shipreq.base.util._
import shipreq.webapp.base.data.Project

sealed trait HashScope

object HashScope {
  case object WholeProject    extends HashScope
  case object CfgIssueTypes   extends HashScope
  case object CfgReqTypes     extends HashScope
  case object CfgFields       extends HashScope
  case object CfgTags         extends HashScope
  case object Reqs            extends HashScope
  case object ReqCodes        extends HashScope
  case object TextFieldData   extends HashScope
  case object TagData         extends HashScope
  case object ImplicationData extends HashScope
  case object DeletionReasons extends HashScope

  implicit def equality: UnivEq[HashScope] = UnivEq.derive

  val all: NonEmptyVector[HashScope] =
    UtilMacros.adtValues[HashScope]

  val defaultSet: NonEmptySet[HashScope] =
    NonEmptySet(
      CfgIssueTypes  ,
      CfgReqTypes    ,
      CfgFields      ,
      CfgTags        ,
      Reqs           ,
      ReqCodes       ,
      TextFieldData  ,
      TagData        ,
      ImplicationData,
      DeletionReasons)

  def overlap(a: HashScope, b: HashScope): Boolean =
    (a == b) ||
    (a == WholeProject || b == WholeProject)

  def hash(scope: HashScope, h: DataHasher, p: Project): Int =
    scope match {
      case WholeProject    => h.hashProject          hash p
      case CfgIssueTypes   => h.hashCustomIssueTypes hash p.config.customIssueTypes
      case CfgReqTypes     => h.hashCustomReqTypes   hash p.config.customReqTypes
      case CfgFields       => h.hashFieldSet         hash p.config.fields
      case CfgTags         => h.hashTagTree          hash p.config.tags
      case Reqs            => h.hashRequirements     hash p.reqs
      case ReqCodes        => h.hashReqCodes         hash p.reqCodes
      case TextFieldData   => h.hashReqDataText      hash p.reqText
      case TagData         => h.hashReqDataTags      hash p.reqTags
      case ImplicationData => h.hashImplications     hash p.implications
      case DeletionReasons => h.hashDeletionReasons  hash p.deletionReasons
    }
}