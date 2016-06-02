package shipreq.webapp.base.hash

import japgolly.univeq.UnivEq
import shipreq.base.util._
import shipreq.webapp.base.data.Project

sealed abstract class HashScope

object HashScope {
  case object WholeProject        extends HashScope
  case object   Config            extends HashScope
  case object     CfgIssueTypes   extends HashScope
  case object     CfgReqTypes     extends HashScope
  case object     CfgFields       extends HashScope
  case object     CfgTags         extends HashScope
  case object   Content           extends HashScope
  case object     Reqs            extends HashScope
  case object       GenericReqs   extends HashScope
  case object       UseCases      extends HashScope
  case object       PubidRegister extends HashScope
  case object     ReqCodes        extends HashScope
  case object     TextFieldData   extends HashScope
  case object     TagData         extends HashScope
  case object     ImplicationData extends HashScope
  case object     DeletionReasons extends HashScope
  case object   Other             extends HashScope

  implicit def equality: UnivEq[HashScope] = UnivEq.derive

  val all: NonEmptyVector[HashScope] =
    UtilMacros.adtValues[HashScope]

  val defaultSet = NonEmptySet[HashScope](
    CfgIssueTypes  ,
    CfgReqTypes    ,
    CfgFields      ,
    CfgTags        ,
    GenericReqs    ,
    UseCases       ,
    PubidRegister  ,
    ReqCodes       ,
    TextFieldData  ,
    TagData        ,
    ImplicationData,
    DeletionReasons,
    Other          )

  def hash(scope: HashScope, h: DataHasher, p: Project): Int =
    scope match {
      case WholeProject    => h.hashProject          hash p
      case Config          => h.hashProjectConfig    hash p.config
      case CfgIssueTypes   => h.hashCustomIssueTypes hash p.config.customIssueTypes
      case CfgReqTypes     => h.hashReqTypes         hash p.config.reqTypes
      case CfgFields       => h.hashFieldSet         hash p.config.fields
      case CfgTags         => h.hashTagTree          hash p.config.tags
      case Content         => h.hashProjectContent   hash p
      case Reqs            => h.hashRequirements     hash p.reqs
      case GenericReqs     => h.hashGenericReqs      hash p.reqs.genericReqs
      case UseCases        => h.hashUseCases         hash p.reqs.useCases
      case PubidRegister   => h.hashPubidRegister    hash p.reqs.pubids
      case ReqCodes        => h.hashReqCodes         hash p.reqCodes
      case TextFieldData   => h.hashReqDataText      hash p.reqText
      case TagData         => h.hashReqDataTags      hash p.reqTags
      case ImplicationData => h.hashImplications     hash p.implications
      case DeletionReasons => h.hashDeletionReasons  hash p.deletionReasons
      case Other           => h.hashProjectOther     hash p
    }
}