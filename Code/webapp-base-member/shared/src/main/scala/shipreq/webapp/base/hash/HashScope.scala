package shipreq.webapp.base.hash

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty._
import japgolly.univeq.UnivEq

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
    AdtMacros.adtValues[HashScope]

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
}