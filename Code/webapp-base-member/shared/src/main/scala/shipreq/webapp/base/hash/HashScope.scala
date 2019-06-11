package shipreq.webapp.base.hash

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.univeq.UnivEq
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.feature.hash.HashFn

sealed abstract class HashScope

object HashScope {
  sealed abstract class WithProjectAccess[@specialized(Int, Long, Char, Boolean) A](f: Project => A) extends HashScope {
    final def -->(h: HashFn[A]): (this.type, HashFn[Project]) =
      (this, h contramap f)
  }

  case object ProjectName     extends WithProjectAccess(_.name)
  case object CfgIssueTypes   extends WithProjectAccess(_.config.customIssueTypes)
  case object CfgReqTypes     extends WithProjectAccess(_.config.reqTypes)
  case object CfgFields       extends WithProjectAccess(_.config.fields)
  case object CfgTags         extends WithProjectAccess(_.config.tags.tree)
  case object GenericReqs     extends WithProjectAccess(_.content.reqs.genericReqs)
  case object UseCases        extends WithProjectAccess(_.content.reqs.useCases)
  case object PubidRegister   extends WithProjectAccess(_.content.reqs.pubids)
  case object ReqCodes        extends WithProjectAccess(_.content.reqCodes)
  case object TextFieldData   extends WithProjectAccess(_.content.reqText)
  case object TagData         extends WithProjectAccess(_.content.reqTags)
  case object ImplicationData extends WithProjectAccess(_.content.implications)
  case object DeletionReasons extends WithProjectAccess(_.content.deletionReasons)
  case object SavedViews      extends WithProjectAccess(_.reqtableViews)

  implicit def univEq: UnivEq[HashScope] = UnivEq.force

  val all = AdtMacros.adtValues[HashScope]
}
