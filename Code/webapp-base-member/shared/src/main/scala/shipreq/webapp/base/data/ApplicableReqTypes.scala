package shipreq.webapp.base.data

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.univeq.UnivEq
import shipreq.base.util._

final case class ApplicableReqTypes(applicability: Applicability,
                                    reqTypes     : NonEmptySet[ReqTypeId])

object ApplicableReqTypes {
  implicit def univEq: UnivEq[ApplicableReqTypes] = UnivEq.derive
}