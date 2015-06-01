package shipreq.webapp.base.test

import monocle._
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.Optics

object TestOptics {

  val customReqTypesAlive: Traversal[Project, Alive] =
    Project.customReqTypes ^|->
    RevAnd.data            ^|->>
    Optics.imapTraversal   ^|->
    CustomReqType.alive
}
