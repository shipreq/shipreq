package shipreq.webapp.base.test

import shipreq.webapp.base.data._
import shipreq.webapp.base.test.ProjectDsl._
import shipreq.webapp.base.test.SampleProject4.{project => project0}
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text._

/**
 * Builds on SampleProject #4 to add:
 *   - SI-1: implicitly dead, explicitly live
 *   - SI-2: implicitly dead, explicitly dead
 */
object SampleProject5 {

  trait Values extends SampleProject4.Values {
    val sis = (0 to 2).toVector.map(i => GenericReqId(i + 1300))
  }

  object Values extends Values
  import Values._

  lazy val project = {
    ( GReq(reqType = si, id = sis(1), title = "Outsource to Johnno.").cftextS(descField, "Johnno gets shit done.")
    + GReq(reqType = si, id = sis(2), title = "Pack up and go home.", live = Dead)
    ) ! project0
  }

  lazy val plainText  = PlainText.ForProject.noCtx(project)
  lazy val textSearch = TextSearch(project, plainText)
}
