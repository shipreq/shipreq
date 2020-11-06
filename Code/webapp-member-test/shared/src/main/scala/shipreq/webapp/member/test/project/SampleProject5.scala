package shipreq.webapp.member.test.project

import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text._
import shipreq.webapp.member.test.project.ProjectDsl._
import shipreq.webapp.member.test.project.SampleProject4.{project => project0}
import shipreq.webapp.member.test.project.UnsafeTypes._

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
