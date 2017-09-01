package shipreq.webapp.base.test

import shipreq.webapp.base.data._
import shipreq.webapp.base.text._
import ProjectDsl._
import SampleProject4.{project => project0}
import SampleProject.Values._
import UnsafeTypes._

/**
 * Builds on SampleProject #4 to add:
 *   - SI-1: implicitly dead, explicitly live
 *   - SI-2: implicitly dead, explicitly dead
 */
object SampleProject5 {

  type     Values = SampleProject4.Values
  lazy val Values = SampleProject4.Values

  lazy val project = {
    val sis = (0 to 2).toVector.map(i => GenericReqId(i + 1300))
    ( GReq(reqType = si, id = sis(1), title = "Outsource to Johnno.").cftextS(descField, "Johnno gets shit done.")
    + GReq(reqType = si, id = sis(2), title = "Pack up and go home.", live = Dead)
    ) ! project0
  }

  lazy val plainText  = PlainText.ForProject(project, ProjectText.Context.Project)
  lazy val textSearch = TextSearch(project, plainText)
}
