package shipreq.webapp.base.test

import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.text._
import Event._
import SampleProject4.Values.uc1
import SampleProject5.{project => project0}
import StaticField.{NormalAltStepTree => NA, ExceptionStepTree => E}
import UnsafeTypes._

/**
 * Builds on SampleProject #5 to:
 *   - Add some dead steps to UC-1
 *   - Change UC-1 title to include refs to live & dead steps
 */
object SampleProject6 {

  trait Values extends SampleProject5.Values {
    val step16_label = "1.0.X.1"
    val step17_label = "1.E.X.1"
    val step18_label = "1.E.1"
    val step19_label = "1.0.2.a"
    val step20_label = "1.X.0"
  }
  object Values extends Values

  private def newTitle: Text.UseCaseTitle.OptionalText = {
    import Text.UseCaseTitle._
    Vector(
      UseCaseStepRef(16),
      Literal(" and "),
      UseCaseStepRef(17),
      Literal(" are dead. "),
      UseCaseStepRef(19),
      Literal(" and "),
      UseCaseStepRef(18),
      Literal(" are not."))
  }

  lazy val project = WebappTestUtil.applyEventsSuccessfully(project0
    , UseCaseStepCreate(16, uc1, NA, "0.0".ploc) // becomes UC-n.0.2, followed by n.0.3, n.0.4.
    , UseCaseStepDelete(16)                      // becomes UC-n.0.X.1, now looks the same as before live
    , UseCaseStepCreate(17, uc1, E, ∅)           // becomes UC-n.E.1
    , UseCaseStepCreate(18, uc1, E, ∅)           // becomes UC-n.E.2
    , UseCaseStepDelete(17)                      // becomes UC-n.E.X.1
    , UseCaseStepCreate(19, uc1, NA, "0.2".ploc) // becomes UC-n.0.3
    , UseCaseStepShiftRight(19)                  // becomes UC-n.0.2.a
    , UseCaseStepCreate(20, uc1, NA, ∅)          // becomes UC-n.1
    , UseCaseStepDelete(20)                      // becomes UC-n.X.0
    , UseCaseTitleSet(uc1, newTitle)
    )

  lazy val plainText  = PlainText.ForProject.noCtx(project)
  lazy val textSearch = TextSearch(project, plainText)
}
