package shipreq.webapp.base.test

import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.text._
import UnsafeTypes._
import SampleProject4.Values.uc1
import SampleProject5.{project => project0}
import StaticField.{NormalAltStepTree => NA, ExceptionStepTree => E}

/**
 * Builds on SampleProject #5 to add:
 *   - UC-2: live UC with some dead steps
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
    , AddUseCaseStep(16, uc1, NA, "0.0".ploc) // becomes UC-n.0.2, followed by n.0.3, n.0.4.
    , DeleteUseCaseStep(16)                   // becomes UC-n.0.X.1, now looks the same as before live
    , AddUseCaseStep(17, uc1, E, ∅)           // becomes UC-n.E.1
    , AddUseCaseStep(18, uc1, E, ∅)           // becomes UC-n.E.2
    , DeleteUseCaseStep(17)                   // becomes UC-n.E.X.1
    , AddUseCaseStep(19, uc1, NA, "0.2".ploc) // becomes UC-n.0.3
    , ShiftUseCaseStepRight(19)               // becomes UC-n.0.2.a
    , AddUseCaseStep(20, uc1, NA, ∅)          // becomes UC-n.1
    , DeleteUseCaseStep(20)                   // becomes UC-n.X.0
    , SetUseCaseTitle(uc1, newTitle)
    )

  lazy val plainText  = PlainText(project, ProjectText.Context.None)
  lazy val textSearch = TextSearch(project, plainText)
}
