package shipreq.webapp.base.test

import shipreq.webapp.base.data.StaticField.{ExceptionStepTree => E, NormalAltStepTree => NA}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.SampleProject5.{project => project0}
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text._

/**
 * Builds on SampleProject #5 to:
 *   - Add some dead steps to UC-1
 *   - Change UC-1 title to include refs to live & dead steps
 *   - Add UC-2 with just the title step
 *
 * UC steps:
 *
 *   - (#10) 1.0
 *   - (#11) 1.0.1
 *   - (#12) 1.0.2
 *   - (#19) 1.0.2.a
 *   - (#13) 1.0.3
 *   - (#14) 1.1
 *   - (#15) 1.1.1
 *   - (#18) 1.E.1
 *
 *   - (#16) 1.0.X.1
 *   - (#17) 1.E.X.1
 *   - (#20) 1.X.0
 */
object SampleProject6 {

  trait Values extends SampleProject5.Values {
    val step16_label = "1.0.X.1"
    val step17_label = "1.E.X.1"
    val step18_label = "1.E.1"
    val step19_label = "1.0.2.a"
    val step20_label = "1.X.0"
    val uc2 = UseCaseId(1204)
  }
  object Values extends Values
  import Values._

  private def newTitleForUC1: Text.UseCaseTitle.OptionalText = {
    import Text.UseCaseTitle._
    apply(
      UseCaseStepRef(16),
      Literal(" and "),
      UseCaseStepRef(17),
      Literal(" are dead. "),
      UseCaseStepRef(19),
      Literal(" and "),
      UseCaseStepRef(18),
      Literal(" are not."))
  }

  private def titleForUC2: Text.UseCaseTitle.OptionalText = {
    import Text.UseCaseTitle._
    apply(Literal("Empty for now"))
  }

  private def setStepText(id: UseCaseStepId, txt: String) =
    UseCaseStepUpdate(id, UseCaseStepGD.Title(txt))

  lazy val project = WebappTestUtil.applyEventsSuccessfully(project0
    , UseCaseStepCreate(16, uc1, NA, "0.0".ploc) // becomes UC-n.0.2, followed by n.0.3, n.0.4.
    , setStepText(16, "x")
    , UseCaseStepDelete(16)                      // becomes UC-n.0.X.1, now looks the same as before live
    , UseCaseStepCreate(17, uc1, E, ∅)           // becomes UC-n.E.1
    , UseCaseStepCreate(18, uc1, E, ∅)           // becomes UC-n.E.2
    , setStepText(17, "x")
    , UseCaseStepDelete(17)                      // becomes UC-n.E.X.1
    , UseCaseStepCreate(19, uc1, NA, "0.2".ploc) // becomes UC-n.0.3
    , UseCaseStepShiftRight(19)                  // becomes UC-n.0.2.a
    , UseCaseStepCreate(20, uc1, NA, ∅)          // becomes UC-n.1
    , setStepText(20, "x")
    , UseCaseStepDelete(20)                      // becomes UC-n.X.0
    , UseCaseTitleSet(uc1, newTitleForUC1)
    , UseCaseCreate(uc2, 200, ∅)
    , UseCaseTitleSet(uc2, titleForUC2)
    )

  lazy val plainText  = PlainText.ForProject.noCtx(project)
  lazy val textSearch = TextSearch(project, plainText)
}
