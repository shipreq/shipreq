package shipreq.webapp.client.project.app.reqdetail

import japgolly.scalajs.react.test._
import japgolly.scalajs.react.test.ReactTestUtils.Simulate
import monocle.macros.Lenses
import org.scalajs.dom.html
import shipreq.base.test.BaseTestUtil.quoteStringForDisplay
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.base.test.TestState._

object ReqDetailTestDsl {

  sealed abstract class Mode
  object Mode {
    case object Error   extends Mode
    case object Details extends Mode
    case object Delete extends Mode
    implicit def univEq: UnivEq[Mode] = UnivEq.derive
    implicit def equal : Equal [Mode] = Equal.by_==
  }

  def unspecifiedState: State =
    State(ExternalPubid(ReqType.Mnemonic("UNSPECIFIED TEST STATE"), ReqTypePos(1)), Mode.Error)

  @Lenses
  case class State(ep: ExternalPubid, mode: Mode)

  @Lenses
  case class TestState(project: Project, state: State) {
    def ep = state.ep
    def mode = state.mode

    val pubidStr = PlainText pubid ep

    //lazy val req = ep.map(project.findReq(_).toOption.get)
  }

  val stateMode = TestState.state ^|-> State.mode

  val * = Dsl[Unit, ReqDetailObs, TestState]

  def checkErrorReason(e: String) =
    *.focus("Error reason").value(_.obs.error.reason).assert(e)

  val allSteps =
    *.focus("All steps").collection(_.obs.uc.stepLabels)

  val filterDead =
    *.focus("FilterDead").value(_.obs.generic.filterDead)

  val visibleFields =
    *.focus("Visible fields").collection(_.obs.generic.fields.keys)

  val life =
    *.focus("Life").value(_.obs.generic.live)

  val editorCount =
    *.focus("Editor count").value(_.obs.editables.length)

  val invariantsWhenBad: *.Invariants =
    *.emptyInvariant

  val invariantsGR: *.Invariants = {
    val pubid = *.focus("Pubid").obsAndState(_.generic.pubid, _.pubidStr).assert.equal

    val delReasonField = *.focus("DeletedReasons visible")
      .value(_.obs.generic.fields contains UiText.FieldNames.deletionReason)
      .assert.equalBy(_.obs.generic.filterDead :: ShowDead)

    val filterDeadLocked =
      *.focus("FilterDead locked").value(_.obs.generic.filterDeadLocked).assert.equalBy(_.obs.generic.live :: Dead)

    val whenDead =
      ( filterDead.assert(ShowDead)
      & editorCount.assert(0)
      )
      .when(_.obs.generic.live :: Dead)

    pubid & delReasonField & filterDeadLocked & whenDead
  }

  val invariantsUC: *.Invariants = {
    val stepsAreUnique = allSteps.assert.distinct

    invariantsGR & stepsAreUnique
  }

  val invariants: *.Invariants =
    *.focus("Mode").obsAndState(_.mode, _.mode).assert.equal &
    *.chooseInvariant("Mode invariants")(i => i.state.mode match {
      case Mode.Error   => invariantsWhenBad
      case Mode.Delete  => *.emptyInvariant
      case Mode.Details =>
        if (i.state.pubidStr startsWith "UC-")
          invariantsUC
        else
          invariantsGR
    })

  private def clickEnabled(b: html.Button): Unit = {
    val disabled = b.disabled.getOrElse(false)
    assert(!disabled, "Button is disabled.")
    Simulate click b
  }

  def addTailStepAC: *.Actions =
    *.action("Add AC tail step")(i => clickEnabled(i.obs.uc.tailStepRowAC.add))

  def addTailStepEC: *.Actions =
    *.action("Add EC tail step")(i => clickEnabled(i.obs.uc.tailStepRowEC.add))

  def addStep(label: String): *.Actions =
    *.action("Add " + label)(i => clickEnabled(i.obs.uc.row(label).add))

  def delStep(label: String): *.Actions =
    *.action("Delete " + label)(i => clickEnabled(i.obs.uc.row(label).del))

  def restoreStep(label: String): *.Actions =
    *.action("Restore " + label)(i => clickEnabled(i.obs.uc.row(label).rest))

  def shiftStepLeft(label: String): *.Actions =
    *.action("ShiftLeft " + label)(i => clickEnabled(i.obs.uc.row(label).left))

  def shiftStepRight(label: String): *.Actions =
    *.action("ShiftRight " + label)(i => clickEnabled(i.obs.uc.row(label).right))

  def stepText(label: String) =
    *.focus(label + " text").value(_.obs.uc.row(label).text)

  def doubleClickStepText(label: String): *.Actions =
    *.action(s"Double-click $label text")(Simulate doubleClick _.obs.uc.row(label).textContainer.dom)

  def editStepText(label: String, newValue: String): *.Actions =
    (doubleClickStepText(label)
      +> editorCount.assert.increment
      >> setStepTextEditValue(label, newValue)
      >> commitStepTextEdit(label)
      +> editorCount.assert.decrement
    ).group(s"Edit $label text to ${quoteStringForDisplay(newValue)}")

  def setStepTextEditValue(label: String, newValue: String): *.Actions =
    *.action(s"Set $label text to ${quoteStringForDisplay(newValue)}")(
      ChangeEventData(newValue) simulate _.obs.uc.row(label).textEditor)

  def commitStepTextEdit(label: String): *.Actions =
    *.action("Commit $label text edit")(Enter.ctrl simulateKeyDown _.obs.uc.row(label).textEditor)

  val filterDeadToggle =
    *.action("Toggle FilterDead")(Simulate change _.obs.generic.filterDeadInput)
      .addCheck(filterDead.assert.changeTo(!_))

  def setFilterDead(fd: FilterDead) =
    filterDeadToggle
      .rename(fd.toString)
      .unless(_.obs.generic.filterDead :: fd)

  def hideDead = setFilterDead(HideDead)
  def showDead = setFilterDead(ShowDead)

  val changeLife =
    *.action(NameFn(_.map(_.obs.generic.live) match {
      case None       => "Change life"
      case Some(Live) => UiText.Life.delete + " req"
      case Some(Dead) => UiText.Life.restore + " req"
      }))(i => clickEnabled(i.obs.generic.lifeChangeButton.get))

  // Hit delete on the delete screen
  def deleteDelete =
    *.action("Hit Delete")(i => clickEnabled(i.obs.deletionForm.get.deleteButton))
      .updateState(stateMode set Mode.Details)

  // Hit cancel on the delete screen
  def deleteCancel =
    *.action("Hit Cancel")(i => clickEnabled(i.obs.deletionForm.get.cancelButton))
      .updateState(stateMode set Mode.Details)

  val doubleClickTitle =
    *.action("Double-click title")(Simulate doubleClick _.obs.generic.titleDom)

  def doubleClickFieldValue(field: String) =
    *.action("Double-click " + field)(Simulate doubleClick _.obs.generic.fields(field).dom)

}
