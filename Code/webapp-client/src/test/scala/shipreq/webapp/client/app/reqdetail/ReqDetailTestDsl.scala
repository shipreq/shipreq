package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react.test.ReactTestUtils.Simulate
import monocle.macros.Lenses
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.data.{FilterDead, HideDead, ShowDead}
import shipreq.webapp.client.test.TestState._

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
    *.focus("All steps").collection(_.obs.uc.stepTitles)

  val filterDead =
    *.focus("FilterDead").value(_.obs.generic.filterDead)

  val visibleFields =
    *.focus("Visible fields").collection(_.obs.generic.fields.keys)

  val life =
    *.focus("Life").value(_.obs.generic.live)

  val editorCount =
    *.focus("Editor count").value(_.obs.editables.length)

  val invariantsWhenBad: *.Invariant =
    *.emptyInvariant

  val invariantsGR: *.Invariant = {
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

  val invariantsUC: *.Invariant = {
    val stepsAreUnique = allSteps.assert.distinct

    invariantsGR & stepsAreUnique
  }

  val invariants: *.Invariant =
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

  def addTailStepAC: *.Action =
    *.action("Add AC tail step")(Simulate click _.obs.uc.tailStepRowAC.add)

  def addTailStepEC: *.Action =
    *.action("Add EC tail step")(Simulate click _.obs.uc.tailStepRowEC.add)

  def addStep(label: String): *.Action =
    *.action("Add " + label)(Simulate click _.obs.uc.row(label).add)

  def delStep(label: String): *.Action =
    *.action("Delete " + label)(Simulate click _.obs.uc.row(label).del)

  def shiftStepLeft(label: String): *.Action =
    *.action("ShiftLeft " + label)(Simulate click _.obs.uc.row(label).left)

  def shiftStepRight(label: String): *.Action =
    *.action("ShiftRight " + label)(Simulate click _.obs.uc.row(label).right)

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
      }))(Simulate click _.obs.generic.lifeChangeButton.get)

  // Hit delete on the delete screen
  def deleteDelete =
    *.action("Hit Delete")(Simulate click _.obs.deletionForm.get.deleteButton)
      .updateState(stateMode set Mode.Details)

  // Hit cancel on the delete screen
  def deleteCancel =
    *.action("Hit Cancel")(Simulate click _.obs.deletionForm.get.cancelButton)
      .updateState(stateMode set Mode.Details)

  val doubleClickTitle =
    *.action("Double-click title")(Simulate doubleClick _.obs.generic.titleDom)

  def doubleClickFieldValue(field: String) =
    *.action("Double-click " + field)(Simulate doubleClick _.obs.generic.fields(field).dom)

}
