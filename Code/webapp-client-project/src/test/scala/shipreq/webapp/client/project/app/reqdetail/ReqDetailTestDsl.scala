package shipreq.webapp.client.project.app.reqdetail

import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import org.scalajs.dom.html
import shipreq.base.test.BaseTestUtil.quoteStringForDisplay
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.test.TestState._

object ReqDetailTestDsl {

  sealed abstract class Mode
  object Mode {
    case object Error   extends Mode
    case object Details extends Mode
    case object Delete  extends Mode
    case object Restore extends Mode
    implicit def univEq: UnivEq[Mode] = UnivEq.derive
    implicit def equal : Equal [Mode] = Equal.by_==
  }

//  sealed abstract class ButtonState
//  object ButtonState {
//    case object Missing extends ButtonState
//    case object Enabled extends ButtonState
//    case object Disabled extends ButtonState
//    def apply(o: Option[html.Button]): ButtonState =
//      o.map(_.disabled.get) match {
//        case Some(true)  => Disabled
//        case Some(false) => Enabled
//        case None        => Missing
//      }
//  }

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
    *.focus("Error reason").value(_.obs.error.reason).test(s"contains '$e'")(_ contains e)

  val allSteps =
    *.focus("All steps").collection(_.obs.uc.stepLabels)

  val allStepRows =
    *.focus("All step rows").collection(_.obs.uc.allRows)

  val filterDead =
    *.focus("FilterDead").value(_.obs.generic.filterDead)

  val visibleFields =
    *.focus("Visible fields").collection(_.obs.generic.fields.keys)

  val life =
    *.focus("Life").value(_.obs.generic.live)

  val editorCount =
    *.focus("Editor count").value(_.obs.editables.length)

  val tailStepAC = *.focus("AC tail step").value(_.obs.uc.tailStepRowAC)
  val tailStepEC = *.focus("EC tail step").value(_.obs.uc.tailStepRowEC)

  val invariantsWhenBad: *.Invariants =
    *.emptyInvariant

  val invariantsGR: *.Invariants = {
    val pubid = *.focus("Pubid").obsAndState(_.generic.pubid, _.pubidStr).assert.equal

    val delReasonField = *.focus("DeletedReasons visible")
      .value(_.obs.generic.fields contains UiText.FieldNames.deletionReason)
      .assert.equalBy(_.obs.generic.filterDead is ShowDead)

    val filterDeadLocked =
      *.focus("FilterDead locked").value(_.obs.generic.filterDeadLocked).assert.equalBy(_.obs.generic.live is Dead)

    val whenDead =
      ( filterDead.assert(ShowDead)
      & editorCount.assert(0)
      )
      .when(_.obs.generic.live is Dead)

    pubid & delReasonField & filterDeadLocked & whenDead
  }

  val invariantsUC: *.Invariants = {
    val whenDead: *.Invariants =
      allStepRows.map(_.buttons).rename("UC step buttons")
        .assert.not.exists("exist", _.nonEmpty)

    val whenLive: *.Invariants = {

      *.emptyInvariant
    }

    val liveOrDead = *.chooseInvariant("UC dead/alive invariants")(_.obs.generic.live match {
      case Live => whenLive
      case Dead => whenDead
    })

    val stepsAreUnique = allSteps.assert.distinct

    invariantsGR & stepsAreUnique & liveOrDead
  }

  val invariants: *.Invariants =
    *.focus("Mode").obsAndState(_.mode, _.mode).assert.equal &
    *.chooseInvariant("Mode invariants")(i => i.state.mode match {
      case Mode.Error   => invariantsWhenBad
      case Mode.Delete  => *.emptyInvariant
      case Mode.Restore => *.emptyInvariant
      case Mode.Details =>
        if (i.state.pubidStr startsWith "UC-")
          invariantsUC
        else
          invariantsGR
    })

  private def clickEnabled(b: html.Button): Unit = {
    assert(!b.disabled, "Button is disabled.")
    Simulate click b
  }

  def addTailStepAC: *.Actions =
    tailStepAC.test("exists")(_.isDefined) +>
    *.action("Add AC tail step")(i => clickEnabled(i.obs.uc.tailStepRowAC.get.add.get))

  def addTailStepEC: *.Actions =
    tailStepEC.test("exists")(_.isDefined) +>
    *.action("Add EC tail step")(i => clickEnabled(i.obs.uc.tailStepRowEC.get.add.get))

  def addStep(label: String): *.Actions =
    *.action("Add " + label)(i => clickEnabled(i.obs.uc.row(label).add.get))

  def delStep(label: String): *.Actions =
    *.action("Delete " + label)(i => clickEnabled(i.obs.uc.row(label).del.get))

  def restoreStep(label: String): *.Actions =
    *.action("Restore " + label)(i => clickEnabled(i.obs.uc.row(label).rest.get))

  def shiftStepLeft(label: String): *.Actions =
    *.action("ShiftLeft " + label)(i => clickEnabled(i.obs.uc.row(label).left.get))

  def shiftStepRight(label: String): *.Actions =
    *.action("ShiftRight " + label)(i => clickEnabled(i.obs.uc.row(label).right.get))

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
      SimEvent.Change(newValue) simulate _.obs.uc.row(label).textEditor)

  def commitStepTextEdit(label: String): *.Actions =
    *.action(s"Commit $label text edit")(KB.Enter.ctrl simulateKeyDown _.obs.uc.row(label).textEditor)

  val filterDeadToggle =
    *.action("Toggle FilterDead")(Simulate click _.obs.generic.filterDeadButton)
      .addCheck(filterDead.assert.changeTo(!_))

  def setFilterDead(fd: FilterDead) =
    filterDeadToggle
      .rename(fd.toString)
      .unless(_.obs.generic.filterDead is fd)

  def hideDead = setFilterDead(HideDead)
  def showDead = setFilterDead(ShowDead)

  val changeLife =
    *.action(NameFn(_.map(_.obs.generic.live) match {
      case None       => "Change life"
      case Some(Live) => UiText.Life.delete + " req"
      case Some(Dead) => UiText.Life.restore + " req"
      }))(i => clickEnabled(i.obs.generic.lifeChangeButton.get))

  // Hit delete on the delete screen
  def deleteScreenDelete =
    *.action("Hit Delete")(i => clickEnabled(i.obs.deletionForm.get.deleteButton))
      .updateState(stateMode set Mode.Details)

  // Hit cancel on the delete screen
  def deleteScreenCancel =
    *.action("Hit Cancel")(i => clickEnabled(i.obs.deletionForm.get.cancelButton))
      .updateState(stateMode set Mode.Details)


  // Hit restore on the restore screen
  def restoreScreenRestore =
    *.action("Hit Restore")(i => clickEnabled(i.obs.restorationForm.get.restoreButton))
      .updateState(stateMode set Mode.Details)

  // Hit cancel on the restore screen
  def restoreScreenCancel =
    *.action("Hit Cancel")(i => clickEnabled(i.obs.restorationForm.get.cancelButton))
      .updateState(stateMode set Mode.Details)

  val doubleClickTitle =
    *.action("Double-click title")(Simulate doubleClick _.obs.generic.titleDom)

  def doubleClickFieldValue(field: String) =
    *.action("Double-click " + field)(Simulate doubleClick _.obs.generic.fields(field).dom)

}
