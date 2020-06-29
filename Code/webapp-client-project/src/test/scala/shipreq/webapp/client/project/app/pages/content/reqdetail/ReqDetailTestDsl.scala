package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.microlibs.testutil.TestUtilInternals.quoteStringForDisplay
import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import nyaya.gen._
import org.scalajs.dom.html
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.CommonObs
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.project.test.TestGlobal

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
  final case class State(ep: ExternalPubid, mode: Mode)

  @Lenses
  final case class TestState(project: Project, state: State) {
    def ep = state.ep
    def mode = state.mode

    val pubidStr = PlainText pubid ep

    //lazy val req = ep.map(project.findReq(_).toOption.get)
  }

  val stateMode = TestState.state ^|-> State.mode

  val * = Dsl[TestGlobal, ReqDetailObs, TestState]

  val global = new TestGlobal.TestDslWithObs(*)(identity, _.global)

  val field = Memo((f: String) => new CommonObs.Editor.TestDsl(*, f)(_.generic.field(f)))

  def checkErrorReason(e: String) =
    *.focus("Error reason").value(_.obs.error.reason).test(s"contains '$e'")(_ contains e)

  val unsavedChanges =
    *.focus("unsaved changes").value(_.obs.nav.unsavedChanges)

  val allSteps =
    *.focus("All steps").collection(_.obs.uc.stepLabels)

  val allStepRows =
    *.focus("All step rows").collection(_.obs.uc.allRows)

  val filterDead =
    *.focus("FilterDead").value(_.obs.generic.filterDead)

  val visibleFields =
    *.focus("Visible fields").collection(_.obs.generic.fieldsInOrder)

  val life =
    *.focus("Life").value(_.obs.generic.live)

  val editorCount =
    *.focus("Editor count").value(_.obs.editables.length)

  val spinnerCount =
    *.focus("Spinner count").value(_.obs.spinnerCount)

  val titleChangeInProgress =
    *.focus("Title change is in progress").value(_.obs.generic.titleSpinning)

  val titleText =
    *.focus("Title text").value(_.obs.generic.titleText)

  val titleEditor =
    *.focus("Title editor").option(_.obs.generic.titleEditor.map(_.value))

  val tailStepAC = *.focus("AC tail step").value(_.obs.uc.tailStepRowAC)
  val tailStepEC = *.focus("EC tail step").value(_.obs.uc.tailStepRowEC)

  val invariantsWhenBad: *.Invariants =
    *.emptyInvariant

  val invariantsGR: *.Invariants = {
    val pubid = *.focus("Pubid").obsAndState(_.generic.pubid, _.pubidStr).assert.equal

    val delReasonField = *.focus("DeletedReasons visible")
      .value(_.obs.generic.fields contains SpecialBuiltInField.DeletionReason.name)
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

  // This isn't part of invariants above cos it's not a universal invariant and I don't want it used by ProjectSpaTest.
  // This invariant holds in the case of ReqDetailTest tests only because all possible editors as on the same page.
  val unsavedChangesInvariant: *.Invariants =
    *.point("unsavedChanges ≤ editors") { x =>
      val u = unsavedChanges.run(x)
      val e = editorCount.run(x)
      val s = spinnerCount.run(x)
      Option.unless(u <= (e + s))(s"unsavedChanges ($u) must be ≤ editorCount ($e) + spinnerCount ($s)")
    }

  private def clickEnabled(b: html.Button): Unit = {
    assert(!b.disabled, "Button is disabled.")
    Simulate click b
  }

  val addTailStepAC: *.Actions =
    tailStepAC.test("exists")(_.isDefined) +>
    *.action("Add AC tail step")(i => clickEnabled(i.obs.uc.tailStepRowAC.get.add.get))

  val addTailStepEC: *.Actions =
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
    *.action(s"Double-click $label text")(Simulate doubleClick _.obs.uc.row(label).textContainer.get.dom)

  def editStepText(label: String, newValue: String): *.Actions =
    _editStepText(label, None, newValue)

  def editStepText(label: String, expectedOldValue: String, newValue: String): *.Actions =
    _editStepText(label, Some(expectedOldValue), newValue)

  def openEditor(label: String): *.Actions =
    doubleClickStepText(label) +> editorCount.assert.increment

  private def _editStepText(label: String, old: Option[String], newValue: String): *.Actions =
    ( openEditor(label)
      +> stepText(label).rename("Initial editor text").assert.equalWhenDefined(old)
      >> setStepTextEditValue(label, newValue)
      >> commitStepTextEdit(label)
    ).group(s"Edit $label text to ${quoteStringForDisplay(newValue)}")

  def setStepTextEditValue(label: String, newValue: String): *.Actions =
    *.action(s"Set $label text to ${quoteStringForDisplay(newValue)}")(
      SimEvent.Change(newValue) simulate _.obs.uc.row(label).textEditor.get)

  def commitStepTextEdit(label: String): *.Actions =
    *.focus("Editor error msg").option(_.obs.uc.row(label).errorMsg).assert(None) +>
      *.action(s"Commit $label text edit")(KB.Enter.ctrl simulateKeyDown _.obs.uc.row(label).textEditor.get) +>
      editorCount.assert.decrement

  def abortStepTextEdit(label: String): *.Actions =
    *.action(s"Abort $label text edit")(KB.Escape simulateKeyDown _.obs.uc.row(label).textEditor.get) +>
      editorCount.assert.decrement

  lazy val commitTitleEditor: *.Actions =
    *.action("Commit title editor")(KB.Enter.ctrl simulateKeyDown _.obs.generic.titleEditor.get)
    // +> editorCount.assert.decrement

  val filterDeadToggle =
    *.action(NameFn {
      case None    => "Toggle FilterDead"
      case Some(x) => s"Set FilterDead to ${!x.obs.generic.filterDead}"
    })(Simulate click _.obs.generic.filterDeadButton)
      .addCheck(filterDead.assert.changeTo(!_))

  def setFilterDead(fd: FilterDead) =
    filterDeadToggle
      .rename(fd.toString)
      .unless(_.obs.generic.filterDead is fd)

  def hideDead = setFilterDead(HideDead)
  def showDead = setFilterDead(ShowDead)

  val clickDeleteOrRestore =
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

  val deleteReq = (
    clickDeleteOrRestore.updateState(stateMode set Mode.Delete) <+ life.assert(Live) >>
      deleteScreenDelete +> life.assert(Dead)
    ).group("Delete req")

  val restoreReq = (
    clickDeleteOrRestore.updateState(stateMode set Mode.Restore) <+ life.assert(Dead) >>
      restoreScreenRestore +> life.assert(Live)
    ).group("Restore req")

  val doubleClickTitle =
    *.action("Double-click title")(Simulate doubleClick _.obs.generic.titleDom)

  def setTitleEditValue(newValue: String): *.Actions =
    *.action(s"Set title text to ${quoteStringForDisplay(newValue)}")(
      SimEvent.Change(newValue) simulate _.obs.generic.titleEditor.get)

  val randomUseCaseStepAction: *.Actions = {

    val addTailSteps = Vector.empty :+ addTailStepAC :+ addTailStepEC

    val genStr = Gen.ascii.string(0 to 4)

    *.chooseAction("Random use case step action") { ros =>
      import ros.obs
      import obs.uc.StepRow

      var stepCount = 0
      val stepArray = new Array[Gen[*.Actions]](8)

      def add(a: Gen[*.Actions]): Unit = {
        stepArray(stepCount) = a
        stepCount += 1
      }

      def addChoice(as: Vector[*.Actions]): Unit =
        if (as.nonEmpty)
          add(Gen.choose_!(as))

      def selectActions(ok: StepRow => Boolean, a: String => *.Actions): Vector[*.Actions] =
        obs.uc.allRows.iterator.filter(ok).flatMap(_.label).map(a).toVector

      // ===============================================================================================================

      add(Gen.choose_!(selectActions(_.canAdd, addStep) ++ addTailSteps))
      addChoice(selectActions(_.canDel, delStep))
      addChoice(selectActions(_.canLeft, shiftStepLeft))
      addChoice(selectActions(_.canRight, shiftStepRight))

      val editorOpen = obs.uc.allRows.iterator.filter(_.isEditorOpen).flatMap(_.label)
      val editorClosed = obs.uc.allRows.iterator.filter(_.isEditorClosed).flatMap(_.label)

      if (editorOpen.nonEmpty) {
        val genLabel = Gen.choose_!(editorOpen)
        add(genLabel.flatMap(l => genStr.map(setStepTextEditValue(l, _))))
        add(genLabel.map(abortStepTextEdit))
        addChoice(selectActions(r => r.isEditorOpen && r.errorMsg.isEmpty, commitStepTextEdit))
      }

      if (editorClosed.nonEmpty) {
        val genLabel = Gen.choose_!(editorClosed)
        add(genLabel.map(openEditor))
      }

      Gen.chooseInt(stepCount).flatMap(stepArray.apply).sample()
    }
  }

  def debugPrintSteps: *.Actions =
    *.action("debugPrintSteps"){ ros =>
      println(ros.obs.uc.allRows.map {row =>
        row.label.fold("")(_ + (if (row.isEditorOpen) "*" else " "))
      }.filter(_.nonEmpty).mkString(" "))
    }
}
