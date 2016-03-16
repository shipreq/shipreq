package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react.test.ReactTestUtils.Simulate
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{UnivEq, univEqOps}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.data.{ShowDead, FilterDead}
import teststate.Exports._

object ReqDetailTestDsl {
  implicit val showFilterDead = Show.byToString[FilterDead]
  implicit val equalFilterDead = Equal.by_==[FilterDead]

  sealed abstract class Mode
  object Mode {
    case object Error extends Mode
    case object GR    extends Mode
    case object UC    extends Mode
    implicit def univEq: UnivEq[Mode] = UnivEq.derive
    implicit def equal : Equal [Mode] = Equal.by_==
    implicit def show  : Show  [Mode] = Show.byToString
  }

  type State = Option[ExternalPubid]
  case class TestState(project: Project, ep: State) {

    val mode: Mode = ep match {
      case None                                 => Mode.Error
      case Some(p) if p.mnemonic.value ==* "UC" => Mode.UC
      case Some(_)                              => Mode.GR
    }

    val pubidStr = ep.map(PlainText.pubid)

    //lazy val req = ep.map(project.findReq(_).toOption.get)
  }

  val * = Dsl.sync[Unit, ReqDetailObs, TestState, String]

  def checkErrorReason(e: String) =
    *.focus("Error reason").value(_.obs.error.reason).assert.equal(e)

  val allSteps = *.focus("All steps").collection(_.obs.uc.stepTitles)

  val invariantsWhenBad: *.Invariant =
    *.emptyInvariant

  val invariantsGR: *.Invariant = {
    val pubid = *.focus("Pubid").obsAndState(_.generic.pubid.some, _.pubidStr).assert.equal

    val delReasonField = *.focus("DeletedReasons visible")
      .value(_.obs.generic.fields contains UiText.FieldNames.deletionReason)
      .assert.equalBy(_.obs.generic.filterDead :: ShowDead)

    val filterDeadLocked =
      *.focus("FilterDead locked").value(_.obs.generic.filterDeadLocked).assert.equalBy(_.obs.generic.live :: Dead)

    val showingDead =
      *.focus("FilterDead").value(_.obs.generic.filterDead).assert.equal(ShowDead).when(_.obs.generic.live :: Dead)

    pubid & delReasonField & filterDeadLocked & showingDead
  }

  val invariantsUC: *.Invariant = {
    val stepsAreUnique = allSteps.assert.distinct

    invariantsGR & stepsAreUnique
  }

  val invariants: *.Invariant =
    *.focus("Mode").obsAndState(_.mode, _.mode).assert.equal &
    *.chooseInvariant("Mode invariants", _.obs.mode match {
      case Mode.GR    => invariantsGR
      case Mode.UC    => invariantsUC
      case Mode.Error => invariantsWhenBad
    })

  def addTailStepAC: *.Action =
    *.action("Add AC tail step").act(Simulate click _.obs.uc.tailStepRowAC.add)

  def addTailStepEC: *.Action =
    *.action("Add EC tail step").act(Simulate click _.obs.uc.tailStepRowEC.add)

  def addStep(label: String): *.Action =
    *.action("Add " + label).act(Simulate click _.obs.uc.row(label).add)

  def delStep(label: String): *.Action =
    *.action("Delete " + label).act(Simulate click _.obs.uc.row(label).del)

  def shiftStepLeft(label: String): *.Action =
    *.action("ShiftLeft " + label).act(Simulate click _.obs.uc.row(label).left)

  def shiftStepRight(label: String): *.Action =
    *.action("ShiftRight " + label).act(Simulate click _.obs.uc.row(label).right)

  val filterDead =
    *.focus("FilterDead").value(_.obs.generic.filterDead)

  val filterDeadToggle =
    *.action("Toggle FilterDead").act(Simulate change _.obs.generic.filterDeadInput)
      .addCheck(filterDead.assert.changeOccurs)
}
