package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react.test.ReactTestUtils.Simulate
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{UnivEq, univEqOps}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import teststate.Exports._

object ReqDetailTestDsl {

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

  val invariantsWhenBad: *.Check =
    *.emptyInvariant

  val invariantsGR: *.Check =
    *.focus("Pubid").obsAndState(_.generic.pubid.some, _.pubidStr).assert.equal

  val invariantsUC: *.Check = {
    def stepsAreUnique = allSteps.assert.distinct

    invariantsGR & stepsAreUnique
  }

  val invariants: *.Check =
    *.focus("Mode").obsAndState(_.mode, _.mode).assert.equal &
    *.choose("Mode invariants", _.obs.mode match {
      case Mode.GR    => invariantsGR
      case Mode.UC    => invariantsUC
      case Mode.Error => invariantsWhenBad
    })

  def addTailStepAC: *.Action1 =
    *.action("Add AC tail step").act(Simulate click _.obs.uc.tailStepRowAC.add)

  def addTailStepEC: *.Action1 =
    *.action("Add EC tail step").act(Simulate click _.obs.uc.tailStepRowEC.add)

  def addStep(label: String): *.Action1 =
    *.action("Add " + label).act(Simulate click _.obs.uc.row(label).add)

  def delStep(label: String): *.Action1 =
    *.action("Delete " + label).act(Simulate click _.obs.uc.row(label).del)

  def shiftStepLeft(label: String): *.Action1 =
    *.action("ShiftLeft " + label).act(Simulate click _.obs.uc.row(label).left)

  def shiftStepRight(label: String): *.Action1 =
    *.action("ShiftRight " + label).act(Simulate click _.obs.uc.row(label).right)
}
