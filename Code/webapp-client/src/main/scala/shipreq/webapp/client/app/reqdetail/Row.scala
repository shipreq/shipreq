package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{CustomField => CF, StaticField => SF, Field, UseCaseSteps}
import shipreq.webapp.client.data.{FilterDead, ShowDead}
import shipreq.webapp.client.lib.DataReusability._
import shipreq.webapp.client.lib.KeyGen

sealed abstract class Row(_key: String) {
  /** A value that can be passed to React to quickly identify columns. */
  val key: String = _key
}

object Row {
  private def autoKey = KeyGen.global.next()

  sealed abstract class UseCaseSteps(_key: String) extends Row(_key) {
    def field     : SF.UseCaseStepTree
    val treeFilter: UseCaseSteps.Tree => Range
    def tailStep  : Boolean
  }

  case object UseCaseStepsN extends UseCaseSteps("n") {
    override def field      = SF.NormalAltStepTree
    override val treeFilter = Function.const(0 to 0) _
    override def tailStep   = false
  }

  case object UseCaseStepsA extends UseCaseSteps("a") {
    override def field      = SF.NormalAltStepTree
    override val treeFilter = 1 until (_: UseCaseSteps.Tree).children.length
    override def tailStep   = true
  }

  case object UseCaseStepsE extends UseCaseSteps("e") {
    override def field = SF.ExceptionStepTree
    override val treeFilter = (_: UseCaseSteps.Tree).children.indices
    override def tailStep   = true
  }

  case object Life              extends Row(autoKey)
  case object DeletionReason    extends Row(autoKey)
  case object ReqType           extends Row(autoKey)
  case object Code              extends Row(autoKey)
  case object Tags              extends Row(autoKey)
  case object Implications      extends Row(autoKey)
  case class CustomField(f: CF) extends Row("f" + f.id.value)

  @inline implicit def univEqUseCaseSteps: UnivEq[UseCaseSteps] =
    UnivEq.derive

  @inline implicit def equality: UnivEq[Row] =
    UnivEq.derive

  implicit val reusability: Reusability[Row] =
    Reusability.byUnivEq

  def head(fd: FilterDead): Vector[Row] =
    if (fd :: ShowDead) headDead else headLive

  private def headLive: Vector[Row] =
    headDead.filterNot(_ ==* DeletionReason)

  private def headDead: Vector[Row] =
    Vector(
      ReqType       ,
      Life          ,
      DeletionReason,
      Code          ,
      Tags          ,
      Implications  )

  val fromField: Field => List[Row] = {
    case f: CF                => CustomField(f) :: Nil
    case SF.NormalAltStepTree => UseCaseStepsN :: UseCaseStepsA :: Nil
    case SF.ExceptionStepTree => UseCaseStepsE :: Nil
    case SF.StepGraph         => Nil // TODO ======================================
  }
}
