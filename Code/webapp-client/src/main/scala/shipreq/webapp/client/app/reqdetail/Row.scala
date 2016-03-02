package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util._
import shipreq.webapp.base.data.{Field, CustomField => CF, StaticField => SF}
import shipreq.webapp.client.lib.KeyGen

sealed abstract class Row(_key: String) {
  /** A value that can be passed to React to quickly identify columns. */
  val key: String = _key
}

object Row {
  private def autoKey = KeyGen.global.next()

  case object ReqType           extends Row(autoKey)
  case object Code              extends Row(autoKey)
  case object Tags              extends Row(autoKey)
  case object Implications      extends Row(autoKey)
  case object UseCaseStepsN     extends Row("n")
  case object UseCaseStepsA     extends Row("a")
  case object UseCaseStepsE     extends Row("e")
  case class CustomField(f: CF) extends Row("f" + f.id.value)

  @inline implicit def equality: UnivEq[Row] =
    UnivEq.deriveAuto

  implicit val reusability: Reusability[Row] =
    Reusability.byEqual

  val head: Vector[Row] =
    Vector(
      ReqType     ,
      Code        ,
      Tags        ,
      Implications)

  val fromField: Field => List[Row] = {
    case f: CF                => CustomField(f) :: Nil
    case SF.NormalAltStepTree => UseCaseStepsN :: UseCaseStepsA :: Nil
    case SF.ExceptionStepTree => UseCaseStepsE :: Nil
    case SF.StepGraph         => ReqType :: Nil // TODO ======================================
  }
}
