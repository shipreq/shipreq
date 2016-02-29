package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util._
import shipreq.webapp.base.data
import shipreq.webapp.base.data.{Field, StaticField}
import shipreq.webapp.client.lib.KeyGen

sealed abstract class Row {
  /** A value that can be passed to React to quickly identify columns. */
  val key: String
}

object Row {
  sealed abstract class AutoKey extends Row {
    override val key = KeyGen.global.next()
  }

  case object ReqType                         extends Row.AutoKey
  case object Code                            extends Row.AutoKey
  case object Tags                            extends Row.AutoKey
  case object Implications                    extends Row.AutoKey
  case class CustomField(f: data.CustomField) extends Row {
    override val key: String = "f" + f.id.value
  }
  case class UseCaseSteps(f: StaticField.UseCaseStepTree) extends Row {
    override val key: String = f match {
      case StaticField.NormalAltStepTree => "n"
      case StaticField.ExceptionStepTree => "e"
    }
  }

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

  val fromField: Field => Row = {
    case f: data.CustomField            => CustomField(f)
    case f: StaticField.UseCaseStepTree => UseCaseSteps(f)
    case StaticField.StepGraph          => ReqType // TODO ======================================
  }
}
