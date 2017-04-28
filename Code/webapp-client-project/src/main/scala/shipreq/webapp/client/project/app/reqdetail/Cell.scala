package shipreq.webapp.client.project.app.reqdetail

import japgolly.scalajs.react.extra.Reusability
import scalaz.\/
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{CustomField, UseCaseStepId}
import shipreq.webapp.client.project.lib.DataReusability._

sealed abstract class Cell

object Cell {
  case object ReqType                                                            extends Cell
  case object Code                                                               extends Cell
  case object Title                                                              extends Cell
  case class  CustomTextField   (field: CustomField.Text.Id)                     extends Cell
  case class  Tags              (field: Option[CustomField.Tag.Id])              extends Cell
  case class  Implications      (scope: CustomField.Implication.Id \/ Direction) extends Cell
  case class  UseCaseStep       (id: UseCaseStepId)                              extends Cell
  case class  UseCaseStepCtrls  (id: UseCaseStepId)                              extends Cell
  case class  AddUseCaseStep    (id: UseCaseStepId)                              extends Cell
  case class  AddUseCaseTailStep(row: Row.UseCaseSteps)                          extends Cell

  @inline implicit def univEq: UnivEq[Cell] =
    UnivEq.derive

  implicit val reusability: Reusability[Cell] =
    Reusability.byUnivEq
}