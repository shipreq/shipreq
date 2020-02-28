package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react.Reusability
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.UseCaseStepId
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.lib.DataReusability._

sealed abstract class Cell

object Cell {
  final case class ReqField          (f: EditorFeature.FieldKey.ForSomeReq) extends Cell
  final case class UseCaseStep       (id: UseCaseStepId)                    extends Cell
  final case class UseCaseStepCtrls  (id: UseCaseStepId)                    extends Cell
  final case class AddUseCaseStep    (id: UseCaseStepId)                    extends Cell
  final case class AddUseCaseTailStep(row: Row.UseCaseSteps)                extends Cell

  @inline implicit def univEq: UnivEq[Cell] =
    UnivEq.derive

  implicit val reusability: Reusability[Cell] =
    Reusability.byUnivEq
}