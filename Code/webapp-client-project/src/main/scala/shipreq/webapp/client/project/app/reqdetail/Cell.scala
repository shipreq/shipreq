package shipreq.webapp.client.project.app.reqdetail

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data
import shipreq.webapp.base.data.{CustomFieldId, UseCaseStepId}
import shipreq.webapp.client.project.feature.ContentEditorFeature.EditFieldKey
import shipreq.webapp.client.project.lib.DataReusability._

sealed abstract class Cell {
  import Cell._

  /**
   * Direction of implications relative to row-subject.
   *
   * If forwards, the user edits what this subject implies (ie. subject → edit-specified).
   * If backwards, then it's what implies this subject     (ie. subject ← edit-specified).
   *
   * Note: Copy of reqtable.Column.implicationDirection
   */
  final def implicationDirection: Direction =
    this match {
      case CustomField(_) => data.CustomField.Implication.dir
      case ImplicationTgt => Forwards
      case _              => Backwards
    }
}

object Cell {
  case object ReqType                                  extends Cell
  case object Code                                     extends Cell
  case object Title                                    extends Cell
  case object Tags                                     extends Cell
  case object ImplicationSrc                           extends Cell
  case object ImplicationTgt                           extends Cell
  case class CustomField       (id: CustomFieldId)     extends Cell
  case class UseCaseStep       (id: UseCaseStepId)     extends Cell
  case class UseCaseStepCtrls  (id: UseCaseStepId)     extends Cell
  case class AddUseCaseStep    (id: UseCaseStepId)     extends Cell
  case class AddUseCaseTailStep(row: Row.UseCaseSteps) extends Cell

  @inline implicit def univEq: UnivEq[Cell] =
    UnivEq.derive

  implicit val reusability: Reusability[Cell] =
    Reusability.byUnivEq

  val EditFieldKeyIntersection = Intersection[Cell, EditFieldKey] {
    case Cell.ReqType               => Some(EditFieldKey.ReqType        )
    case Cell.Code                  => Some(EditFieldKey.Code           )
    case Cell.Title                 => Some(EditFieldKey.Title          )
    case Cell.Tags                  => Some(EditFieldKey.Tags           )
    case Cell.ImplicationSrc        => Some(EditFieldKey.ImplicationSrc )
    case Cell.ImplicationTgt        => Some(EditFieldKey.ImplicationTgt )
    case Cell.CustomField(id)       => Some(EditFieldKey.CustomField(id))
    case Cell.UseCaseStep(id)       => Some(EditFieldKey.UseCaseStep(id))
    case Cell.UseCaseStepCtrls(_)
       | Cell.AddUseCaseStep(_)
       | Cell.AddUseCaseTailStep(_) => None
  } {
    case EditFieldKey.ReqType         => Some(Cell.ReqType        )
    case EditFieldKey.Code            => Some(Cell.Code           )
    case EditFieldKey.Title           => Some(Cell.Title          )
    case EditFieldKey.Tags            => Some(Cell.Tags           )
    case EditFieldKey.ImplicationSrc  => Some(Cell.ImplicationSrc )
    case EditFieldKey.ImplicationTgt  => Some(Cell.ImplicationTgt )
    case EditFieldKey.CustomField(id) => Some(Cell.CustomField(id))
    case EditFieldKey.UseCaseStep(id) => Some(Cell.UseCaseStep(id))
  }
}