package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import shipreq.webapp.client.lib.KeyGen
import shipreq.base.util._
import shipreq.webapp.base.data
import shipreq.webapp.base.data.{CustomFieldId, Field, StaticField}
import shipreq.webapp.client.feature.ContentEditorFeature.EditFieldKey

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
    case StaticField.StepGraph          => ReqType // TODO
  }
}

// =====================================================================================================================

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
  case object ReqType                       extends Cell
  case object Code                          extends Cell
  case object Title                         extends Cell
  case object Tags                          extends Cell
  case object ImplicationSrc                extends Cell
  case object ImplicationTgt                extends Cell
  case class CustomField(id: CustomFieldId) extends Cell

  @inline implicit def equality: UnivEq[Cell] =
    UnivEq.deriveAuto

  implicit val reusability: Reusability[Cell] =
    Reusability.byEqual

  val EditFieldKeyIntersection = Intersection[Cell, EditFieldKey] {
    case Cell.ReqType         => Some(EditFieldKey.ReqType        )
    case Cell.Code            => Some(EditFieldKey.Code           )
    case Cell.Title           => Some(EditFieldKey.Title          )
    case Cell.Tags            => Some(EditFieldKey.Tags           )
    case Cell.ImplicationSrc  => Some(EditFieldKey.ImplicationSrc )
    case Cell.ImplicationTgt  => Some(EditFieldKey.ImplicationTgt )
    case Cell.CustomField(id) => Some(EditFieldKey.CustomField(id))
  } {
    case EditFieldKey.ReqType         => Some(Cell.ReqType        )
    case EditFieldKey.Code            => Some(Cell.Code           )
    case EditFieldKey.Title           => Some(Cell.Title          )
    case EditFieldKey.Tags            => Some(Cell.Tags           )
    case EditFieldKey.ImplicationSrc  => Some(Cell.ImplicationSrc )
    case EditFieldKey.ImplicationTgt  => Some(Cell.ImplicationTgt )
    case EditFieldKey.CustomField(id) => Some(Cell.CustomField(id))
  }
}