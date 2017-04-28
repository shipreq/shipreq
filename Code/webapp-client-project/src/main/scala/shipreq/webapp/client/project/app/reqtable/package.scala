package shipreq.webapp.client.project.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util.univeq._
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._

/**
 * Requirements Table.
 * "Common Req View & Editor" in the prototype.
 *
 * An Excel-like table for reading and editing requirements.
 */
package object reqtable {

  type Rows = Vector[Row]
  implicit val reusabilityRows: Reusability[Rows] = Reusability.byRef // Each row will be checked anyway

  type RowSelection        = Selection[Row.SourceId]
  type RowSelectionVisible = Selection.LegalWithUpdateFn[Row.SourceId]

  implicit val reusabilityCs: Reusability[NonEmptyVector[Column]] =
    Reusability.byRef || reusabilityNonEmptyVector

  @inline def shouldComponentUpdate[P: Reusability, C <: Children, S: Reusability, B]: ScalaComponent.Config[P, C, S, B] =
    shipreq.webapp.client.project.app.shouldComponentUpdate[P, C, S, B]
    // Reusability.shouldComponentUpdateWithOverlay[P, C, S, B]

  // -----------------------------------------------------------------------------------------------

  sealed trait FocusId
  object FocusId {
    case class InEditor(id: EditorFeature.PreviewId) extends FocusId
    case class InCI(typ: CreationInterface.Type, col: Column) extends FocusId
    implicit def equalityCI: UnivEq[InCI] = UnivEq.derive
    implicit def equality: UnivEq[FocusId] = UnivEq.derive
    implicit def reusability: Reusability[FocusId] = Reusability.byUnivEq
  }
}
