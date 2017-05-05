package shipreq.webapp.client.project.app

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import shipreq.webapp.client.project.feature.Selection
import shipreq.webapp.client.project.lib.DataReusability._

package object reqtable2 {

  type SetFn[A] = A ~=> Callback
  type ModFn[A] = (A => A) ~=> Callback

  type RowSelection        = Selection[Row.SourceId]
  type RowSelectionVisible = Selection.LegalWithUpdateFn[Row.SourceId]

  implicit val reusabilityNonEmptyVectorColumn: Reusability[NonEmptyVector[Column]] =
    Reusability.byRef || reusabilityNonEmptyVector

  implicit val reusabilityNonEmptyVectorColumnPlus: Reusability[NonEmptyVector[ColumnPlus]] =
    Reusability.byRef || reusabilityNonEmptyVector

  @inline def shouldComponentUpdate[P: Reusability, C <: Children, S: Reusability, B]: ScalaComponent.Config[P, C, S, B] =
//    shipreq.webapp.client.project.app.shouldComponentUpdate[P, C, S, B]
   Reusability.shouldComponentUpdateWithOverlay[P, C, S, B]

}
