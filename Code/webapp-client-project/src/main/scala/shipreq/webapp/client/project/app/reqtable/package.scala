package shipreq.webapp.client.project.app

import japgolly.scalajs.react._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra._
import shipreq.base.util.univeq._
import shipreq.webapp.client.project.feature.Selection

package object reqtable {

  type Column = shipreq.webapp.base.data.reqtable.Column
  val Column = shipreq.webapp.base.data.reqtable.Column
  @inline implicit def ColumnImplicitExt(o: Column.type) = ColumnExt
  implicit def reusability: Reusability[Column] = Reusability.byEqual

  type SetFn[A] = A ~=> Callback
  type ModFn[A] = (A => A) ~=> Callback

  type RowSelection        = Selection[Row.SourceId]
  type RowSelectionVisible = Selection.LegalWithUpdateFn[Row.SourceId]

  @inline def shouldComponentUpdate[P: Reusability, C <: Children, S: Reusability, B]: ScalaComponent.Config[P, C, S, B] =
    shipreq.webapp.client.project.app.shouldComponentUpdate[P, C, S, B]
//   Reusability.shouldComponentUpdateWithOverlay[P, C, S, B]

}
