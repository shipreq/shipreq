package shipreq.webapp.client.project.app.pages.content

import japgolly.scalajs.react._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.lib.BaseReusability._
import shipreq.webapp.base.protocol.websocket.SavedViewCmd
import shipreq.webapp.client.project.feature.Selection

package object reqtable {

  type SetFn[A] = Reusable[SetStateFnPure[A]]
  type ModFn[A] = Reusable[ModStateFnPure[A]]

  type RowSelection        = Selection[Row.SourceId]
  type RowSelectionVisible = Selection.LegalWithUpdateFn[Row.SourceId]

  @inline implicit def ColumnImplicitExt(o: Column.type) = ColumnExt

  implicit val reusabilityColumn       : Reusability[Column             ] = Reusability.byUnivEq
  implicit val reusabilitySortCriterion: Reusability[SortCriterion      ] = Reusability.byRefOrUnivEq
  implicit val reusabilitySortCriteria : Reusability[SortCriteria       ] = Reusability.byRefOrUnivEq
  implicit val reusabilityView         : Reusability[View               ] = Reusability.byRefOrUnivEq
  implicit val reusabilitySavedViewsNE : Reusability[SavedViews.NonEmpty] = Reusability.byRefOrUnivEq
  implicit val reusabilitySavedViewId  : Reusability[SavedView.Id       ] = Reusability.byUnivEq
  implicit val reusabilitySavedViewName: Reusability[SavedView.Name     ] = Reusability.byUnivEq
  implicit val reusabilitySavedView    : Reusability[SavedView          ] = Reusability.byRef || Reusability.derive
  implicit val reusabilitySavedViewCmdD: Reusability[SavedViewCmd.Delete] = Reusability.byUnivEq

  @inline def shouldComponentUpdate[P: Reusability, C <: Children, S: Reusability, B, U <: UpdateSnapshot]: ScalaComponent.Config[P, C, S, B, U, U] =
    shipreq.webapp.client.project.app.shouldComponentUpdate[P, C, S, B, U]
//   ReusabilityOverlay.install[P, C, S, B]

}
