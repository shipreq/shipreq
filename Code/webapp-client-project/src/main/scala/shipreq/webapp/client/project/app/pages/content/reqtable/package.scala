package shipreq.webapp.client.project.app.pages.content

import japgolly.scalajs.react._
import shipreq.webapp.client.project.feature.Selection

package object reqtable {

  type SetFn[A] = Reusable[SetStateFnPure[A]]
  type ModFn[A] = Reusable[ModStateFnPure[A]]

  type RowSelection        = Selection[Row.SourceId]
  type RowSelectionVisible = Selection.LegalWithUpdateFn[Row.SourceId]

  // TODO Delete shouldComponentUpdate aliases now that we have ReusabilityOverlay.installGloballyInDev()
  @inline def shouldComponentUpdate[P: Reusability, C <: Children, S: Reusability, B, U <: UpdateSnapshot]: ScalaComponent.Config[P, C, S, B, U, U] =
    shipreq.webapp.client.project.app.shouldComponentUpdate[P, C, S, B, U]
//   ReusabilityOverlay.install[P, C, S, B]

}
