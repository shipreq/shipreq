package shipreq.webapp.client.project.app

import japgolly.scalajs.react._

package object issues {
  @inline def shouldComponentUpdate[P: Reusability, C <: Children, S: Reusability, B, U <: UpdateSnapshot]: ScalaComponent.Config[P, C, S, B, U, U] =
    shipreq.webapp.client.project.app.shouldComponentUpdate[P, C, S, B, U]
//   ReusabilityOverlay.install[P, C, S, B]

}
