package shipreq.webapp.client.project

import japgolly.scalajs.react.{Reusability, _}

package object app {

  @inline def shouldComponentUpdate[P: Reusability, C <: Children, S: Reusability, B, U <: UpdateSnapshot]: ScalaComponent.Config[P, C, S, B, U, U] =
    Reusability.shouldComponentUpdate[P, C, S, B, U]
  //  ReusabilityOverlay.install[P, C, S, B]
  //  { val no = (_: Any) => cats.effect.IO(())
  //    ReusabilityOverlay.install[P, S, B, N](DefaultReusabilityOverlay.defaults
  //      .copy(updateHighlighter = no, mountHighlighter = no))(Reusability const false, Reusability const false)
  //  }

}
