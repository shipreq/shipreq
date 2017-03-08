package shipreq.webapp.client.project

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability

package object app {

  @inline def shouldComponentUpdate[P: Reusability, C <: Children, S: Reusability, B]: ScalaComponentConfig[P, C, S, B] =
    Reusability.shouldComponentUpdate[P, C, S, B]
  //  Reusability.shouldComponentUpdateWithOverlay[P, C, S, B]
  //  { val no = (_: Any) => scalaz.effect.IO(())
  //    ReusabilityOverlay.install[P, S, B, N](DefaultReusabilityOverlay.defaults
  //      .copy(updateHighlighter = no, mountHighlighter = no))(Reusability const false, Reusability const false)
  //  }

}
