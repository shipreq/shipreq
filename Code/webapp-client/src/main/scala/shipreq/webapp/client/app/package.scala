package shipreq.webapp.client

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability

package object app {

  @inline def shouldComponentUpdate[P: Reusability, S: Reusability, B, N <: TopNode] =
    Reusability.shouldComponentUpdate[P, S, B, N]
  //  Reusability.shouldComponentUpdateWithOverlay[P, S, B, N]
  //  { val no = (_: Any) => scalaz.effect.IO(())
  //    ReusabilityOverlay.install[P, S, B, N](DefaultReusabilityOverlay.defaults
  //      .copy(updateHighlighter = no, mountHighlighter = no))(Reusability const false, Reusability const false)
  //  }

}
