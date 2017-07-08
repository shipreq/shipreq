package shipreq.webapp.client.public.root

import monocle.macros.Lenses
import shipreq.webapp.client.public.pages._

@Lenses
final case class State(landingPage: LandingPage.State)

object State {
  def init: State =
    State(LandingPage.State.init)
}