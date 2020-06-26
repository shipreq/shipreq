package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.jsfacade.Screenfull
import shipreq.webapp.base.ui.{BaseStyles => *}

object OptionalFullscreen {

  final case class Ctx(currentlyFullscreen: Boolean,
                       toggleFullscreen: Callback)

  final case class Props(renderer: Ctx => VdomNode) {
    @inline def render: VdomElement = Component(this)
  }

  final case class State(fullscreen: Boolean) {
    def toggle = State(!fullscreen)
  }

  object State {
    def init: State =
      apply(fullscreen = false)
  }

  private val fullscreenContainer =
    <.div(*.fullscreen)

  private val browserFullscreenEnter: Callback =
    Callback {
      if (Screenfull.isEnabled)
        Screenfull.request()
    }

  private val browserFullscreenExit: Callback =
    Callback {
      if (Screenfull.isEnabled)
        Screenfull.exit()
    }

  final class Backend($: BackendScope[Props, State]) {

    private val toggleFullscreen: Callback =
      for {
        s1 <- $.state
        s2 <- CallbackTo.pure(s1.toggle)
        _  <- $.setState(s2)
        _  <- if (s2.fullscreen) browserFullscreenEnter else browserFullscreenExit
      } yield ()

    def render(p: Props, s: State): VdomNode = {
      val ctx = Ctx(s.fullscreen, toggleFullscreen)
      val inner = p.renderer(ctx)
      if (s.fullscreen)
        fullscreenContainer(inner)
      else
        inner
    }
  }

  val Component = ScalaComponent.builder[Props]
    .initialState(State.init)
    .renderBackend[Backend]
    .build
}