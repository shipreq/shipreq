package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.document
import scalacss.ScalaCssReact._
import shipreq.webapp.base.jsfacade.Screenfull
import shipreq.webapp.base.ui.{BaseStyles => *}

trait OptionalFullscreen {
  def apply(f: OptionalFullscreen.Ctx => VdomNode): VdomNode
}

object OptionalFullscreen {

  // When true, hitting the fullscreen button will make the browser itself go fullscreen.
  // When false, we only go fullscreen within the viewport.
  //
  // Setting to false because I always have to toggle browser fullscreen off when I use this.
  // We'll leave it to users to control the browser themselves.
  //
  // Maybe this should be a setting in users' profiles one day?
  private final val browserFullscreen = false

  final case class Ctx(currentlyFullscreen: Boolean,
                       toggleFullscreen   : Callback) {

    def enterFullscreen: Option[Callback] =
      Option.unless(currentlyFullscreen)(toggleFullscreen)

    def exitFullscreen: Option[Callback] =
      Option.when(currentlyFullscreen)(toggleFullscreen)
  }

  implicit def reusability: Reusability[OptionalFullscreen] =
    Reusability.byRef

  val real: OptionalFullscreen = {
    val browserFullscreenEnter: Callback =
      Callback {

        // Hide scroll bars. Just because we're overlaying a div to the viewport, doesn't mean the rest of the content
        // that's underneath the overlay, goes away.
        document.body.style.overflow = "hidden"

        if (browserFullscreen && Screenfull.isEnabled)
          Screenfull.request()
      }

    val browserFullscreenExit: Callback =
      Callback {

        document.body.style.overflow = ""

        if (Screenfull.isEnabled)
          Screenfull.exit()
      }

    val impl = new Impl(browserFullscreenEnter, browserFullscreenExit)

    impl.Component(_)
  }

  final class Impl(fullscreenEnter: Callback, fullscreenExit: Callback) {

    type Props = Ctx => VdomNode

    case class State(fullscreen: Boolean) {
      def toggle = State(!fullscreen)
    }

    object State {
      def init: State =
        apply(fullscreen = false)
    }

    private val fullscreenContainer =
      <.div(*.fullscreen)

    final class Backend($: BackendScope[Props, State]) {

      private val toggleFullscreen: Callback =
        for {
          s1 <- $.state
          s2 <- CallbackTo.pure(s1.toggle)
          _  <- $.setState(s2)
          _  <- if (s2.fullscreen) fullscreenEnter else fullscreenExit
        } yield ()

      def render(p: Props, s: State): VdomNode = {
        val ctx = Ctx(s.fullscreen, toggleFullscreen)
        val inner = p(ctx)
        if (s.fullscreen)
          fullscreenContainer(inner)
        else
          inner
      }

      val onUnmount =
        $.state.flatMap(s => fullscreenExit.when_(s.fullscreen))
    }

    val Component = ScalaComponent.builder[Props]
      .initialState(State.init)
      .renderBackend[Backend]
      .componentWillUnmount(_.backend.onUnmount)
      .build
  }
}