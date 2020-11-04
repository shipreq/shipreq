package shipreq.webapp.member.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import java.time.Duration
import org.scalajs.dom.MouseEvent
import scala.scalajs.js
import shipreq.webapp.base.ui.semantic._

/** WARNING! This currently uses a dirty, dirty hack!!
  * On mount, it replaces the onmousemove callback of the PARENT dom element.
  *
  * To control the animation speed, set the animationDuration directly on the `content` tag.
  */
object OnlyVisibleOnMouseMove {

  // For unit tests
  var allowHide = true

  final case class Props(content      : VdomTag,
                         transition   : Transition,
                         direction    : Transition.Direction,
                         decay        : Duration,
                         showInitially: Boolean) {

    @inline def render: VdomNode = Component(this)
  }

  final case class State(shownYet: Boolean,
                         show    : Boolean,
                         decaying: Option[Callback.SetTimeoutResult]) {

    def onShow: State =
      copy(shownYet = true, show = true)

    def onHide: State =
      copy(show = false)
  }

  object State {
    def init(show: Boolean): State =
      apply(
        shownYet = show,
        show     = show,
        decaying = None)
  }

  private def rateLimit(c: Callback): Callback =
    c.rateLimitMs(100).void

  final class Backend($: BackendScope[Props, State]) {

    private var mounted = false

    private val clearTimeout: Callback =
      $.state.flatMap { s =>
        Callback.traverseOption(s.decaying) { h =>
          h.cancel >> $.modStateOption(s2 => Option.when(s2.decaying eq s.decaying)(s2.copy(decaying = None)))
        }
      }

    private val hide: Callback =
      $.modState(_.onHide).when_(mounted && allowHide)

    private val decay: Callback =
      for {
        _ <- clearTimeout
        p <- $.props
        d <- hide.setTimeout(p.decay).when(mounted && allowHide)
        _ <- $.modStateOption(s => d.map(_ => s.copy(decaying = d)))
      } yield ()

    private val show: Callback =
      clearTimeout >> $.modStateOption(s => Option.unless(s.show)(s.onShow))

    private val mods: TagMod =
      TagMod(
        ^.onMouseMove ==> (stopPropagation(_) >> rateLimit(show)),
        ^.onMouseLeave ==> (stopPropagation(_) >> decay))

    def render(p: Props, s: State): VdomNode = {
      val cls = Transition.cls(s.show, p.transition, p.direction)

      if (!s.shownYet && !s.show)
        p.content(mods, cls, ^.visibility.hidden) // avoid closing animation
      else
        p.content(mods, cls)
    }

    private def hackySetMouseMoveListener(f: js.Function1[MouseEvent, _]): Callback =
      $.getDOMNode.map(_.toHtml).asCBO
        .flatMap(d => CallbackTo(d.parentElement).attemptTry.map(_.toOption).asCBO)
        .map { dom => dom.onmousemove = f }

    val onMount: Callback = {
      val setMounted           = Callback { mounted = true }
      val mouseListener        = rateLimit(show >> decay).when_(mounted)
      val installMouseListener = hackySetMouseMoveListener(mouseListener.toJsFn1)
      val startDecay           = $.state.flatMap(s => Callback.when(s.show)(decay))
      setMounted >> installMouseListener >> startDecay
    }

    val onUnmount: Callback = {
      val setUnmounted        = Callback { mounted = false }
      val removeMouseListener = hackySetMouseMoveListener(null)
      setUnmounted >> removeMouseListener >> clearTimeout
    }
  }

  val Component = ScalaComponent.builder[Props]
    .initialStateFromProps(p => State.init(p.showInitially))
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount)
    .componentWillUnmount(_.backend.onUnmount)
    .build
}