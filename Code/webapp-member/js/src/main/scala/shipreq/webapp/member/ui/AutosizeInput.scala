package shipreq.webapp.member.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import shipreq.base.util.{Disabled, Enabled, Identity}

object AutosizeInput {

  final case class Props(state     : StateSnapshot[State],
                         tagMod    : TagMod                         = EmptyVdom,
                         correct   : String => String               = Identity.apply,
                         enabled   : Enabled                        = Enabled,
                         ref       : Option[Ref.Simple[html.Input]] = None,
                         extraWidth: Option[String]                 = None) {

    @inline def render: VdomElement = Component(this)
  }

  type State = String

  private[this] val styleShared =
    TagMod(
      ^.font := "inherit",
      ^.padding := "0",
      ^.margin := "0",
    )

  private[this] val baseSpan =
    <.span(
      styleShared,
      ^.position.absolute,
      ^.height := "0",
      ^.overflow.hidden,
      ^.whiteSpace.pre,
    )

  private[this] val baseInput =
    <.input.text(styleShared)

  final class Backend($: BackendScope[Props, Unit]) {

    private val onChange: ReactEventFromInput => Callback =
      _.extract(_.target.value)(i => $.props.flatMap(p => p.state.setState(p.correct(i), resize)))

    private val spanRef  = Ref[html.Span]
    private val inputRef = Ref[html.Input]

    def render(p: Props): VdomNode = {
      val v = p.state.value

      val span =
        baseSpan(v).withRef(spanRef)

      val input =
        baseInput(
          p.tagMod,
          ^.value := v,
          ^.onChange ==> onChange,
          ^.disabled := p.enabled.is(Disabled),
        ).withRef(p.ref getOrElse inputRef)

      React.Fragment(span, input)
    }

    lazy val resize: Callback =
      for {
        s <- spanRef.get.asCBO
        p <- $.props.toCBO
        i <- p.ref.getOrElse(inputRef).get.asCBO
      } yield {
        val width1 = s"${s.offsetWidth}px"
        val width = p.extraWidth.fold(width1)(extra => s"calc($width1 + $extra)")
        i.style.width = width
        (i.style.width: Any) match {
          case "" | null | () => resize.delayMs(1).toCallback.logAround("RETRYING....").runNow()
          case _              =>
        }
      }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .componentDidMount(_.backend.resize)
    .componentDidUpdate(_.backend.resize)
    .build
}