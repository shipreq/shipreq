package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.Svg
import shipreq.webapp.base.jsfacade.{ReactSvgPanZoom, ReactVirtualized}

object SvgPanZoom {
  import ReactSvgPanZoom.Exports._

  final case class Props(svg: Svg) {
    @inline def render: VdomElement = Component(this)
  }

  final case class State(svg        : Svg,
                         value      : ReactSvgPanZoom.Value,
                         tool       : ReactSvgPanZoom.Tool,
                         var autoFit: Boolean)

  private val container = <.div(^.width := "100%", ^.height := "100%")

  private val miniatureProps =
    ReactSvgPanZoom.MiniatureProps(
      position = POSITION_RIGHT,
    )

  private val toolbarProps =
    ReactSvgPanZoom.ToolbarProps(
      SVGAlignX = ALIGN_CENTER,
      SVGAlignY = ALIGN_CENTER,
    )

  final class Backend($: BackendScope[Props, State]) {

    private val ref = Ref.toJsComponent(ReactSvgPanZoom.Component)

    val onUpdate: Callback =
      for {
        m <- ref.get
        s <- $.state.toCBO
        _ <- CallbackOption.require(s.autoFit)
      } yield {
        s.autoFit = false // using a var to avoid a modState here which would cause a re-render
        m.raw.fitToViewer(ALIGN_CENTER, ALIGN_CENTER)
        ()
      }

    def render(p: Props, s: State): VdomNode =
      container(
        ReactVirtualized.AutoSize { dims =>

          val props = ReactSvgPanZoom.Props(
            width          = dims.width,
            height         = dims.height,
            value          = s.value,
            onChangeValue  = v => $.modState(_.copy(value = v)),
            tool           = s.tool,
            onChangeTool   = t => $.modState(_.copy(tool = t)),
            detectAutoPan  = false,
            background     = "white",
            miniatureProps = miniatureProps,
            toolbarProps   = toolbarProps,
          )
          ReactSvgPanZoom.Component.withRef(ref).withKey(s.svg.content)(props)(
            p.svg.vdom
          )
        }
      )
  }

  implicit val reusabilityProps: Reusability[Props] = Reusability.derive
  implicit val reusabilityState: Reusability[State] = Reusability.byRef

  private def deriveState(p: Props, prevState: Option[State]): State =
    prevState match {
      case Some(s) if s.svg ==* p.svg =>
        s // no state change
      case _ =>
        State(
          svg     = p.svg,
          value   = INITIAL_VALUE,
          tool    = TOOL_AUTO,
          autoFit = true,
        )
    }

  val Component = ScalaComponent.builder[Props]
    .getDerivedStateFromPropsAndState(deriveState)
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .componentDidUpdate(_.backend.onUpdate)
    .build
}
