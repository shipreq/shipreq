package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.scalajs.js
import shipreq.webapp.base.data.Svg
import shipreq.webapp.base.jsfacade.{ReactSvgPanZoom, ReactVirtualized, TransformationMatrix}

object SvgPanZoom {
  import ReactSvgPanZoom.Exports._

  final case class Props(svg: Svg) {
    @inline def render: VdomElement = Component(this)
  }

  final case class State(svg        : Svg,
                         value      : ReactSvgPanZoom.Value,
                         tool       : ReactSvgPanZoom.Tool,
                         var autoFit: Boolean) // using a var to avoid a modState here which would cause a re-render

  private val container = <.div(^.width := "100%", ^.height := "100%")

  private final val maxInitialScale = 1.5

  private val miniatureProps =
    ReactSvgPanZoom.MiniatureProps(
      position = POSITION_RIGHT,
    )

  private val toolbarProps =
    ReactSvgPanZoom.ToolbarProps(
      SVGAlignX = ALIGN_CENTER,
      SVGAlignY = ALIGN_CENTER,
    )

  // Ripped off from react-svg-pan-zoom's fitToViewer in src/features/zoom.js
  def fitToViewerWithMaxScale(backend: ReactSvgPanZoom.Backend, maxScale: Double): Unit = {
    val v            = backend.getValue()
    val viewerWidth  = v.viewerWidth .get
    val viewerHeight = v.viewerHeight.get
    val SVGMinX      = v.SVGMinX     .get
    val SVGMinY      = v.SVGMinY     .get
    val SVGWidth     = v.SVGWidth    .get
    val SVGHeight    = v.SVGHeight   .get

    val scaleX     = viewerWidth / SVGWidth
    val scaleY     = viewerHeight / SVGHeight
    val origScale  = Math.min(scaleX, scaleY)
    val scaleLevel = Math.min(origScale, maxScale)

    var translateX, translateY = 0d

    if (scaleLevel < origScale) {
      val remainderX = viewerWidth - scaleLevel * SVGWidth
      val remainderY = viewerHeight - scaleLevel * SVGHeight
      translateX = Math.round(remainderX / 2) - SVGMinX * scaleLevel
      translateY = Math.round(remainderY / 2) - SVGMinY * scaleLevel
    } else if (scaleX < scaleY) {
      val remainderY = viewerHeight - scaleX * SVGHeight
      translateX = -SVGMinX * scaleX
      translateY = Math.round(remainderY / 2) - SVGMinY * scaleLevel
    } else {
      val remainderX = viewerWidth - scaleY * SVGWidth
      translateX = Math.round(remainderX / 2) - SVGMinX * scaleLevel
      translateY = -SVGMinY * scaleY
    }

    val scaleMatrix       = TransformationMatrix.scale(scaleLevel, scaleLevel)
    val translationMatrix = TransformationMatrix.translate(translateX, translateY)
    val matrix            = TransformationMatrix(translationMatrix, scaleMatrix)

    val newValue =
      if (isZoomLevelGoingOutOfBounds(v, scaleLevel / v.d)) {
        // Do not allow scale and translation
        set(v, js.Dynamic.literal(
          mode   = MODE_IDLE,
          startX = null,
          startY = null,
          endX   = null,
          endY   = null,
        ))
      } else {
        val patch = js.Object.assign(
          js.Dynamic.literal(
            mode   = MODE_IDLE,
            startX = null,
            startY = null,
            endX   = null,
            endY   = null,
          ),
          limitZoomLevel(v, matrix)
        )
        set(v, patch, ACTION_ZOOM)
      }

    backend.setValue(newValue)
  }

  final class Backend($: BackendScope[Props, State]) {

    private val ref = Ref.toJsComponent(ReactSvgPanZoom.Component)
    private val refGetOption = ref.get.asCallback

    lazy val fitToViewer: Callback =
      $.state.flatMap { s =>
        Callback.when(s.autoFit) {
          refGetOption.flatMap {
            case None => fitToViewer // no need for delay because it's debounced
            case Some(m) => Callback {
              s.autoFit = false
              fitToViewerWithMaxScale(m.raw, maxInitialScale)
            }
          }
        }
      }.debounceMs(16)

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
    .componentDidMount(_.backend.fitToViewer)
    .componentDidUpdate(_.backend.fitToViewer)
    .build
}
