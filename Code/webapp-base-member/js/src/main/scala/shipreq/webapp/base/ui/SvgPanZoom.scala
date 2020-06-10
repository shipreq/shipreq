package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import scala.scalajs.js
import shipreq.webapp.base.data.Svg
import shipreq.webapp.base.jsfacade.{ReactSvgPanZoom, ReactVirtualized}

object SvgPanZoom {
  import ReactSvgPanZoom.Exports._

  final case class Props(svg: Svg) {
    @inline def render: VdomElement = Component(this)
  }

  final case class State(svg  : Svg,
                         value: ReactSvgPanZoom.Value,
                         tool : ReactSvgPanZoom.Tool)

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

    // private var autoFitted: Boolean = false

    def render(p: Props, s: State): VdomNode = {
      container(
        ReactVirtualized.AutoSize { dims =>

          // Can't get ReactSvgPanZoom to center on start
//          var v = s.value
//          if (!autoFitted) {
//            autoFitted = true
//
//            val svgProps = p.svg.vdom.rawNode.asInstanceOf[js.Dynamic].props
//            def parse(value: js.Any): Double = value.asInstanceOf[String].stripSuffix("pt").toDouble
//            val svgW = parse(svgProps.width)
//            val svgH = parse(svgProps.height)
//
//            if (svgW < dims.width) {
//              v = js.Object.assign(new js.Object, v).asInstanceOf[ReactSvgPanZoom.Value]
//              v.viewerWidth = dims.width
//              v.viewerHeight = dims.height
//              v.SVGWidth = svgW
//              v.SVGHeight = svgH
//              v.SVGMinX = 0.0
//              v.SVGMinY = 0.0
//
//              v = fitToViewer(v, ALIGN_CENTER, ALIGN_CENTER)
//              org.scalajs.dom.console.log(s"($svgW,$svgH) vs (${dims.width},${dims.height}) ==> ", s.value, v)
//            }
//          }

//          for (rendered <- lastRendered) {
//            val lastAutoFitForCurrentSvg = lastAutoFitted.exists(_ eq rendered)
//            if (!lastAutoFitForCurrentSvg) {
//              lastAutoFitted = Some(rendered)
//              //            m.raw.fitToViewer(ALIGN_CENTER, ALIGN_CENTER)
//              //m.raw.reset()
//
//              org.scalajs.dom.console.log("SVG:" , p.svg.vdom.rawNode)
//              val svgProps = p.svg.vdom.rawNode.asInstanceOf[js.Dynamic].props
//              def parse(value: js.Any): Double = value.asInstanceOf[String].stripSuffix("pt").toDouble
//
//              val svgW = parse(svgProps.width)
//              val svgH = parse(svgProps.height)
//
//              if (svgW < dims.width) {
//                v = js.Object.assign(new js.Object, v).asInstanceOf[ReactSvgPanZoom.Value]
//                v.viewerWidth = dims.width
//                v.viewerHeight = dims.height
//                v.SVGWidth = svgW
//                v.SVGHeight = svgH
//
//                org.scalajs.dom.console.log(s"($svgW,$svgH) vs (${dims.width},${dims.height}) ==> ", v, fitToViewer(v, ALIGN_CENTER, ALIGN_CENTER))
//                v = fitToViewer(v, ALIGN_CENTER, ALIGN_CENTER)
//              }
//            }
//          }

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
          ReactSvgPanZoom.Component(props)(
            p.svg.vdom
          )
        }
      )
    }
  }

  implicit val reusabilityProps: Reusability[Props] = Reusability.derive
  implicit val reusabilityState: Reusability[State] = Reusability.byRef

  private def deriveState(p: Props, prevState: Option[State]): State =
    prevState match {
      case Some(s) if s.svg ==* p.svg =>
        s // no state change
      case _ =>
        State(
          svg   = p.svg,
          value = INITIAL_VALUE,
          tool  = TOOL_AUTO,
        )
    }

  val Component = ScalaComponent.builder[Props]
    .getDerivedStateFromPropsAndState(deriveState)
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
