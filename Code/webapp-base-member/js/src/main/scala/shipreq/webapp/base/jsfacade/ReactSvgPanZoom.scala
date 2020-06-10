package shipreq.webapp.base.jsfacade

import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.SyntheticEvent
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation._

// https://github.com/chrvadala/react-svg-pan-zoom/blob/master/docs/documentation.md
object ReactSvgPanZoom {

  @js.native sealed trait Action   extends js.Any
  @js.native sealed trait AlignX   extends js.Any
  @js.native sealed trait AlignY   extends js.Any
  @js.native sealed trait Mode     extends js.Any
  @js.native sealed trait Position extends js.Any
  @js.native sealed trait Tool     extends js.Any

  @js.native
  sealed trait Value extends js.Object {
    var viewerWidth : js.UndefOr[Double]
    var viewerHeight: js.UndefOr[Double]
    var SVGWidth    : js.UndefOr[Double]
    var SVGHeight   : js.UndefOr[Double]
    var SVGMinX     : js.UndefOr[Double]
    var SVGMinY     : js.UndefOr[Double]
  }

  def Value(): Value =
    (new js.Object).asInstanceOf[Value]

  @JSGlobal("RSPZ")
  @js.native
  object Exports extends js.Object {
  //val ACTION_PAN                  : Action             = js.native
  //val ACTION_ZOOM                 : Action             = js.native
  //val ALIGN_BOTTOM                : AlignY             = js.native
    val ALIGN_CENTER                : AlignX with AlignY = js.native
  //val ALIGN_COVER                 : Align?             = js.native
  //val ALIGN_LEFT                  : AlignX             = js.native
  //val ALIGN_RIGHT                 : AlignX             = js.native
  //val ALIGN_TOP                   : AlignY             = js.native
    val INITIAL_VALUE               : Value              = js.native
  //val MODE_IDLE                   : Mode               = js.native
  //val MODE_PANNING                : Mode               = js.native
  //val MODE_ZOOMING                : Mode               = js.native
  //val Miniature                   : js.Any             = js.native // f(props)
  //val POSITION_BOTTOM             : Position           = js.native
  //val POSITION_LEFT               : Position           = js.native
  //val POSITION_NONE               : Position           = js.native
    val POSITION_RIGHT              : Position           = js.native
  //val POSITION_TOP                : Position           = js.native
    val ReactSVGPanZoom             : js.Any             = js.native // f(props, context)
    val TOOL_AUTO                   : Tool               = js.native
  //val TOOL_NONE                   : Tool               = js.native
  //val TOOL_PAN                    : Tool               = js.native
  //val TOOL_ZOOM_IN                : Tool               = js.native
  //val TOOL_ZOOM_OUT               : Tool               = js.native
  //val Toolbar                     : js.Any             = js.native // f(_ref)
  //val UncontrolledReactSVGPanZoom : js.Any             = js.native // f(props)
  //val Viewer                      : js.Any             = js.native // f()
  //val closeMiniature              : js.Any             = js.native // f(value)
  //val fitSelection                : js.Any             = js.native // f(value, selectionSVGPointX, selectionSVGPointY, selectionWidth, selectionHeight)
    val fitToViewer                 : FitToViewer        = js.native // f(value)
  //val openMiniature               : js.Any             = js.native // f(value)
  //val pan                         : js.Any             = js.native // f(value, SVGDeltaX, SVGDeltaY)
  //val reset                       : js.Any             = js.native // f(value)
  //val setPointOnViewerCenter      : js.Any             = js.native // f(value, SVGPointX, SVGPointY, zoomLevel)
  //val zoom                        : js.Any             = js.native // f(value, SVGPointX, SVGPointY, scaleFactor)
  //val zoomOnViewerCenter          : js.Any             = js.native // f(value, scaleFactor)
  }

  type OnChangeTool  = js.Function1[Tool, Unit]
  type OnChangeValue = js.Function1[Value, Unit]
  type OnMouseEvent  = js.Function1[ViewerMouseEvent, Unit]
  type FitToViewer   = js.Function3[Value, AlignX, AlignY, Value]

  @js.native
  trait Props extends js.Object {
    var width         : Double                     = js.native
    var height        : Double                     = js.native
    var value         : Value                      = js.native
    var onChangeValue : OnChangeValue              = js.native
    var tool          : Tool                       = js.native
    var onChangeTool  : OnChangeTool               = js.native
    var background    : js.UndefOr[String]         = js.native
    var detectAutoPan : js.UndefOr[Boolean]        = js.native
    var miniatureProps: js.UndefOr[MiniatureProps] = js.native
    var toolbarProps  : js.UndefOr[ToolbarProps]   = js.native

    // SVGBackground                       white    String                                 Background of the SVG
    // SVGStyle                            {}       Object                                 Style of the SVG
    // style                               -        Object                                 CSS style of the viewer
    // className                           -        String                                 CSS class of the viewer
    // detectWheel                         true     Boolean                                Perform zoom operation on mouse scroll
    // detectAutoPan                       true     Boolean                                Perform PAN if the mouse is on the border of the viewer
    // detectPinchGesture                  true     Boolean                                Perform zoom operation on pinch gesture
    // onZoom                              -        fn(value: object)                      Callback called when the zoom level changes
    // onPan                               -        fn(value: object)                      Callback called when a pan action is performed
    // onClick                             -        fn(viewerEvent: ViewerMouseEvent)      Handler* for click
    // onDoubleClick                       -        fn(viewerEvent: ViewerMouseEvent)      Handler* for dblclick
    // onMouseUp                           -        fn(viewerEvent: ViewerMouseEvent)      Handler* for mouseup
    // onMouseMove                         -        fn(viewerEvent: ViewerMouseEvent)      Handler* for mousemove
    // onMouseDown                         -        fn(viewerEvent: ViewerMouseEvent)      Handler* for mousedown
    // onTouchStart                        -        fn(viewerEvent: ViewerTouchEvent)      Handler* for mousedown
    // onTouchMove                         -        fn(viewerEvent: ViewerTouchEvent)      Handler* for mousedown
    // onTouchEnd                          -        fn(viewerEvent: ViewerTouchEvent)      Handler* for mousedown
    // onTouchCancel                       -        fn(viewerEvent: ViewerTouchEvent)      Handler* for mousedown
    // preventPanOutside                   true     Boolean                                User can't move the image outside the viewer
    // scaleFactor                         1.1      Number                                 How much scale in or out (%)
    // scaleFactorOnWheel                  1.06     Number                                 how much scale in or out on mouse wheel (requires detectWheel to be enabled) (%)
    // scaleFactorMax                      -        Number                                 maximum amount of scale a user can zoom in to
    // scaleFactorMin                      -        Number                                 minimum amount of scale a user can zoom out of
    // modifierKeys                        -        Array                                  Array with modifier keys used with the tool auto to swap zoom in and zoom out (Accepted value)
    // disableDoubleClickZoomWithToolAuto  false    Boolean                                Turn off zoom on double click
    // customMiniature                     -        Component                              Override miniature component
    // customToolbar                       -        Component                              Override toolbar component
  }

  @js.native
  trait MiniatureProps extends js.Object {
    var position  : js.UndefOr[Position] = js.native // Default = left
    var background: js.UndefOr[String]   = js.native // Default = #616264
    var width     : js.UndefOr[Double]   = js.native // Default = 100
    var height    : js.UndefOr[Double]   = js.native // Default = 80
  }

  @js.native
  trait ToolbarProps extends js.Object {
    var position       : js.UndefOr[Position] = js.native // Default = right   Toolbar position
    var SVGAlignX      : js.UndefOr[AlignX]   = js.native // Default = left    X Alignment used for "Fit to Viewer" action
    var SVGAlignY      : js.UndefOr[AlignY]   = js.native // Default = top     Y Alignment used for "Fit to Viewer" action
    var activeToolColor: js.UndefOr[String]   = js.native // Default = #1CA6FC Color of active and hovered tool icons
  }

  def Props(width         : Double,
            height        : Double,
            value         : Value,
            onChangeValue : Value => Callback,
            tool          : Tool,
            onChangeTool  : Tool => Callback,
            background    : js.UndefOr[String]         = js.undefined,
            detectAutoPan : js.UndefOr[Boolean]        = js.undefined,
            miniatureProps: js.UndefOr[MiniatureProps] = js.undefined,
            toolbarProps  : js.UndefOr[ToolbarProps]   = js.undefined,
           ): Props =
    js.Dynamic.literal(
      width          = width,
      height         = height,
      value          = value,
      onChangeValue  = onChangeValue.andThen(_.runNow()): OnChangeValue,
      tool           = tool,
      onChangeTool   = onChangeTool.andThen(_.runNow()): OnChangeTool,
      background     = background,
      detectAutoPan  = detectAutoPan,
      miniatureProps = miniatureProps,
      toolbarProps   = toolbarProps,
    ).asInstanceOf[Props]

  def MiniatureProps(position  : js.UndefOr[Position] = js.undefined,
                     background: js.UndefOr[String]   = js.undefined,
                     width     : js.UndefOr[Double]   = js.undefined,
                     height    : js.UndefOr[Double]   = js.undefined,
                    ): MiniatureProps =
    js.Dynamic.literal(
      position   = position,
      background = background,
      width      = width,
      height     = height,
    ).asInstanceOf[MiniatureProps]

  def ToolbarProps(position       : js.UndefOr[Position] = js.undefined,
                   SVGAlignX      : js.UndefOr[AlignX]   = js.undefined,
                   SVGAlignY      : js.UndefOr[AlignY]   = js.undefined,
                   activeToolColor: js.UndefOr[String]   = js.undefined,
                    ): ToolbarProps =
    js.Dynamic.literal(
      position        = position,
      SVGAlignX       = SVGAlignX,
      SVGAlignY       = SVGAlignY,
      activeToolColor = activeToolColor,
    ).asInstanceOf[ToolbarProps]

  @js.native
  sealed trait ViewerMouseEvent extends js.Object {
    val originalEvent: SyntheticEvent[dom.Node]
    // SVGViewer	SVGSVGElement	Reference to SVGViewer
    // point	Object	Coordinates (x,y) of the event mapped to SVG coordinates
    val x           : Double
    val y           : Double
    val scaleFactor : Double
    val translationX: Double
    val translationY: Double
    def preventDefault(): Unit
    def stopPropagation(): Unit
  }

  @js.native
  sealed trait State extends js.Object {
    val defaultValue: Value
  }

  @js.native
  sealed trait Backend extends js.Object {

    /** Fit all SVG to Viewer (SVGAlignX: one of left, center, right, SVGAlignY: one of top, center, bottom) */
    def fitToViewer(SVGAlignX: AlignX = js.native, SVGAlignY: AlignY = js.native): Unit

    /** Reset Viewer view to default */
    def reset(): Unit

    // def pan(SVGDeltaX, SVGDeltaY)	Apply a pan
    // def zoom(SVGPointX, SVGPointY, scaleFactor)	Zoom in or out the SVG
    // def fitSelection(selectionSVGPointX, selectionSVGPointY, selectionWidth, selectionHeight)	Fit an SVG area to viewer
    // def setPointOnViewerCenter(SVGPointX, SVGPointY, zoomLevel)	Set a point on Viewer center
    // def zoomOnViewerCenter(scaleFactor)	Zoom SVG on center
    // def getValue()	Get current viewer value
    // def setValue(value)	Through this method you can set a new value
    // def getTool()	Get current tool
    // def changeTool(tool)	Change the tool (one of none,pan,zoom-in,zoom-out,auto)
    // def openMiniature	Open the miniature
    // def closeMiniature	Close the miniature
  }

  val Component = JsComponent[Props, Children.Varargs, State](Exports.ReactSVGPanZoom)
    .addFacade[Backend]

}
