package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react.ScalazReact.reusabilityDisjunction
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.raw.{SVGGElement, SVGSVGElement}
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.data.Svg
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.base.ui.SvgPanZoom
import shipreq.webapp.client.project.app.{Style, WebWorkerClient}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.ww.api.WebWorkerCmd.picklerErrorMsgOrSvg
import shipreq.webapp.client.ww.api._

/**
 * The reusable bits of a component the renders a graph through the WebWorker API.
 *
 * Usage:
 * 1. Make your `Props` extend `HasWebWorker`.
 * 2. Make your `Backend` extend `GraphBackend`.
 * 3. Call `.graphState` when creating the component.
 * 4. Call `.configure(graphConfig)` when creating the component.
 */
object GraphComponent {

  abstract class HasWebWorker {
    val webWorker: WebWorkerClient.Instance
  }

  /**
   * @param validity Invalid means data is stale (i.e. there's a new graph on the way)
   */
  final case class State(value: Option[ErrorMsg \/ Svg], validity: Validity)

  object State {
    def init: State =
      apply(None, Valid)

    implicit def reusability: Reusability[State] =
      Reusability.byRef || Reusability.derive
  }

  sealed trait DisplayMode
  object DisplayMode {
    case object PanZoom extends DisplayMode
    case object FitToWidth extends DisplayMode
  }

  private[this] val container =
    <.div(^.display.inline)

  abstract class GraphBackend[Props <: HasWebWorker]($: BackendScope[Props, State]) {

    protected def displayMode(p: Props): DisplayMode

    def cmd(p: Props): WebWorkerCmd[ErrorMsg \/ Svg]

    def refresh(p: Props): Callback =
      $.modState(_.copy(validity = Invalid)) >>
        p.webWorker.post(cmd(p))
          .flatMapSync(result => $.setState(State(Some(result), Valid)))
          .toCallback

    def onRender(prevProps: Option[Props], p: Props, s: State)(implicit r: Reusability[Props]): Callback = {
      val maybeRefresh = Callback.unless(prevProps.exists(_ ~=~ p))(refresh(p))
      val maybeEnrich  = Callback.when(s.value.exists(_.isRight))(enrichAttempt(p, 30, 400))
      maybeRefresh >> maybeEnrich
    }

    private def enrichAttempt(p: Props, delayMs: Double, remainingDelayMs: Double): Callback =
      $.getDOMNode.map(_.toElement).asCBO.flatMap { dom =>

        // The first svg is the result we care about.
        // The others are ReactSvgPanZoom drawing tools and the minimap.
        dom.querySelector("svg") match {

          case svg: SVGSVGElement =>
            enrich(p, svg).toCBO

          case _ =>
            enrichAttempt(p, delayMs, remainingDelayMs - delayMs)
              .delayMs(delayMs)
              .toCallback
              .when_(remainingDelayMs >= delayMs)
              .toCBO
        }
      }

    @nowarn("cat=unused")
    def enrich(p: Props, root: SVGSVGElement): Callback =
      Callback.empty

    def render(p: Props, s: State): VdomElement = {
      val content: VdomElement =
        (s.value, displayMode(p)) match {
          case (Some(\/-(svg)), DisplayMode.FitToWidth) => <.div(Style.svgGraphFitToWidth, ^.dangerouslySetInnerHtml := svg.content)
          case (Some(\/-(svg)), DisplayMode.PanZoom)    => SvgPanZoom.Props(svg).render
          case (Some(-\/(err)), _)                      => <.div(Style.svgGraphError, err.value)
          case (None, _)                                => <.div
        }
      s.validity match {
        case Valid   => container(content)
        case Invalid => container(Style.svgGraphInvalid, content)
      }
    }

    protected def graphNodeIterator(root: SVGSVGElement): Iterator[SVGGElement] =
      root.querySelectorAll("g.node").iterator.map(_.domCast[SVGGElement])
  }

  def graphConfig[P <: HasWebWorker : Reusability, C <: Children, B <: GraphBackend[P]]: ScalaComponent.Config[P, C, State, B, UpdateSnapshot.None, UpdateSnapshot.Some[Unit]] =
    _.configure(Reusability.shouldComponentUpdate)
      .componentDidMount($ => $.backend.onRender(None, $.props, $.state))
      .componentDidUpdate(i => i.backend.onRender(Some(i.prevProps), i.currentProps, i.currentState))
}
