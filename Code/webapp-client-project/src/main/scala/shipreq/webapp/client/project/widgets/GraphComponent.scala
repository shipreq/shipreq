package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.ScalaCssReact._
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.client.project.app.{Style, WebWorkerClient}
import shipreq.webapp.client.project.lib.DataReusability._
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
    val webWorker: WebWorkerClient
  }

  type State = Option[Svg]

  def initialState: State = None

  abstract class GraphBackend[Props <: HasWebWorker]($: BackendScope[Props, State]) {

    def cmd(p: Props): WebWorkerCmd[Svg]

    def refresh(p: Props): Callback =
      p.webWorker.post(cmd(p)).flatMapSync(svg => $.setState(Some(svg))).toCallback

    def onRender(prevProps: Option[Props], p: Props, s: State)(implicit r: Reusability[Props]): Callback = {
      val maybeRefresh = Callback.unless(prevProps.exists(_ ~=~ p))(refresh(p))
      val maybeEnrich  = Callback.when(s.isDefined)(enrich(p))
      maybeRefresh >> maybeEnrich
    }

    def enrich(p: Props): Callback =
      Callback.empty

    def render(s: State): VdomElement =
      s match {
        case Some(svg) => <.div(Style.svgGraph, ^.dangerouslySetInnerHtml := svg.content)
        case None      => <.div
      }

    protected def graphNodeIterator(root: dom.Element): Iterator[dom.svg.G] =
      root.querySelectorAll("g.node").iterator.map(_.domCast[dom.svg.G])
  }

  def graphConfig[P <: HasWebWorker : Reusability, C <: Children, B <: GraphBackend[P]]: ScalaComponent.Config[P, C, State, B, UpdateSnapshot.None, UpdateSnapshot.Some[Unit]] =
    _.configure(Reusability.shouldComponentUpdate)
      .componentDidMount($ => $.backend.onRender(None, $.props, $.state))
      .componentDidUpdate(i => i.backend.onRender(Some(i.prevProps), i.currentProps, i.currentState))
}
