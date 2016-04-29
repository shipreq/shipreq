package shipreq.webapp.client.widgets

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.webapp.client.app.WebWorkerClient
import shipreq.webapp.client.lib.DataReusability._
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

  type State = Option[SVG]

  abstract class GraphBackend[Props <: HasWebWorker]($: BackendScope[Props, State]) {

    def cmd(p: Props): Cmd[SVG]

    def refresh(p: Props): Callback =
      p.webWorker.postCB(cmd(p))(svg => $.setState(Some(svg)))

    def render(s: State): ReactElement =
      s match {
        case Some(svg) => <.div(^.dangerouslySetInnerHtml(svg.content))
        case None      => <.div
      }
  }

  @inline implicit class ReactCompBExt[P](private val self: ReactComponentB.P[P]) extends AnyVal {
    def graphState = self.initialState[State](None)
  }

  def graphConfig[P <: HasWebWorker : Reusability, B <: GraphBackend[P], N <: TopNode] =
    (_: ReactComponentB[P, State, B, N])
      .configure(Reusability.shouldComponentUpdate)
      .componentWillMount($ => $.backend.refresh($.props))
      .componentWillReceiveProps(i => Callback.when(i.currentProps ~/~ i.nextProps)(i.$.backend.refresh(i.nextProps)))
}
