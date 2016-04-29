package shipreq.webapp.client.widgets.high

import japgolly.scalajs.react._
import vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.WebWorkerClient
import shipreq.webapp.client.lib.DataReusability._
import shipreq.webapp.client.ww.api._

object UseCaseStepFlowGraph {

  final case class Props(id       : UseCaseId,
                         useCases : UseCases,
                         webWorker: WebWorkerClient) {
    @inline def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] = {
    implicit def useCases: Reusability[UseCases] = Reusability.byRef
    Reusability.caseClass
  }

  implicit def reusabilitySVG: Reusability[SVG] =
    Reusability.caseClass

  type State = Option[SVG]

  final class Backend($: BackendScope[Props, State]) {

    def refresh(p: Props): Callback =
      p.webWorker.postCB(Cmd.GraphUseCaseStepFlow(p.id, p.useCases))(svg =>
        $.setState(Some(svg)))

    def render(p: Props, s: State): ReactElement =
      s match {
        case Some(svg) => <.div(^.dangerouslySetInnerHtml(svg.content))
        case None      => <.div
      }
  }

  val Component = ReactComponentB[Props]("UseCaseStepFlowGraph")
    .initialState[State](None)
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .componentWillMount($ => $.backend.refresh($.props))
    .componentWillReceiveProps(i => Callback.when(i.currentProps ~/~ i.nextProps)(i.$.backend.refresh(i.nextProps)))
    .build
}
