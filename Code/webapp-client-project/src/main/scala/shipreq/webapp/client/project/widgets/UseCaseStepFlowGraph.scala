package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import shipreq.webapp.base.data._
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.base.text.ProjectText
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.GraphComponent._
import shipreq.webapp.client.ww.api.Cmd

object UseCaseStepFlowGraph {

  final case class Props(id       : UseCaseId,
                         project  : Project,
                         ctx      : ProjectText.Context,
                         webWorker: WebWorkerClient) extends HasWebWorker {
    @inline def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  final class Backend($: BackendScope[Props, State]) extends GraphBackend($) {
    override def cmd(p: Props) =
      Cmd.GraphUseCaseStepFlow(p.id, p.project, p.ctx)

    override def enrich(p: Props): Callback =
      $.getDOMNode.map(_.toElement.foreach { root =>
        for (node <- graphNodeIterator(root)) {
          val hasTitle = node.children.headOption.exists(_.hasAttribute("xlink:title"))
          node.style.cursor = if (hasTitle) "help" else "default"
        }
      })
  }

  val Component = ScalaComponent.builder[Props]
    .initialState(initialState)
    .renderBackend[Backend]
    .configure(graphConfig)
    .build
}

