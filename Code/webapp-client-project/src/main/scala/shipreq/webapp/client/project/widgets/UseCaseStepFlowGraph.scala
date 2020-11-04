package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import org.scalajs.dom.raw.SVGSVGElement
import shipreq.webapp.base.data._
import shipreq.webapp.member.text.ProjectText
import shipreq.webapp.base.util.DomUtil._
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.GraphComponent._
import shipreq.webapp.client.ww.api.WebWorkerCmd

object UseCaseStepFlowGraph {

  final case class Props(ord      : WebWorkerCmd.Ord,
                         id       : UseCaseId,
                         ctx      : ProjectText.Context,
                         webWorker: WebWorkerClient.Instance) extends HasWebWorker {
    @inline def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  final class Backend($: BackendScope[Props, State]) extends GraphBackend($) {

    override protected def displayMode(p: Props): DisplayMode =
      DisplayMode.FitToWidth

    override def cmd(p: Props) =
      WebWorkerCmd.GraphUseCaseFlow(p.ord, p.id, p.ctx)

    override def enrich(p: Props, root: SVGSVGElement): Callback =
      Callback {
        for (node <- graphNodeIterator(root)) {
          val hasTitle = node.children.headOption.exists(_.hasAttribute("xlink:title"))
          node.style.cursor = if (hasTitle) "help" else "default"
        }
      }
  }

  val Component = ScalaComponent.builder[Props]
    .initialState(State.init)
    .renderBackend[Backend]
    .configure(graphConfig)
    .build
}

