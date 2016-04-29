package shipreq.webapp.client.widgets.high

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.WebWorkerClient
import shipreq.webapp.client.lib.DataReusability._
import shipreq.webapp.client.widgets.GraphComponent._
import shipreq.webapp.client.ww.api.Cmd

object UseCaseStepFlowGraph {

  final case class Props(id       : UseCaseId,
                         useCases : UseCases,
                         webWorker: WebWorkerClient) extends HasWebWorker {
    @inline def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] = {
    implicit def useCases: Reusability[UseCases] = Reusability.byRef
    Reusability.caseClass
  }

  final class Backend($: BackendScope[Props, State]) extends GraphBackend($) {
    override def cmd(p: Props) =
      Cmd.GraphUseCaseStepFlow(p.id, p.useCases)
  }

  val Component = ReactComponentB[Props]("UseCaseStepFlowGraph")
    .graphState
    .renderBackend[Backend]
    .configure(graphConfig)
    .build
}

