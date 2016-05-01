package shipreq.webapp.client.widgets.high

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.WebWorkerClient
import shipreq.webapp.client.lib.DataReusability._
import shipreq.webapp.client.widgets.GraphComponent._
import shipreq.webapp.client.ww.api.Cmd

object ImplicationGraph {

  final case class Props(focus     : ReqId,
                         filterDead: FilterDead,
                         imps      : Implications.BiDir,
                         reqs      : Requirements,
                         reqTypes  : ReqTypes,
                         webWorker : WebWorkerClient) extends HasWebWorker {
    @inline def render = Component(this)
  }

  object Props {
    def fromProject(focus: ReqId, filterDead: FilterDead, p: Project, w: WebWorkerClient): Props =
      Props(focus, filterDead, p.implications, p.reqs, p.config.reqTypes, w)
  }

  implicit val reusabilityProps: Reusability[Props] = {
    implicit def a: Reusability[Implications.BiDir] = Reusability.byRef
    implicit def b: Reusability[Requirements      ] = Reusability.byRef
    implicit def c: Reusability[ReqTypes          ] = Reusability.byRef
    Reusability.caseClass
  }

  final class Backend($: BackendScope[Props, State]) extends GraphBackend($) {
    override def cmd(p: Props) =
      Cmd.GraphReqImplications(p.focus, p.filterDead, p.imps, p.reqs, p.reqTypes)
  }

  val Component = ReactComponentB[Props]("ImplicationGraph")
    .graphState
    .renderBackend[Backend]
    .configure(graphConfig)
    .build
}

