package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import org.scalajs.dom.document
import scala.annotation.nowarn
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.GraphComponent._
import shipreq.webapp.client.ww.api.WebWorkerCmd

object ImplicationGraph {

  final case class Props(focus      : Option[ReqId],
                         filterDead : FilterDead,
                         imps       : Implications.BiDir,
                         reqs       : Requirements,
                         reqTypes   : ReqTypes,
                         plainText  : PlainText.ForProject.AnyCtx,
                         reqDetailRC: RouterCtl[ExternalPubid],
                         webWorker  : WebWorkerClient) extends HasWebWorker {
    @inline def render = Component(this)

    def isEmpty: Boolean =
      focus.isEmpty && reqs.reqIterator().map(_.live(reqTypes)).filter(filterDead.filter).isEmpty
  }

  implicit val reusabilityProps: Reusability[Props] = {
    @nowarn("cat=unused") implicit def a: Reusability[Implications.BiDir] = Reusability.byRef
    Reusability.derive
  }

  final class Backend($: BackendScope[Props, State]) extends GraphBackend($) {
    override def cmd(p: Props) =
      p.focus match {
        case Some(f) => WebWorkerCmd.GraphReqImplications(f, p.filterDead, p.imps, p.reqs, p.reqTypes)
        case None    => WebWorkerCmd.GraphAllImplications(p.filterDead, p.imps, p.reqs, p.reqTypes)
      }

    override def enrich(p: Props): Callback =
      $.getDOMNode.map(_.toElement.foreach { root =>
        for (node <- graphNodeIterator(root)) {
          val pubid = node.querySelector("text").textContent
          for {
            ep  <- ExternalPubid.parse(pubid)
            req <- ep.lookup(p.reqTypes, p.reqs)
          }
            if (p.focus.exists(_ ==* req.id)) {
              // Enrich focus node
              node.style.cursor = "default"

            } else {
              // Set title
              val titleEl = document.createElementNS(SvgNS, "title")
              titleEl.textContent = p.plainText.reqTitle(req)
              node.appendChild(titleEl)

              // Make link
              node.onclick = p.reqDetailRC.set(ep).toJsFn1
              node.style.cursor = "pointer"
            }
        }
      })
  }

  val Component = ScalaComponent.builder[Props]
    .initialState(initialState)
    .renderBackend[Backend]
    .configure(graphConfig)
    .build
}
