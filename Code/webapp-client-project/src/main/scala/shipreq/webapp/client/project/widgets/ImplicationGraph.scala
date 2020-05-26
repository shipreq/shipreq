package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.VdomElement
import org.scalajs.dom.document
import scala.annotation.nowarn
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.GraphComponent._
import shipreq.webapp.client.ww.api.WebWorkerCmd

object ImplicationGraph {

  sealed trait Props extends HasWebWorker {
    val imps       : Implications.BiDir
    val reqs       : Requirements
    val reqTypes   : ReqTypes
    val plainText  : PlainText.ForProject.AnyCtx
    val reqDetailRC: RouterCtl[ExternalPubid]
    val webWorker  : WebWorkerClient

    def isEmpty: Boolean

    @inline final def render: VdomElement = Component(this)
  }

  object Props {

    final case class FocusReq(focus      : ReqId,
                              filterDead : FilterDead,
                              imps       : Implications.BiDir,
                              reqs       : Requirements,
                              reqTypes   : ReqTypes,
                              plainText  : PlainText.ForProject.AnyCtx,
                              reqDetailRC: RouterCtl[ExternalPubid],
                              webWorker  : WebWorkerClient) extends Props {

      override def isEmpty: Boolean =
        false
    }

    final case class All(reqWhitelist: Option[Set[ReqId]],
                         config      : ImpGraphConfig,
                         imps        : Implications.BiDir,
                         reqs        : Requirements,
                         reqTypes    : ReqTypes,
                         plainText   : PlainText.ForProject.AnyCtx,
                         reqDetailRC : RouterCtl[ExternalPubid],
                         webWorker   : WebWorkerClient) extends Props {

      override val isEmpty: Boolean =
        reqWhitelist match {
          case Some(w) => w.isEmpty
          case None    => reqs.isEmpty
        }
    }
  }


  implicit val reusabilityProps: Reusability[Props] = {
    @nowarn("cat=unused") implicit def a: Reusability[Implications.BiDir] = Reusability.byRef
    Reusability.derive
  }

  final class Backend($: BackendScope[Props, State]) extends GraphBackend($) {
    override def cmd(props: Props) =
      props match {
        case p: Props.FocusReq =>
          WebWorkerCmd.GraphReqImplications(p.focus, p.filterDead, p.imps, p.reqs, p.reqTypes)

        case p: Props.All =>
          WebWorkerCmd.GraphAllImplications(p.imps, p.reqs, p.reqTypes, p.reqWhitelist, p.config)
      }

    override def enrich(p: Props): Callback =
      $.getDOMNode.map(_.toElement.foreach { root =>
        for (node <- graphNodeIterator(root)) {
          val pubid = node.querySelector("text").textContent
          for {
            ep  <- ExternalPubid.parse(pubid)
            req <- ep.lookup(p.reqTypes, p.reqs)
          }
            p match {
              case x: Props.FocusReq if x.focus ==* req.id =>
                // Enrich focus node
                node.style.cursor = "default"

              case _ =>
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
