package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.VdomElement
import org.scalajs.dom.document
import org.scalajs.dom.raw.SVGSVGElement
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.data.savedview.ImpGraphConfig.{Colours, LabelFormat}
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.GraphComponent._
import shipreq.webapp.client.ww.api.WebWorkerCmd

object ImplicationGraph {

  sealed trait Props extends HasWebWorker {
    val project    : Project
    val plainText  : PlainText.ForProject.AnyCtx
    val reqDetailRC: RouterCtl[ExternalPubid]
    val webWorker  : WebWorkerClient.Instance

    def isEmpty: Boolean

    @inline final def render: VdomElement = Component(this)
  }

  object Props {

    final case class FocusReq(ord        : WebWorkerCmd.Ord,
                              focus      : ReqId,
                              filterDead : FilterDead,
                              project    : Project,
                              plainText  : PlainText.ForProject.AnyCtx,
                              reqDetailRC: RouterCtl[ExternalPubid],
                              webWorker  : WebWorkerClient.Instance) extends Props {

      override def isEmpty: Boolean =
        false
    }

    final case class All(ord         : WebWorkerCmd.Ord,
                         reqWhitelist: Option[Set[ReqId]],
                         filterDead  : FilterDead,
                         config      : ImpGraphConfig,
                         project     : Project,
                         plainText   : PlainText.ForProject.AnyCtx,
                         reqDetailRC : RouterCtl[ExternalPubid],
                         webWorker   : WebWorkerClient.Instance) extends Props {

      override val isEmpty: Boolean =
        reqWhitelist match {
          case Some(w) => w.isEmpty
          case None    => project.content.reqs.isEmpty
        }
    }
  }


  implicit val reusabilityProps: Reusability[Props] = {
    @nowarn("cat=unused") implicit def a: Reusability[Implications.BiDir] = Reusability.byRef
    Reusability.derive
  }

  final class Backend($: BackendScope[Props, State]) extends GraphBackend($) {

    override protected def displayMode(p: Props): DisplayMode =
      p match {
        case _: Props.FocusReq => DisplayMode.FitToWidth
        case _: Props.All      => DisplayMode.PanZoom
      }

    override def cmd(props: Props) =
      props match {
        case p: Props.FocusReq =>
          WebWorkerCmd.GraphReqImplications(
            ord        = p.ord,
            focus      = p.focus,
            filterDead = p.filterDead,
          )

        case p: Props.All =>
          WebWorkerCmd.GraphAllImplications(
            ord        = p.ord,
            filterDead = p.filterDead,
            scope      = p.reqWhitelist,
            config     = p.config,
          )
      }

    override def enrich(p: Props, root: SVGSVGElement): Callback =
      Callback {

        val hoverTextFor: Req => String =
          p match {
            case x: Props.FocusReq => HoverText(ImpGraphConfig.default, x.filterDead, x.project, x.plainText)
            case x: Props.All      => HoverText(x.config, x.filterDead, x.project, x.plainText)
          }

        for (node <- graphNodeIterator(root)) {
          val pubid = node.id
          for {
            ep  <- ExternalPubid.parse(pubid)
            req <- ep.lookup(p.project.config.reqTypes, p.project.content.reqs)
          }
            p match {
              case x: Props.FocusReq if x.focus ==* req.id =>
                // Enrich focus node
                node.style.cursor = "default"

              case _ =>
                // Set title
                val h = hoverTextFor(req)
                if (h.nonEmpty) {
                  val titleEl = document.createElementNS(SvgNS, "title")
                  titleEl.textContent = h
                  node.appendChild(titleEl)
                }

                // Make link
                val parent = node.parentNode
                if (parent.nodeName.toUpperCase != "A") {
                  val a = document.createElementNS(SvgNS, "a")
                  a.setAttributeNS(XlinkNS, "xlink:href", p.reqDetailRC.urlFor(ep).value)
                  parent.replaceChild(a, node)
                  a.appendChild(node)
                }
            }
        }
      }
  }

  private[widgets] object HoverText {
    val none = (_: Any) => ""

    def apply(config    : ImpGraphConfig,
              filterDead: FilterDead,
              project   : Project,
              plainText : PlainText.ForProject.AnyCtx): Req => String = {

      val title: Req => String =
        config.labelFormat match {
          case LabelFormat.Pubid         => plainText.reqTitleWithoutMarkup
          case LabelFormat.PubidAndTitle => none
        }

      val tags: Req => String =
        config.colours match {

          case Colours.ByReqType =>
            none

          case Colours.ByTag(tagGroupId) =>
            val reqTags = project.virtualTags.underTagGroup(tagGroupId, filterDead)
            req => plainText.tagListWithHashtags(reqTags(req.id))
        }

      val all = (title :: tags :: Nil).filter(_ ne none)
      all match {
        case Nil      => none
        case h :: Nil => h
        case _ =>
          req => {
            var t = ""
            for (a <- all) {
              val h = a(req)
              if (h.nonEmpty) {
                if (t.nonEmpty)
                  t += "\n\n"
                t += h
              }
            }
            t
          }
      }
    }
  }

  val Component = ScalaComponent.builder[Props]
    .initialState(State.init)
    .renderBackend[Backend]
    .configure(graphConfig)
    .build
}
