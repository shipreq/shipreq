package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.VdomElement
import org.scalajs.dom.document
import org.scalajs.dom.raw.SVGSVGElement
import scala.scalajs.js
import scala.util.Try
import shipreq.base.util.JsExt._
import shipreq.base.util.MutableRef
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.data.savedview.ImpGraphConfig.LabelFormat
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.GraphComponent._
import shipreq.webapp.client.ww.api.WebWorkerCmd

object ImplicationGraph {

  sealed trait Props extends HasWebWorker {
    val project        : Project
    val plainText      : PlainText.ForProject.AnyCtx
    val reqDetailRC    : RouterCtl[ExternalPubid]
    val webWorker      : WebWorkerClient.Instance
    def edgeEditorArgs: Option[EdgeEditor.Args]

    def isEmpty: Boolean

    @inline final def render: VdomElement = Component(this)
  }

  object Props {

    final case class FocusReq(ord        : WebWorkerCmd.Ord,
                              focus      : ReqId,
                              filterDead : FilterDead,
                              project    : Project,
                              plainText  : PlainText.ForProject.AnyCtx,
                              colours    : Option[ImpGraphConfig.Colours],
                              reqDetailRC: RouterCtl[ExternalPubid],
                              webWorker  : WebWorkerClient.Instance) extends Props {
      override def isEmpty = false
      override def edgeEditorArgs = None
    }

    final case class All(ord           : WebWorkerCmd.Ord,
                         reqWhitelist  : Option[Set[ReqId]],
                         filterDead    : FilterDead,
                         config        : ImpGraphConfig,
                         project       : Project,
                         plainText     : PlainText.ForProject.AnyCtx,
                         reqDetailRC   : RouterCtl[ExternalPubid],
                         webWorker     : WebWorkerClient.Instance,
                         edgeEditorArgs: Option[EdgeEditor.Args],
                        ) extends Props {

      override val isEmpty: Boolean =
        reqWhitelist match {
          case Some(w) => w.isEmpty
          case None    => project.content.reqs.isEmpty
        }
    }

    @nowarn("cat=unused")
    private implicit def reusabilityImplications: Reusability[Implications] =
      Reusability.byRef

    implicit val reusabilityFocusReq: Reusability[FocusReq] =
      Reusability.derive

    implicit val reusabilityProps: Reusability[Props] =
      Reusability.derive
  }

  final class Backend($: BackendScope[Props, State]) extends GraphBackend($) {

    private var edgeEditor = Option.empty[EdgeEditor]

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
            colours    = p.colours,
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

        if (edgeEditor.isEmpty && p.edgeEditorArgs.isDefined)
          edgeEditor = Some(new EdgeEditor)

        for (ee <- edgeEditor)
          ee.enrich(root, p.edgeEditorArgs)
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
        req => {
          val tags = project.virtualTags(req.id, filterDead).ordered(TagFieldId.All)
          plainText.tagListWithHashtags(tags)
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

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  // Allows users to modify implications directly from the graph.
  //
  // A simpler, isolated PoC of this is available in :/Experimentation/req_graph-edit_edges
  //
  // Note: The EdgeEditor is managed outside of React. Even the state is just a bunch of vars inside the EdgeEditor
  // class.
  object EdgeEditor {

    final case class Args()

    implicit def reusabilityArgs: Reusability[Args] =
      Reusability.derive

    private object StaticInternals {
      import org.scalajs.dom._

      implicit class EEExt_EventTarget(private val self: EventTarget) extends AnyVal {
        def asSvgEl = self.asInstanceOf[svg.Element]
      }
      implicit class EEExt_Node(private val self: Node) extends AnyVal {
        def asSvgEl = self.asInstanceOf[svg.Element]
      }
      implicit class EEExt_SvgElement(private val self: svg.Element) extends AnyVal {
        def parentElement = self.asInstanceOf[js.Dynamic].parentElement.asInstanceOf[svg.Element]
        def getBBox()     = self.asInstanceOf[js.Dynamic].getBBox().asInstanceOf[svg.Rect]
      }

      def lineAngleInDegrees(x: Double, y: Double): Double =
        Math.atan2(y, x) * 180 / Math.PI

      private final val arrowSize = 6
      final val arrowPath = "l-" + arrowSize + ",-" + arrowSize + " l" + arrowSize + "," + arrowSize + " l-" + arrowSize + "," + arrowSize

      val logger      = LoggerJs.off // LoggerJs.devOnly.prefixedWith("[EE] ")
      val eventLogger = LoggerJs.off
    }
  }

  private final class EdgeEditor {
    import shipreq.webapp.client.project.app.Style.widgets.{impGraphEdgeEditor => *}
    import org.scalajs.dom._
    import EdgeEditor.Args
    import EdgeEditor.StaticInternals._

    private var root        = null: SVGSVGElement
    private var point       = null: svg.Point
    private var dragArrow   = null: svg.Element
    private val dragSrc     = MutableRef.option[svg.Element]
    private val dragTgt     = MutableRef.option[svg.Element]

    def enrich(root: SVGSVGElement, args: Option[Args]): Unit = {
      logger(_.debug("Enriching: ", root))

      this.root          = root
      this.dragSrc.value = None
      this.dragTgt.value = None

      args match {
        case Some(_) => enable()
        case None    => disable()
      }

      logger(_.debug("dragArrow: ", dragArrow))
    }

    private def enable(): Unit = {
      logger(_.debug("Enabling..."))
      point = root.createSVGPoint()

      // Update root
      root.classList.add(*.root.className.value)
      root.onmousemove = onRootMouseMove
      root.onmouseup   = onRootMouseUp

      // Update nodes
      for (n_ <- root.querySelectorAll("g.node")) {
        val n = n_.asSvgEl
        n.onmousedown = onNodeMouseDown
        n.onmousemove = onNodeMouseMove
      }

      // Update edges
      for (e_ <- root.querySelectorAll("g.edge")) {
        val e = e_.asSvgEl

        val alreadyExists = Option(e.lastElementChild).exists(_.classList.contains(*.clsEdge2))

        if (!alreadyExists) {

          // Duplicate the edge, make it invisible and very thick.
          // This is so users can click on an edge without requiring pixel-precision.
          val e2 = e.cloneNode(true).asSvgEl
          e2.classList.remove("edge")
          e2.classList.add(*.clsEdge2)
          e2.setAttribute("stroke-width", "16")
          e.appendChild(e2)

          e2.onclick = onEdgeClick
          e2.ondblclick = onEdgeDblClick
        }
      }

      // Find or create drag arrow
      dragArrow =
        Option(root.querySelector(s".${*.clsDragArrow} > path").asSvgEl).getOrElse {
          val path = document.createElementNS(SvgNS, "path").asSvgEl
          path.onmousemove = onDragArrowMouseMove

          val g = document.createElementNS(SvgNS, "g")
          g.classList.add(*.clsDragArrow)
          g.appendChild(path)

          val scope = root.lastElementChild.lastElementChild.lastElementChild
          logger(_.debug("Scope: ", scope))
          scope.appendChild(g)

          path
        }
    }

    private def disable(): Unit = {
      logger(_.debug("Disabling..."))
      point = null

      // Uninstall from root
      root.classList.remove(*.root.className.value)
      root.onmousemove = null
      root.onmouseup   = null

      // Uninstall from nodes
      for (n_ <- root.querySelectorAll("g.node")) {
        val n = n_.asSvgEl
        n.onmousedown = null
        n.onmousemove = null
      }

      // Uninstall from edges
      for (e <- root.querySelectorAll("." + *.clsEdge2)) {
        e.asSvgEl.parentNode.removeChild(e)
      }

      // Uninstall drag arrow
      if (dragArrow ne null) {
        Try(dragArrow.parentNode.removeChild(dragArrow))
        dragArrow = null
      }
    }

    private def getSelectedEdge(): Option[svg.Element] =
      Option(root.querySelector("." + *.clsSelectedEdge).asSvgEl)

    // Taken from https://stackoverflow.com/a/50396546/1846272
    private def getCTM(): svg.Matrix = {
      val height      = root.height.baseVal.value
      val width       = root.width.baseVal.value
      val viewBoxRect = root.viewBox.baseVal
      val vHeight     = viewBoxRect.height
      val vWidth      = viewBoxRect.width
      if (vWidth.falsy || vHeight.falsy)
        root.getCTM()
      else {
        val sH = height / vHeight
        val sW = width / vWidth
        val matrix = root.createSVGMatrix()
        matrix.a = sW
        matrix.d = sH
        val realCTM = root.getCTM().multiply(matrix.inverse())
        realCTM.e = realCTM.e / sW + viewBoxRect.x
        realCTM.f = realCTM.f / sH + viewBoxRect.y
        realCTM
      }
    }

    private def getNodeMidpoint(node: svg.Element): svg.Point = {
      val ctm = getCTM()
      val bb = node.getBBox()
      point.x = bb.x + bb.width / 2
      point.y = bb.y + bb.height / 2
      point.matrixTransform(ctm)
    }

    private def setDrag(ref     : MutableRef[Option[svg.Element]],
                        cls     : String,
                        newValue: Option[svg.Element]): Boolean = {
      val changed = ref.value != newValue
      if (changed) {
        ref.value.foreach(_.classList.remove(cls))
        ref.value = newValue
        ref.value.foreach(_.classList.add(cls))
      }
      changed
    }

    private val setDragSrc = setDrag(dragSrc, *.clsDragSrc, _)
    private val setDragTgt = setDrag(dragTgt, *.clsDragTgt, _)

    private def clearDragTarget(): Unit = {
      val changed = setDragTgt(None)
      if (changed)
        dragArrow.setAttribute("d", "")
    }

    private val onEdgeClick: js.Function1[MouseEvent, Unit] = ev => {
      eventLogger(_.debug("onEdgeClick: ", ev))
      val edge = ev.currentTarget.asSvgEl.parentElement
      if (edge.classList.contains(*.clsSelectedEdge)) {
        // De-select
        edge.classList.remove(*.clsSelectedEdge)
      } else {
        // Select
        for (prev <- getSelectedEdge())
          prev.classList.remove(*.clsSelectedEdge)
        edge.classList.add(*.clsSelectedEdge)
      }

      // Prevent clicks being interpreted by ReactSvgPanZoom
      ev.stopPropagation()
    }

    private val onEdgeDblClick: js.Function1[MouseEvent, Unit] = ev => {
      eventLogger(_.debug("onEdgeDblClick: ", ev))

      // Prevent double-clicks being interpreted by ReactSvgPanZoom
      ev.stopPropagation()
    }

    private val onNodeMouseDown: js.Function1[MouseEvent, Unit] = ev => {
      eventLogger(_.debug("onNodeMouseDown: ", ev))
      if (root ne null) {

        // TODO would be a good idea to have a delay before we interpret mouse down as an edge creation command.
        // Normally clicking on a node shouldn't trigger this.

        setDragSrc(Some(ev.currentTarget.asSvgEl))
        dragArrow.setAttribute("d", "")
        root.classList.add(*.clsDragging)
      }

      // Prevent ReactSvgPanZoom intercepting the drag
      ev.stopPropagation()
    }

    private val onDragArrowMouseMove: js.Function1[MouseEvent, Unit] = ev => {
      eventLogger(_.debug("onDragArrowMouseMove: ", ev))
      // onNodeMouseMove of the node under the dragArrow wont accept this event because it only checks ev.currentTarget.
      // Calling stopPropagation() here prevents the event being picked up by onRootMouseMove instead, which will clear
      // the dragTgt.
      ev.stopPropagation()
    }

    private val onNodeMouseMove: js.Function1[MouseEvent, Unit] = ev => {
      if (root ne null) {
        for (ds <- dragSrc.value) {
          eventLogger(_.debug("onNodeMouseMove: ", ev))

          val dt = ev.currentTarget.asSvgEl
          if (dt == ds) {
            clearDragTarget()
          } else {
            val changed = setDragTgt(Some(dt))
            if (changed) {

              // Draw an arrow from source to target
              val src = getNodeMidpoint(ds)
              val tgt = getNodeMidpoint(dt)
              val dx  = tgt.x - src.x
              val dy  = tgt.y - src.y
              val len = Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2))
              val pth = s"M${src.x},${src.y} h$len $arrowPath"
              val deg = lineAngleInDegrees(x = dx, y = dy)
              val rot = s"rotate($deg,${src.x},${src.y})"
              dragArrow.setAttribute("d", pth)
              dragArrow.setAttribute("transform", rot)
            }
          }
        }

        // Don't let this propagate to onRootMouseMove
        ev.stopPropagation()
      }
    }

    private val onRootMouseMove: js.Function1[MouseEvent, Unit] = ev => {
      if (root ne null) {
        if (dragSrc.value.isDefined) {
          eventLogger(_.debug("onRootMouseMove: ", ev))

          clearDragTarget()

          // Just in case. Everywhere else needed it.
          ev.stopPropagation()
        }
      }
    }

    private val onRootMouseUp: js.Function1[MouseEvent, Unit] = ev => {
      eventLogger(_.debug("onRootMouseUp: ", ev))
      if (root ne null) {
        for (ds <- dragSrc.value) {
          root.classList.remove(*.clsDragging)
          for (dt <- dragTgt.value) {
            createEdge(ds.id, dt.id)
            setDragTgt(None)
          }
          setDragSrc(None)
          dragArrow.setAttribute("d", "")

          // Prevent node click navigating to ReqDetail
          ev.stopPropagation()
        }
      }
    }

    private def createEdge(fromId: String, toId: String): Unit = {
      console.log(s"New edge: $fromId -> $toId")
      // async handling
    }
  }
}
