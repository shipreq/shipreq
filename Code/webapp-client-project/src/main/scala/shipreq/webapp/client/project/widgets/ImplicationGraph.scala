package shipreq.webapp.client.project.widgets

import japgolly.microlibs.nonempty.NonEmpty
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.VdomElement
import org.scalajs.dom.document
import org.scalajs.dom.raw.SVGSVGElement
import scala.scalajs.js
import scala.scalajs.js.timers
import scala.util.Try
import shipreq.base.util.JsExt._
import shipreq.base.util.{Backwards, ErrorMsg, Forwards, MutableRef, SetDiff}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.data.savedview.ImpGraphConfig.LabelFormat
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.base.lib.{DomUtil, LoggerJs}
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.UpdateContentCmd
import shipreq.webapp.base.protocol.websocket.UpdateContentCmd.PatchImplications
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.util.Must._
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

  var runningInUnitTest = false

  final class Backend($: BackendScope[Props, State]) extends GraphBackend($) {

    private var edgeEditor = Option.empty[EdgeEditor]

    override protected def displayMode(p: Props): DisplayMode =
      if (runningInUnitTest)
        DisplayMode.FitToWidth
      else
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
          edgeEditor = Some(new EdgeEditor($.props.map(_.project)))

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

    final case class Args(ssp   : ServerSideProcInvoker[UpdateContentCmd, ErrorMsg, Any],
                          asyncW: AsyncFeature.Write.D1[UpdateContentCmd, ErrorMsg],
                          asyncR: Reusable[CallbackTo[AsyncFeature.Read.D1[UpdateContentCmd, ErrorMsg]]])

    implicit def reusabilityArgs: Reusability[Args] =
      Reusability.derive

    import org.scalajs.dom._
    import shipreq.webapp.client.project.app.Style.widgets.{impGraphEdgeEditor => *}

    private object StaticInternals {

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

      @inline def lineAngleInDegrees(x: Double, y: Double): Double =
        Math.atan2(y, x) * 180 / Math.PI

      private final val arrowSize = 6
      final val arrowPath = "l-" + arrowSize + ",-" + arrowSize + " l" + arrowSize + "," + arrowSize + " l-" + arrowSize + "," + arrowSize

      private val edgeIdRegex = "^(.+?)--(.+)$".r

      def edgeIds(edge: svg.Element): (String, String) = {
        edge.id match {
          case edgeIdRegex(from, to) => (from, to)
        }
      }

      sealed trait NewEdgeResult
      object NewEdgeResult {
        case object Invalid                             extends NewEdgeResult
        case object NoOp                                extends NewEdgeResult
        final case class Change(cmd: PatchImplications) extends NewEdgeResult
      }

      val logger      = LoggerJs.off // LoggerJs.devOnly.prefixedWith("[EE] ")
      val eventLogger = LoggerJs.off
    }

    // Exposed for tests
    def getDragArrow(root: svg.Element) =
      Option(root.querySelector(s".${*.clsDragArrow} > path").asInstanceOf[svg.Path])
  }

  private final class EdgeEditor(projectCB: CallbackTo[Project]) {

    import shipreq.webapp.client.project.app.Style.widgets.{impGraphEdgeEditor => *}
    import org.scalajs.dom._
    import EdgeEditor.Args
    import EdgeEditor.StaticInternals._

    private var args          = Option.empty[Args]
    private var root          = null: SVGSVGElement
    private var point         = null: svg.Point
    private var dragArrow     = null: svg.Element
    private var dragDelay     = Option.empty[timers.SetTimeoutHandle]
    private val dragSrc       = MutableRef.option[svg.Element]
    private val dragTgt       = MutableRef.option[svg.Element]
    private var lastMouseUpMs = 0L

    def enrich(root: SVGSVGElement, args: Option[Args]): Unit = {
      logger(_.debug("Enriching: ", root))

      this.args          = args
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

      // Update global
      document.addEventListener("keypress", onKeyPress)

      // Update root
      root.classList.add(*.root.className.value)
      root.onmousemove = onRootMouseMove
      root.onmouseup   = onRootMouseUp

      // Update nodes
      for (n_ <- root.querySelectorAll("g.node")) {
        val n = n_.asSvgEl
        n.onmousedown = onNodeMouseDown
        n.onmousemove = onNodeMouseMove
        n.onclick = onNodeClick
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
        EdgeEditor.getDragArrow(root).getOrElse {
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

      // Uninstall from global
      document.removeEventListener("keypress", onKeyPress)

      // Uninstall from root
      root.classList.remove(*.root.className.value)
      root.onmousemove = null
      root.onmouseup   = null

      // Uninstall from nodes
      for (n_ <- root.querySelectorAll("g.node")) {
        val n = n_.asSvgEl
        n.onmousedown = null
        n.onmousemove = null
        n.onclick = null
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

    private def setDragDelay(newHandle: Option[timers.SetTimeoutHandle]): Unit = {
      for (old <- dragDelay)
        timers.clearTimeout(old)
      dragDelay = newHandle
    }

    private val onEdgeClick: js.Function1[MouseEvent, Unit] = ev => {
      eventLogger(_.debug("onEdgeClick: ", ev))

      setDragDelay(None)

      DomUtil.unfocus.runNow()

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

      setDragDelay(None)

      // Prevent double-clicks being interpreted by ReactSvgPanZoom
      ev.stopPropagation()
    }

    private val onNodeClick: js.Function1[MouseEvent, Unit] = ev => {
      eventLogger(_.debug("onNodeClick: ", ev))

      // Prevent mouseup being treated as a click as well
      if (!runningInUnitTest) {
        val msSinceLastMouseUp = System.currentTimeMillis() - lastMouseUpMs
        if (msSinceLastMouseUp <= 200) {
          ev.preventDefault()
          ev.stopPropagation()
        }
      }
    }

    private val onKeyPress: js.Function1[KeyboardEvent, Unit] = ev => {
      eventLogger(_.debug("onKeyPress: ", ev))
      if (root ne null) {
        ev.key.toUpperCase match {

          case "DELETE" =>
            for {
              edge <- getSelectedEdge()
              args <- this.args
            } yield {
              ev.stopPropagation()
              val (from, to) = edgeIds(edge)
              deleteEdge(args, from, to)
            }

          case _ =>
        }
      }
    }

    private val onNodeMouseDown: js.Function1[MouseEvent, Unit] = ev => {
      eventLogger(_.debug("onNodeMouseDown: ", ev))

      DomUtil.unfocus.runNow()

      if (root ne null) {

        // DragSrc might still be specified because an async commit is in progress
        if (dragSrc.value.isEmpty) {

          // TODO would be a good idea to have a delay before we interpret mouse down as an edge creation command.
          // Normally clicking on a node shouldn't trigger this.

          val ds = ev.currentTarget.asSvgEl

          def action() = {
            setDragDelay(None)
            setDragSrc(Some(ds))
            dragArrow.setAttribute("d", "")
            root.classList.add(*.clsDragging)
          }

          if (runningInUnitTest)
            action()
          else
            setDragDelay(Some(timers.setTimeout(200)(action())))
        }
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
              val p      = projectCB.runNow()
              val result = newEdgeResult(p, ds.id, dt.id)

              result match {
                case _: NewEdgeResult.Change =>
                  root.classList.remove(*.clsDragInvalid)
                  root.classList.remove(*.clsDragNoOp)

                case NewEdgeResult.Invalid =>
                  root.classList.add(*.clsDragInvalid)
                  root.classList.remove(*.clsDragNoOp)

                case NewEdgeResult.NoOp =>
                  root.classList.remove(*.clsDragInvalid)
                  root.classList.add(*.clsDragNoOp)
              }

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

          (dragTgt.value, args) match {
            case (Some(dt), Some(a)) => createEdge(a, ds.id, dt.id)
            case _                   => reset()
          }

          // Prevent node click navigating to ReqDetail
          ev.preventDefault()
          ev.stopPropagation()
          lastMouseUpMs = System.currentTimeMillis()
        }
      }
    }

    private def needReq(p: Project, pubidStr: String): Req = {
      def err = ErrorMsg("Bad id: " + pubidStr.quote)
      val ep = ExternalPubid.parse(pubidStr).mustExistElse(err)
      ep.lookup(p).mustExistElse(err)
    }

    private def isEitherDead(p: Project, a: Req, b: Req): Boolean = {
      assert(a ne b)
      val reqTypes = p.config.reqTypes
      (a.live(reqTypes) is Dead) || (b.live(reqTypes) is Dead)
    }

    private def newEdgeValue(p: Project, reqSrc: Req, idTgt: String): Any \/ SetDiff[ReqId] = {
      val reqTgt = needReq(p, idTgt)
      if (isEitherDead(p, reqSrc, reqTgt))
        -\/(())
      else {
        val initialValues = p.content.implications.forwards(reqSrc.id)
        val auditor = DataValidators.implicationAuditor(p, Some(reqSrc.id), initialValues, Forwards)
        auditor(initialValues + reqTgt.id)
      }
    }

    private def newEdgeResult(p: Project, idStrFrom: String, idStrTo: String): NewEdgeResult = {
      val reqSrc = needReq(p, idStrFrom)
      val result = newEdgeValue(p, reqSrc, idStrTo)

      result match {
        case \/-(maybePatch) =>
          NonEmpty(maybePatch) match {

            case Some(patch) =>
              getSelectedEdge().map(edgeIds) match {

                case Some((idStrSelFrom, idStrSelTo)) if idStrSelFrom == idStrFrom =>
                  // Replace selected edge tgt
                  val reqSelTo = needReq(p, idStrSelTo)
                  if (reqSelTo.live(p.config.reqTypes) is Dead)
                    NewEdgeResult.Invalid
                  else {
                    val patch2 = NonEmpty.force(SetDiff(removed = Set1(reqSelTo.id), added = patch.added))
                    val cmd    = PatchImplications(reqSrc.id, Forwards, patch2)
                    NewEdgeResult.Change(cmd)
                  }

                case Some((idStrSelFrom, idStrSelTo)) if idStrSelTo == idStrTo =>
                  // Replace selected edge src
                  val reqSelFrom = needReq(p, idStrSelFrom)
                  if (reqSelFrom.live(p.config.reqTypes) is Dead)
                    NewEdgeResult.Invalid
                  else {
                    val reqTgtId = patch.added.head
                    val patch2   = NonEmpty.force(SetDiff(removed = Set1(reqSelFrom.id), added = Set1(reqSrc.id)))
                    val cmd      = PatchImplications(reqTgtId, Backwards, patch2)
                    NewEdgeResult.Change(cmd)
                  }

                case _ =>
                  // Add new edge
                  val cmd = PatchImplications(reqSrc.id, Forwards, patch)
                  NewEdgeResult.Change(cmd)
              }

            case None =>
              NewEdgeResult.NoOp
          }

        case -\/(_) =>
          NewEdgeResult.Invalid
      }
    }

    private def createEdge(args: Args, idStrFrom: String, idStrTo: String): Unit = {
      val p      = projectCB.runNow()
      val result = newEdgeResult(p, idStrFrom, idStrTo)

      logger(_.info(s"New edge: $idStrFrom -> $idStrTo = $result"))

      result match {

        case NewEdgeResult.Change(cmd) =>
          // Change the graph
          val commit = args.ssp(cmd)
          val reset  = AsyncCallback.delay(this.reset())
          val proc   = args.asyncW(cmd).onFailureShowAndForget(commit <* reset)
          proc.runNow()

        case NewEdgeResult.NoOp | NewEdgeResult.Invalid =>
          reset()
      }
    }

    private def deleteEdge(args: Args, idStrFrom: String, idStrTo: String): Unit = {
      logger(_.info(s"Delete edge: $idStrFrom -> $idStrTo"))

      val p      = projectCB.runNow()
      val reqSrc = needReq(p, idStrFrom)
      val reqTgt = needReq(p, idStrTo)

      if (!isEitherDead(p, reqSrc, reqTgt)) {
        val patch  = NonEmpty.force(SetDiff(removed = Set1(reqTgt.id), added = Set.empty))
        val cmd    = PatchImplications(reqSrc.id, Forwards, patch)
        val commit = args.ssp(cmd)
        val proc   = args.asyncW(cmd).onFailureShowAndForget(commit)
        proc.runNow()
      }
    }

    private def reset(): Unit = {
      root.classList.remove(*.clsDragging)
      root.classList.remove(*.clsDragInvalid)
      root.classList.remove(*.clsDragNoOp)
      setDragDelay(None)
      setDragSrc(None)
      setDragTgt(None)
      dragArrow.setAttribute("d", "")
      for (e <- getSelectedEdge())
        e.classList.remove(*.clsSelectedEdge)
    }
  }
}
