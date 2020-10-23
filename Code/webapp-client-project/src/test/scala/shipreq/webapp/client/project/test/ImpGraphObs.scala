package shipreq.webapp.client.project.test

import org.scalajs.dom.svg
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.Style.widgets.{impGraphEdgeEditor => *}
import shipreq.webapp.client.project.widgets.{GraphComponent, ImplicationGraph}

final class ImpGraphObs($: DomZipperJs) {
  import ImpGraphObs._

  //  println()
  //  println($.outerHTML)
  //  println()

  private val rootDom = $.domAs[svg.SVG]

  val dragState: DragState =
    if (!rootDom.classList.contains(*.clsDragging))
      DragState.None
    else {
      val dragArrow = ImplicationGraph.EdgeEditor.getDragArrow(rootDom)

      val dragArrowPath =
        dragArrow
          .flatMap(p => Option(p.getAttribute("d")))
          .getOrElse("")

      if (dragArrowPath.isEmpty)
        DragState.Invisible
      else if (rootDom.classList.contains(*.clsDragInvalid))
        DragState.Invalid
      else if (rootDom.classList.contains(*.clsDragNoOp))
        DragState.NoOp
      else
        DragState.Valid
    }

  val nodes = $.collect0n("g.node").map(new Node(_))
  val edges = $.collect0n("g.edge").map(new Edge(_))

  val nodeIds = nodes.map(_.id)
  val edgeIds = edges.map(_.id)

  def nodeById(id: String): Node = nodes.find(_.id == id).getOrThrow(s"Node [$id] not found")
  def edgeById(id: String): Edge = edges.find(_.id == id).getOrThrow(s"Edge [$id] not found")

  val selectedEdge: Option[Edge] =
    edges.filter(_.isSelected).asOption()
}

object ImpGraphObs {

  def find($: DomZipperJs): Option[ImpGraphObs] =
    GraphComponent.getGraphSvg($.dom).map { svg =>
      val z = DomZipperJs(svg)
      new ImpGraphObs(z)
    }

  final class Node($: DomZipperJs) {
    private val dom = $.dom
    val id          = dom.id
    def mouseDown() = dispatchEvent(dom, "mousedown")
    def mouseMove() = dispatchEvent(dom, "mousemove")
    def mouseUp()   = dispatchEvent(dom, "mouseup")
  }

  final class Edge($: DomZipperJs) {
    private val dom  = $.dom.asInstanceOf[svg.G]
    private val dom2 = $("." + *.clsEdge2).dom.asInstanceOf[svg.G]
    val id           = Option(dom.id).getOrElse("")
    val isSelected   = dom.classList.contains(*.clsSelectedEdge)
    def click()      = dispatchEvent(dom2, "click")
  }

  sealed trait DragState

  object DragState {
    case object None      extends DragState
    case object Invisible extends DragState
    case object Valid     extends DragState
    case object Invalid   extends DragState
    case object NoOp      extends DragState

    implicit def univEq: UnivEq[DragState] = UnivEq.derive
  }

  // ===================================================================================================================

  final class TestDsl[R, O, S](val dsl: Dsl[Id, R, O, S, String])
                              (getObs: O => Option[ImpGraphObs]) {

    private implicit def autoObs(o: O) = getObs(o)

    val selectedEdgeId = dsl.focus("selectedEdgeId").option(_.obs.selectedEdge.map(_.id))
    val visible        = dsl.focus("Graph is visible").value(_.obs.isDefined)
    val dragState      = dsl.focus("DragState").value(_.obs.dragState)
    val nodeIds        = dsl.focus("Node ids").collection(_.obs.map(_.nodeIds).getOrElse(Vector.empty))
    val edgeIds        = dsl.focus("Edge ids").collection(_.obs.map(_.edgeIds).getOrElse(Vector.empty))
    val edgeIdsNE      = dsl.focus("Edge ids (non-empty)").collection(_.obs.map(_.edgeIds.filter(_.nonEmpty)).getOrElse(Vector.empty))

    val invariants: dsl.Invariants = (
      nodeIds.assert.distinct
      & edgeIdsNE.assert.distinct
    )

    private implicit def autoObs2[A](o: A)(implicit ev: A => Option[ImpGraphObs]): ImpGraphObs =
      ev(o).getOrThrow("No SVG found.")

    def dragStart(nodeId: String): dsl.Actions =
      dsl.action(s"dragStart($nodeId)")(_.obs.nodeById(nodeId).mouseDown())

    def dragOver(nodeId: String): dsl.Actions =
      dsl.action(s"dragOver($nodeId)")(_.obs.nodeById(nodeId).mouseMove())

    def dragEnd(nodeId: String): dsl.Actions =
      dsl.action(s"dragEnd($nodeId)")(_.obs.nodeById(nodeId).mouseUp())

    def dragNewEdge(fromAndTo: (String, String)): dsl.Actions = {
      val (from, to) = fromAndTo
      (dragStart(from) >> dragOver(to)).group(s"dragNewEdge $from -> $to")
    }

    private def edgeId(fromAndTo: (String, String)): String = {
      val (from, to) = fromAndTo
      from + "--" + to
    }

    def clickEdge(fromAndTo: (String, String)): dsl.Actions = {
      val id = edgeId(fromAndTo)
      dsl.action("Click edge: " + id)(_.obs.edgeById(id).click())
    }

    def assertSelectedEdge(fromAndTo: (String, String)) = {
      val id = edgeId(fromAndTo)
      selectedEdgeId.assert(Some(id))
    }
  }

}
