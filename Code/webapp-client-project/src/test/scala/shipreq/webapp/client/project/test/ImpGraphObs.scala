package shipreq.webapp.client.project.test

import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.widgets.GraphComponent

final class ImpGraphObs($: DomZipperJs) {
  import ImpGraphObs._

  //  println()
  //  println($.outerHTML)
  //  println()

  val nodes = $.collect0n("g.node").map(new Node(_))
  val edges = $.collect0n("g.edge").map(new Edge(_))

  val nodeIds = nodes.map(_.id)
  val edgeIds = edges.map(_.id)

  def nodeById(id: String): Node =
    nodes.find(_.id == id).getOrThrow(s"Node [$id] not found")
}

object ImpGraphObs {

  def find($: DomZipperJs): Option[ImpGraphObs] =
    GraphComponent.getGraphSvg($.dom).map { svg =>
      val z = DomZipperJs(svg)
      new ImpGraphObs(z)
    }

  final class Node($: DomZipperJs) {
    private val dom = $.dom
    val id = dom.id

    def mouseDown() = dispatchEvent(dom, "mousedown")
    def mouseMove() = dispatchEvent(dom, "mousemove")
    def mouseUp()   = dispatchEvent(dom, "mouseup")
  }

  final class Edge($: DomZipperJs) {
    private val dom = $.dom
    val id = dom.id
  }

  private def dispatchEvent(target: dom.Node, eventName: String): Unit = {
    val name = eventName.toLowerCase
    val interface =
      if (name.startsWith("mouse"))
        "MouseEvents"
      else
        "Event"
    val event = document.createEvent(interface)
    event.asInstanceOf[js.Dynamic].initEvent(name, true, true)
    target.dispatchEvent(event)
  }

  // ===================================================================================================================

  final class TestDsl[R, O, S](val dsl: Dsl[Id, R, O, S, String])
                              (getObs: O => Option[ImpGraphObs]) {

    private implicit def autoObs(o: O) = getObs(o)

    val visible   = dsl.focus("Graph is visible").value(_.obs.isDefined)
    val nodeIds   = dsl.focus("node ids").collection(_.obs.map(_.nodeIds).getOrElse(Vector.empty))
    val edgeIds   = dsl.focus("edge ids").collection(_.obs.map(_.edgeIds).getOrElse(Vector.empty))
    val edgeIdsNE = dsl.focus("edge ids (non-empty)").collection(_.obs.map(_.edgeIds.filter(_.nonEmpty)).getOrElse(Vector.empty))

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
      (dragStart(from) >> dragOver(to) >> dragEnd(to)).group(s"dragNewEdge $from -> $to")
    }
  }

}
