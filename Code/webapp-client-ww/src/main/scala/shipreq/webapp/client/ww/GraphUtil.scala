package shipreq.webapp.client.ww

import japgolly.microlibs.utils.Memo
import scala.annotation.tailrec
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText

private[ww] object GraphUtil {

  sealed trait Shape {
    def declareShape: String
    def style_=(s: String): String

    val styleFilled =
      style_=("filled")

    val styleMultiColour =
      style_=(this match {
        case Shape.Ellipse => "wedged"
        case Shape.Box     => "striped"
      })
  }

  object Shape {
    case object Ellipse extends Shape {
      override def declareShape: String =
        "shape=ellipse"
      override def style_=(s: String): String =
        "style=" + s
    }

    case object Box extends Shape {
      override def declareShape: String =
        "shape=box"
      override def style_=(s: String): String =
        s"""style="rounded,$s""""
    }
  }

  type Content = () => Unit

  /** Declaration of node(s), and flow(s). */
  final case class DeclAndFlow[D, F](decl: D, flow: F) {
    def bimap[DD, FF](d: D => DD, f: F => FF) =
      DeclAndFlow(d(decl), f(flow))
  }

  final case class DirectAndIndirect[D, I](direct: D, indirect: I)

  final class ImpHelpers(reqs: Requirements, reqTypes: ReqTypes)(implicit b: GraphViz.Builder) {
    val live: ReqId => Live =
      Memo(reqs.need(_).live(reqTypes))

    val pubid    : ReqId => String = PlainText.pubidByReqId(_, reqs, reqTypes)
    val nodeName : ReqId => String = _.value.toString
    val node     : ReqId => Unit   = id => b.append(id.value)
    val declareId: ReqId => Unit   = id => b.idAttr(pubid(id))

    lazy val reqIdsSortedByPubId = Project.reqIdsSortedByPubId(reqs, reqTypes)
  }

  def deadLink()(implicit b: GraphViz.Builder): Unit =
    b append """[color="#bbbbbb" style=dashed]"""

  final val blackish = "#222222"

  def styleSubsequentNodesAsImplications(shape: Shape)(implicit b: GraphViz.Builder): Unit =
    b append s"""node[${shape.styleFilled} ${shape.declareShape} color="$blackish"]"""

  def deadNodeStyle()(implicit b: GraphViz.Builder): Unit =
    b append """[fillcolor="#dddddd" color="#777777" fontcolor="#666666"]"""

  @inline def deadNodeStyleIfDead(live: Live)(implicit b: GraphViz.Builder): Unit =
    if (live is Dead) deadNodeStyle()

  /** Traverses a graph processing each node once.
    *
    * Root nodes are also processed.
    *
    * @param process 1. Mutate some external state to record the A.
    *                2. Return the argument node's child-nodes.
    */
  def mutableGraphTraversal[A](roots: Set[A])(process: A => Set[A]): Unit = {
    @tailrec
    def go(queue: List[A], queueNext: Set[A], seen: Set[A]): Unit =
      queue match {
        case Nil =>
          if (queueNext.nonEmpty)
            go(queueNext.toList, Set.empty, seen)

        case fromId :: queue2 =>
          if (seen.contains(fromId))
            go(queue2, queueNext, seen)
          else {
            val toIds = process(fromId)
            go(queue2, queueNext ++ toIds, seen + fromId)
          }
      }
    go(Nil, roots, Set.empty)
  }
}
