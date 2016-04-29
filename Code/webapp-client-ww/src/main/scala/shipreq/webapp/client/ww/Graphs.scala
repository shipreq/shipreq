package shipreq.webapp.client.ww

import scala.annotation.tailrec
import scala.collection.mutable
import shipreq.base.util.ScalaExt._
import shipreq.base.util.VectorTree.PartialLocation
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import GraphViz.DOT

object Graphs {

  private def digraph(f: StringBuilder => Unit): DOT = {
    implicit val sb = new StringBuilder
    group("digraph G")(f(sb))
    DOT(sb.result())
  }

  private def group(group: String)(inner: => Unit)(implicit sb: StringBuilder): Unit = {
    sb append group
    sb append '{'
    inner
    sb append '}'
  }

  private def attrGroup(attr: String)(inner: => Unit)(implicit sb: StringBuilder): Unit = {
    sb append '{'
    sb append attr
    inner
    sb append '}'
  }

  private def attrBlock(inner: => Unit)(implicit sb: StringBuilder): Unit = {
    sb append '['
    inner
    sb append ']'
  }

  private def setLabel(label: String)(implicit sb: StringBuilder): Unit = {
    sb append "label=\""
    sb append label
    sb append '"'
  }

  /** [label="x"] */
  private def labelAttr(label: String)(implicit sb: StringBuilder): Unit =
    attrBlock {
      setLabel(label)
    }

  private def intercalate[A](as: TraversableOnce[A], between: => Unit)(f: A => Unit): Unit = {
    var first = true
    for (a <- as) {
      if (first)
        first = false
      else
        between
      f(a)
    }
  }

  def flow(from: TraversableOnce[String], to: TraversableOnce[String], dir: Direction = Forwards)(implicit sb: StringBuilder): Boolean =
    if (from.nonEmpty && to.nonEmpty) {
      var a = from
      var b = to
      if (dir :: Backwards) {
        a = to
        b = from
      }
      intercalate(a, sb append ',')(sb append _)
      sb append "->"
      intercalate(b, sb append ',')(sb append _)
      true
    } else
      false

  private type Content = () => Unit

  /** Declaration of node(s), and flow(s). */
  private case class DeclAndFlow[D, F](decl: D, flow: F) {
    def bimap[DD, FF](d: D => DD, f: F => FF) =
      new DeclAndFlow(d(decl), f(flow))
  }

  private case class DirectAndIndirect[D, I](direct: D, indirect: I)

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  /**
   * Creates a graph of the flow of steps in a given UseCase.
   *
   * Currently only graphs intra-usecase flow. Flow to or from other UseCases is currently ignored.
   */
  def useCaseStepFlow(id: UseCaseId, useCases: UseCases): DOT = {
    import StaticField.{ExceptionStepTree => E, NormalAltStepTree => NA, UseCaseStepTree => F}

    val StartNode = "S"
    val EndNode   = "E"

    val uc      = useCases.imap.need(id)
    val stepsNA = NA.useCaseSteps get uc
    val stepsE  = E .useCaseSteps get uc
    val flow    = useCases.stepFlow.forwards: Digraph.UniDir[UseCaseStepId]

    digraph { implicit sb =>

      val terminalStyleEnd = " style=filled color=black fontsize=1 height=.3]"
      def startNode(): Unit = {
        sb append StartNode
        sb append "[shape=circle"
        sb append terminalStyleEnd
      }

      def endNode(): Unit = {
        sb append EndNode
        sb append "[shape=doublecircle"
        sb append terminalStyleEnd
      }

      val _nodes = mutable.Map.empty[UseCaseStepId, String]
      def getNode(id: UseCaseStepId): Option[String] = _nodes.get(id)
      def register(id: UseCaseStepId, node: String): Unit = _nodes.update(id, node)

      def initSubtreeNodes(steps: UseCaseSteps, field: F, tf: UseCaseSteps.Tree => Range): Iterator[(PartialLocation, Content)] = {
        steps.tree.subtreeLocAndValueIterator[(PartialLocation, Content)](tf(steps.tree), (loc, step) => {
          val ploc = steps.partialLocs.forward(loc)
          if (ploc.validity :: Valid) {
            val label = field.stepLabel(uc.pos, ploc, mnemonicPrefix = false)
            val node = step.id.value.toString
            register(step.id, node)
            val nodeDOT: Content = () => {
              sb append node
              labelAttr(label)
            }
            (ploc, nodeDOT)
          } else
            null
        }).filter(_ ne null)
      }

      def initSubtreeNodesHT(headAttr: String, tailAttr: String, ns: Iterator[(PartialLocation, Content)]): Unit = {
        val h = Vector.newBuilder[Content]
        val t = Vector.newBuilder[Content]
        for (x <- ns)
          (if (x._1.value.tail.isEmpty) h else t) += x._2
        execWithAttr(headAttr, h.result())
        execWithAttr(tailAttr, t.result())
      }

      def execWithAttr(attr: String, fs: TraversableOnce[Content]): Unit =
        if (fs.nonEmpty)
          attrGroup(attr)(fs.foreach(_()))

      def implicitFlow(steps: UseCaseSteps, field: F, tf: UseCaseSteps.Tree => Range): Unit = {
        var first = true
        steps.tree.subtreeLocAndValueIterator(tf(steps.tree), (loc, step) =>
          for (node <- getNode(step.id)) {

            if (loc.tail.isEmpty) {
              // Beginning of a new flow
              if (first)
                first = false
              else
                sb append ';'
            } else
              // Flow continuation
              sb append "->"

            sb append node
          }
        ).drain()
      }

      def explicitFlow(tree: UseCaseSteps.Tree): Unit =
        for {
          fromStep <- tree.valueIterator
          fromNode <- getNode(fromStep.id)
          toStepId <- flow(fromStep.id)
          toNode   <- getNode(toStepId)
        } {
          sb append fromNode
          sb append "->"
          sb append toNode
          sb append ';'
        }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      sb append "rankdir=LR;ranksep=0.28;"

      startNode()
      endNode()

      initSubtreeNodesHT(
        "node[fillcolor=lawngreen style=filled shape=invhouse]",
        "node[fillcolor=lawngreen style=filled shape=ellipse]",
        initSubtreeNodes(stepsNA, NA, NA.treeFilterN))

      initSubtreeNodesHT(
        "node[fillcolor=skyblue style=filled shape=invhouse]",
        "node[fillcolor=skyblue style=\"filled,rounded\" shape=box]",
        initSubtreeNodes(stepsNA, NA, NA.treeFilterA))

      execWithAttr(
        "node[fillcolor=tomato style=filled shape=octagon]",
        initSubtreeNodes(stepsE, E, E.treeFilter).map(_._2))

      attrGroup("edge[weight=9]") {
        sb append StartNode
        sb append "->"
        implicitFlow(stepsNA, NA, NA.treeFilterN)
        sb append "->"
        sb append EndNode
      }

      implicitFlow(stepsNA, NA, NA.treeFilterA); sb append ';'
      implicitFlow(stepsE , E , E .treeFilter ); sb append ';'

      explicitFlow(stepsNA.tree)
      explicitFlow(stepsE .tree)
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  // TODO implication graphs ignore dead reqs
  def implicationFocused(focus: ReqId, imps: Implications.BiDir, reqs: Requirements, customReqTypes: CustomReqTypeIMap): DOT = {
    val Focus = "F"

    val filter: ReqId => Boolean =
      Memo(id => reqs.req(id).live(customReqTypes) :: Live)

    val pubid: ReqId => String =
      PlainText.pubidByReqId(_, reqs, customReqTypes)

    digraph { implicit sb =>

      def declare(ids: TraversableOnce[ReqId]): Unit =
        for (id <- ids) {
          sb append id.value
          labelAttr(pubid(id))
        }

      def traverse(dir: Direction) = {
        val graph    = imps(dir)
        val direct   = graph(focus).filter(filter)
        val indirect = DeclAndFlow(List.newBuilder[ReqId], List.newBuilder[Content])

        def flow2(from: String, to: TraversableOnce[ReqId], unconstrain: Boolean): Content =
          () => if (flow(from :: Nil, to.toIterator.map(_.value.toString), dir)) {
            if (unconstrain)
              sb append "[constraint=0]"
            else
              sb append ';'
          }

        @tailrec
        def go(queue: List[ReqId], queueNext: Set[ReqId], seen: Set[ReqId]): Unit =
          queue match {
            case Nil =>
              if (queueNext.nonEmpty)
                go(queueNext.toList, Set.empty, seen)

            case id :: queue2 =>
              if (seen.contains(id))
                go(queue2, queueNext, seen)
              else {
                val next = graph(id).filter(filter)
                if (!direct.contains(id))
                  indirect.decl += id
                val (x, y) = next.partition(direct.contains)
                indirect.flow += flow2(id.value.toString, x, true)
                indirect.flow += flow2(id.value.toString, y, false)
                go(queue2, queueNext ++ next, seen + id)
              }
          }

        go(Nil, direct, Set.empty)

        val d = DeclAndFlow(direct, flow2(Focus, direct, false))
        val i = indirect.bimap(_.result(), _.result())
        DirectAndIndirect(d, i)
      }

      val forwards  = traverse(Forwards)
      val backwards = traverse(Backwards)

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      sb append """rankdir=LR;node[style=filled color="#333333"]"""

      // Focus
      sb append Focus
      sb append """[style=bold fillcolor="#cccccc" """
      setLabel(pubid(focus))
      sb append ']'

      sb append """node[fillcolor="#FFEDE2"]""";                           declare(backwards.indirect.decl)
      attrGroup("""rank=same;node[fillcolor="#FFC19C"]"""                )(declare(backwards.direct.decl))
      attrGroup("""rank=same;node[fillcolor="#7692B7" fontcolor=white]""")(declare(forwards .direct.decl))
      sb append """node[fillcolor="#D6E1EF"]""";                           declare(forwards .indirect.decl)

      sb append """edge[color="#FFC19C"]"""; backwards.indirect.flow.foreach(_())
      sb append """edge[color="#C27040"]"""; backwards.direct.flow()
      sb append """edge[color="#31537F"]"""; forwards .direct.flow()
      sb append """edge[color="#7692B7"]"""; forwards .indirect.flow.foreach(_())
    }
  }
}
