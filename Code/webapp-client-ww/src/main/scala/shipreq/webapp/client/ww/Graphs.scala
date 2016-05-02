package shipreq.webapp.client.ww

import scala.annotation.tailrec
import scala.collection.mutable
import shipreq.base.util.univeq._
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

  def flowS(from: String, dir: Direction, to: String)(implicit sb: StringBuilder): Unit =
    flowSB(sb append from, dir, sb append to)

  def flowSB(from: => Unit, dir: Direction, to: => Unit)(implicit sb: StringBuilder): Unit =
    dir match {
      case Forwards  => from; arrow(); to
      case Backwards => to  ; arrow(); from
    }

  def arrow()(implicit sb: StringBuilder): Unit =
    sb append "->"

  def eol()(implicit sb: StringBuilder): Unit =
    sb append ';'

  def rankdirLR()(implicit sb: StringBuilder): Unit = sb append "rankdir=LR;"
  def rankdirTB()(implicit sb: StringBuilder): Unit = sb append "rankdir=TB;"

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
                eol()
            } else
              // Flow continuation
              arrow()

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
          flowS(fromNode, Forwards, toNode)
          eol()
        }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      rankdirLR()
      sb append "ranksep=0.28;"

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
        arrow()
        implicitFlow(stepsNA, NA, NA.treeFilterN)
        arrow()
        sb append EndNode
      }

      implicitFlow(stepsNA, NA, NA.treeFilterA); sb append ';'
      implicitFlow(stepsE , E , E .treeFilter ); sb append ';'

      explicitFlow(stepsNA.tree)
      explicitFlow(stepsE .tree)
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private final class ImpHelpers(fd: FilterDead, reqs: Requirements, reqTypes: ReqTypes)(implicit sb: StringBuilder) {
    val live: ReqId => Live =
      Memo(reqs.need(_).live(reqTypes))

    val filterIdSet: Set[ReqId] => Set[ReqId] =
      fd(_)(live)

    val pubid: ReqId => String =
      PlainText.pubidByReqId(_, reqs, reqTypes)

    def declare(ids: TraversableOnce[ReqId]): Unit =
      for (id <- ids) {
        sb append id.value
        labelAttr(pubid(id))
        if (live(id) :: Dead)
          sb append """[fillcolor="#dddddd" color="#777777" fontcolor="#666666"]"""
      }

    def deadLink()(implicit sb: StringBuilder): Unit =
      sb append """[color="#bbbbbb" style=dashed]"""
  }

  private def implicationNodeStyle()(implicit sb: StringBuilder): Unit =
    sb append """node[style=filled color="#333333"]"""

  def implicationFocused(focus: ReqId, fd: FilterDead, p: Project): DOT =
    implicationFocused(focus, fd, p.implications, p.reqs, p.config.reqTypes)

  def implicationFocused(focus: ReqId, fd: FilterDead,
                         imps: Implications.BiDir, reqs: Requirements, reqTypes: ReqTypes): DOT =
    digraph { implicit sb =>

      val impHelpers = new ImpHelpers(fd, reqs, reqTypes)
      import impHelpers._

      val Focus = "F"
      val focusLive = live(focus)

      def traverse(dir: Direction) = {
        val graph    = imps(dir)
        val direct   = filterIdSet(graph(focus))
        val indirect = DeclAndFlow(List.newBuilder[ReqId], List.newBuilder[Content])

        def flow(from: String, fromLive: Live, to: ReqId, unconstrain: Boolean): Content =
          () => {
            flowS(from, dir, to.value.toString)

            if (fromLive :: Dead || live(to) :: Dead)
              deadLink()

            if (unconstrain)
              sb append "[constraint=0]"
            else
              eol()
          }

        @tailrec
        def go(queue: List[ReqId], queueNext: Set[ReqId], seen: Set[ReqId]): Unit =
          queue match {
            case Nil =>
              if (queueNext.nonEmpty)
                go(queueNext.toList, Set.empty, seen)

            case fromId :: queue2 =>
              if (seen.contains(fromId))
                go(queue2, queueNext, seen)
              else {

                if (!direct.contains(fromId))
                  indirect.decl += fromId

                val toIds = filterIdSet(graph(fromId))
                for (toId <- toIds)
                  indirect.flow += flow(fromId.value.toString, live(fromId), toId, direct contains toId)

                go(queue2, queueNext ++ toIds, seen + fromId)
              }
          }

        go(Nil, direct, Set.empty)

        val d = DeclAndFlow(direct, direct.iterator.map(flow(Focus, focusLive, _, false)))
        val i = indirect.bimap(_.result(), _.result())
        DirectAndIndirect(d, i)
      }

      val forwards  = traverse(Forwards)
      val backwards = traverse(Backwards)

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      rankdirLR()
      implicationNodeStyle()

      // Focus
      sb append Focus
      sb append """[style=bold fillcolor="#cccccc" """
      setLabel(pubid(focus))
      sb append ']'

      sb append """node[fillcolor="#FFEDE2"]""";                           declare(backwards.indirect.decl)
      attrGroup("""rank=same;node[fillcolor="#FFC19C"]"""                )(declare(backwards.direct  .decl))
      attrGroup("""rank=same;node[fillcolor="#7692B7" fontcolor=white]""")(declare(forwards .direct  .decl))
      sb append """node[fillcolor="#D6E1EF"]""";                           declare(forwards .indirect.decl)

      sb append """edge[color="#FFC19C"]"""; backwards.indirect.flow.foreach(_())
      sb append """edge[color="#C27040"]"""; backwards.direct  .flow.foreach(_())
      sb append """edge[color="#31537F"]"""; forwards .direct  .flow.foreach(_())
      sb append """edge[color="#7692B7"]"""; forwards .indirect.flow.foreach(_())
    }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def implicationAll(fd: FilterDead, p: Project): DOT =
    implicationAll(fd, p.implications, p.reqs, p.config.reqTypes)

  def implicationAll(fd: FilterDead,
                     imps: Implications.BiDir, reqs: Requirements, reqTypes: ReqTypes): DOT =
    digraph { implicit sb =>
      val impHelpers = new ImpHelpers(fd, reqs, reqTypes)
      import impHelpers._

      // Add to reqTypesWithReqs regardless of live status so that colours don't change when user toggles the
      // FilterDead setting. Colours jumping around it's a needless cognitive burden when you're trying to analyse
      // the graph.
      val reqTypesWithReqs: Map[ReqTypeId, Int] =
        MutableArray(reqs.reqsByType.keys)
          .sortBy(reqTypes.order) // Deterministic (and stable until config changes) order of colours
          .iterator
          .zipWithIndex
          .toMap

      val reqsByReqType = {
        var x = reqs.reqsByType
        if (fd :: HideDead)
          for (rt <- x.keys)
            x = x.mod(rt, _.filter(_.live(reqTypes) :: Live))
        x
      }

      val impReqResult = DataLogic.requiringImplication(reqTypes, imps, reqs)

      val colourFn =
        DistinctColours("ffffff", reqTypesWithReqs.size, "ffffff")

      def nodeData =
        MutableArray(reqsByReqType.iterator)
          .sortBy(x => reqTypes.order(x._1))
          .iterator

      def flow(fromId: ReqId, fromLive: Live, toIds: TraversableOnce[ReqId], toLive: Live): Unit =
        if (toIds.nonEmpty) {
          sb append fromId.value
          arrow()
          intercalate(toIds, sb append ',')(sb append _.value)
          fromLive & toLive match {
            case Live => eol()
            case Dead => deadLink()
          }
        }

      def allFlow(graph: Implications.UniDir): Unit =
        for ((fromId, toIds) <- graph.iterator) {
          val fromLive = live(fromId)
          fd match {
            case HideDead =>
              if (fd.filterFn(fromLive))
                flow(fromId, fromLive, filterIdSet(toIds), Live)
            case ShowDead =>
              val (l, d) = toIds.partition(live(_) :: Live)
              flow(fromId, fromLive, l, Live)
              flow(fromId, fromLive, d, Dead)
          }
        }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      rankdirTB()
      implicationNodeStyle()
      sb append """edge[color="#333333"]"""

      // Declare nodes
      for ((reqType, reqs) <- nodeData) {
        val color = colourFn(reqTypesWithReqs(reqType))
        sb append """node[fillcolor="#"""
        sb append color
        sb append """"]"""
        declare(reqs.iterator.map(_.id))
      }

      // Implication required
      if (impReqResult.badIds.nonEmpty)
        attrGroup("edge[color=\"#dd0000\"]") {
          sb append "R[shape=octagon fillcolor=red fontcolor=white margin=0 fontsize=18 label=\"?\"]R"
          arrow()
          intercalate(impReqResult.badIds, sb append ',')(sb append _.value)
          eol()
          allFlow(impReqResult.badImpGraph.forwards)
        }

      // Flow
      allFlow(impReqResult.goodImpGraph.forwards)
    }

}
