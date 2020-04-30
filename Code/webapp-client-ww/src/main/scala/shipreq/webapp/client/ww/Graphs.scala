package shipreq.webapp.client.ww

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import scala.annotation.tailrec
import scala.collection.mutable
import scala.scalajs.js.JSON
import shipreq.base.util.univeq._
import shipreq.base.util.VectorTree.PartialLocation
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.text.{PlainText, ProjectText}
import GraphViz.DOT
import shipreq.webapp.base.UiText

object Graphs {

  private def digraph(f: StringBuilder => Unit): DOT = {
    implicit val sb = new StringBuilder
    group("digraph G"){
      sb append "bgcolor=transparent;"
      f(sb)
    }
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

  @inline private def escapeAttrValue(s: String): String =
    JSON.stringify(s)

  private def setLabel(label: String)(implicit sb: StringBuilder): Unit = {
    sb append "label="
    sb append escapeAttrValue(label)
  }

  /** [label="x"] */
  private def labelAttr(label: String)(implicit sb: StringBuilder): Unit =
    attrBlock(setLabel(label))

  /** hover text. TITLE in HTML */
  private def setTooltip(tooltip: String)(implicit sb: StringBuilder): Unit = {
    sb append "tooltip="
    sb append escapeAttrValue(UiText.hoverText(tooltip))
  }

  /*
  Having flow like `1 -> 2,3,4` works fine in the latest versions of GraphViz, but (sometimes) causes problems with the
  version used in Viz.js. Instead they need to be broken into `1->2; 1->3; 1->4`.

  This is a graph that causes viz.js problems:
    digraph G{rankdir=TB;node[style=filled color="#333333"]edge[color="#333333"]node[fillcolor="#91D5BC"]1[label="BL-1"]node[fillcolor="#94DD59"]7[label="CO-1"]8[label="CO-2"]node[fillcolor="#D0A9D4"]5[label="MF-3"]6[label="MF-4"]3[label="MF-1"]4[label="MF-2"]node[fillcolor="#DFB863"]9[label="UC-1"]10[label="UC-2"]11[label="UC-3"]5->6;10->11;9->11,10;3->5,1;11->1;4->5;}
  */
//  private def intercalate[A](as: IterableOnce[A], between: => Unit)(f: A => Unit): Unit = {
//    var first = true
//    for (a <- as) {
//      if (first)
//        first = false
//      else
//        between
//      f(a)
//    }
//  }

  def flowOneToMany[A](fromId: A, toIds: IterableOnce[A])(id: A => Unit, atEnd: => Unit)(implicit sb: StringBuilder): Unit =
    for (toId <- toIds.iterator) {
      id(fromId)
      arrow()
      id(toId)
      atEnd
      eol()
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
    if (sb.last !=* ';') // Guess what? ;; causes Viz.JS to crash! (GraphViz itself is ok with it.)
      sb append ';'

  def eolAfterChange(body: => Unit)(implicit sb: StringBuilder): Unit = {
    val before = sb.length
    body
    if (before !=* sb.length)
      eol()
  }

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
  def useCaseStepFlow(id: UseCaseId, project: Project, ctx: ProjectText.Context): DOT = {
    import StaticField.{ExceptionStepTree => E, NormalAltStepTree => NA, UseCaseStepTree => F}

    val StartNode = "S"
    val EndNode   = "E"

    val ptext    = PlainText.ForProject(project, ctx)
    val useCases = project.content.reqs.useCases
    val uc       = useCases.imap.need(id)
    val stepsNA  = NA.useCaseSteps get uc
    val stepsE   = E .useCaseSteps get uc
    val flow     = useCases.stepFlow.forwards : Digraph.UniDir[UseCaseStepId]
    val flowBack = useCases.stepFlow.backwards: Digraph.UniDir[UseCaseStepId]

    sealed abstract class ImplicitFlow {
      def link(flow: => Set[_]): Boolean
    }
    object ImplicitFlow {

      /** Always link a node to another. */
      case object Force extends ImplicitFlow {
        override def link(flow: => Set[_]) = true
      }

      /** Only link a node to another if it doesn't have any manual flow specified. */
      case object Default extends ImplicitFlow {
        override def link(flow: => Set[_]) = flow.isEmpty
      }

      /** Never link a node to another. */
      case object Never extends ImplicitFlow {
        override def link(flow: => Set[_]) = false
      }
    }

    digraph { implicit sb =>

      val terminalStyleEnd = " style=filled color=black fontsize=1 height=.3]"
      def startNode(): Unit = {
        sb append StartNode
        sb append "[shape=circle tooltip=Start"
        sb append terminalStyleEnd
      }

      def endNode(): Unit = {
        sb append EndNode
        sb append "[shape=doublecircle tooltip=End"
        sb append terminalStyleEnd
      }

      val _nodes = mutable.Map.empty[UseCaseStepId, String]
      def getNode(id: UseCaseStepId): Option[String] = _nodes.get(id)
      def register(id: UseCaseStepId, node: String): Unit = _nodes.update(id, node)

      def initSubtreeNodes(steps: UseCaseSteps, field: F, tf: UseCaseSteps.Tree => Range): Iterator[(PartialLocation, Content)] = {
        steps.tree.subtreeLocAndValueIterator[(PartialLocation, Content)](tf(steps.tree), (loc, step) => {
          val ploc = steps.partialLocs.forward(loc)
          if (ploc.validity is Valid) {
            val label = field.stepLabel(uc.pos, ploc, UseCaseStepLabelFmt.`N.m`)
            val node = step.id.value.toString
            register(step.id, node)
            val nodeDOT: Content = () => {
              sb append node
              attrBlock {
                setLabel(label)
                sb append ' '
                setTooltip(ptext.text(step.titleA(uc), UseCaseStep.live(uc, ploc), Mandatory))
              }
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

      def execWithAttr(attr: String, fs: IterableOnce[Content]): Unit =
        if (fs.iterator.nonEmpty)
          attrGroup(attr)(fs.iterator.foreach(_()))

      def implicitFlow(steps    : UseCaseSteps,
                       field    : F,
                       tf       : UseCaseSteps.Tree => Range,
                       fromStart: ImplicitFlow,
                       toEnd    : ImplicitFlow): Unit = {

        var prevStep: UseCaseStep = null

        def handleEnd(): Unit =
          if (prevStep ne null) {
            if (toEnd.link(flow(prevStep.id))) {
              arrow()
              sb append EndNode
            }
            eol()
            prevStep = null
          }

        steps.tree.subtreeLocAndValueIterator(tf(steps.tree), (loc, step) =>
          for (node <- getNode(step.id)) {

            // Beginning of new flow (eg. n.1, n.2, n.3, n.E.1, n.E.2)
            if (loc.tail.isEmpty) {
              handleEnd()
              if (fromStart.link(flowBack(step.id))) {
                sb append StartNode
                arrow()
              }
            } else
              // Flow continuation (1->2->...)
              arrow()

            sb append node
            prevStep = step
          }
        ).drain()

        handleEnd()
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

      attrGroup("edge[weight=9]")(
        implicitFlow(stepsNA, NA, NA.treeFilterN, ImplicitFlow.Force, ImplicitFlow.Force))

      eolAfterChange(implicitFlow(stepsNA, NA, NA.treeFilterA, ImplicitFlow.Default, ImplicitFlow.Default))
      eolAfterChange(implicitFlow(stepsE , E , E .treeFilter , ImplicitFlow.Default, ImplicitFlow.Default))

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

    val nodeName: ReqId => String = _.value.toString
    val node    : ReqId => Unit   = sb append _.value

    def declare(ids: IterableOnce[ReqId]): Unit =
      for (id <- ids.iterator) {
        node(id)
        labelAttr(pubid(id))
        if (live(id) is Dead)
          sb append """[fillcolor="#dddddd" color="#777777" fontcolor="#666666"]"""
      }

    def deadLink()(implicit sb: StringBuilder): Unit =
      sb append """[color="#bbbbbb" style=dashed]"""
  }

  private def implicationNodeStyle()(implicit sb: StringBuilder): Unit =
    sb append """node[style=filled color="#333333"]"""

  def implicationFocused(focus: ReqId, fd: FilterDead, p: Project): DOT =
    implicationFocused(focus, fd, p.content.implications, p.content.reqs, p.config.reqTypes)

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
            flowS(from, dir, nodeName(to))

            if (fromLive.is(Dead) || live(to).is(Dead))
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
                  indirect.flow += flow(nodeName(fromId), live(fromId), toId, direct contains toId)

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
    implicationAll(fd, p.content.implications, p.content.reqs, p.config.reqTypes)

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
        if (fd is HideDead)
          for (rt <- x.keys)
            x = x.mod(rt, _.filter(_.live(reqTypes) is Live))
        x
      }

      val impReqResult = DataLogic.requiringImplication(reqTypes, imps, reqs)

      val colourFn =
        DistinctColours("ffffff", reqTypesWithReqs.size, "ffffff")

      def nodeData =
        MutableArray(reqsByReqType.iterator)
          .sortBy(x => reqTypes.order(x._1))
          .iterator

      def flow(fromId: ReqId, fromLive: Live, toIds: IterableOnce[ReqId], toLive: Live): Unit = {
        val atEnd: () => Unit =
          fromLive & toLive match {
            case Live => () => ()
            case Dead => () => deadLink()
          }
        flowOneToMany(fromId, toIds)(node, atEnd())
      }

      def allFlow(graph: Implications.UniDir): Unit =
        for ((fromId, toIds) <- graph.iterator) {
          val fromLive = live(fromId)
          fd match {
            case HideDead =>
              if (fd.filter(fromLive))
                flow(fromId, fromLive, filterIdSet(toIds), Live)
            case ShowDead =>
              val (l, d) = toIds.partition(live(_) is Live)
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
          sb append "R[shape=octagon fillcolor=red fontcolor=white margin=0 fontsize=18 label=\"?\"]"
          flowOneToMany("R", impReqResult.badIds.iterator map nodeName)(sb append _, ())
          allFlow(impReqResult.badImpGraph.forwards)
        }

      // Flow
      allFlow(impReqResult.goodImpGraph.forwards)
    }

}
