package shipreq.webapp.client.ww

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import nyaya.util.Multimap
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import shipreq.base.util.VectorTree.PartialLocation
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.data.savedview.ImpGraphConfig.{Colours, GraphDir, LabelFormat}
import shipreq.webapp.base.text.{PlainText, ProjectText}
import shipreq.webapp.client.ww.GraphViz.DOT
import shipreq.webapp.client.ww.api.WebWorkerCmd

object Graphs {

  private type Content = () => Unit

  /** Declaration of node(s), and flow(s). */
  private final case class DeclAndFlow[D, F](decl: D, flow: F) {
    def bimap[DD, FF](d: D => DD, f: F => FF) =
      DeclAndFlow(d(decl), f(flow))
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

    GraphViz.digraph { implicit b =>

      val terminalStyleEnd = " style=filled color=black fontsize=1 height=.3]"
      def startNode(): Unit = {
        b append StartNode
        b append "[shape=circle tooltip=Start"
        b append terminalStyleEnd
      }

      def endNode(): Unit = {
        b append EndNode
        b append "[shape=doublecircle tooltip=End"
        b append terminalStyleEnd
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
              b append node
              b.attrBlock {
                b.setLabel(label)
                b append ' '
                b.setTooltip(ptext.text(step.titleA(uc), UseCaseStep.live(uc, ploc), Mandatory))
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
          b.attrGroup(attr)(fs.iterator.foreach(_()))

      def implicitFlow(steps    : UseCaseSteps,
                       tf       : UseCaseSteps.Tree => Range,
                       fromStart: ImplicitFlow,
                       toEnd    : ImplicitFlow): Unit = {

        var prevStep: UseCaseStep = null

        def handleEnd(): Unit =
          if (prevStep ne null) {
            if (toEnd.link(flow(prevStep.id))) {
              b.arrow()
              b append EndNode
            }
            b.eol()
            prevStep = null
          }

        steps.tree.subtreeLocAndValueIterator(tf(steps.tree), (loc, step) =>
          for (node <- getNode(step.id)) {

            // Beginning of new flow (eg. n.1, n.2, n.3, n.E.1, n.E.2)
            if (loc.tail.isEmpty) {
              handleEnd()
              if (fromStart.link(flowBack(step.id))) {
                b append StartNode
                b.arrow()
              }
            } else
              // Flow continuation (1->2->...)
              b.arrow()

            b append node
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
          b.flowS(fromNode, Forwards, toNode)
          b.eol()
        }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      b.rankdir(GraphDir.LeftToRight)
      b append "ranksep=0.28;"

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

      b.attrGroup("edge[weight=9]")(
        implicitFlow(stepsNA, NA.treeFilterN, ImplicitFlow.Force, ImplicitFlow.Force))

      b.eolAfterChange(implicitFlow(stepsNA, NA.treeFilterA, ImplicitFlow.Default, ImplicitFlow.Default))
      b.eolAfterChange(implicitFlow(stepsE , E .treeFilter , ImplicitFlow.Default, ImplicitFlow.Default))

      explicitFlow(stepsNA.tree)
      explicitFlow(stepsE .tree)
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private final class ImpHelpers(reqs: Requirements, reqTypes: ReqTypes)(implicit b: GraphViz.Builder) {
    val live: ReqId => Live =
      Memo(reqs.need(_).live(reqTypes))

    val pubid   : ReqId => String = PlainText.pubidByReqId(_, reqs, reqTypes)
    val nodeName: ReqId => String = _.value.toString
    val node    : ReqId => Unit   = id => b.append(id.value)

    lazy val reqIdsSortedByPubId = Project.reqIdsSortedByPubId(reqs, reqTypes)
  }

  private def deadLink()(implicit b: GraphViz.Builder): Unit =
    b append """[color="#bbbbbb" style=dashed]"""

  private def styleSubsequentNodesAsImplications()(implicit b: GraphViz.Builder): Unit =
    b append """node[style=filled color="#333333"]"""

  private def deadNodeStyle()(implicit b: GraphViz.Builder): Unit =
    b append """[fillcolor="#dddddd" color="#777777" fontcolor="#666666"]"""

  @inline private def deadNodeStyleIfDead(live: Live)(implicit b: GraphViz.Builder): Unit =
    if (live is Dead) deadNodeStyle()

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def implicationFocused(focus: ReqId, fd: FilterDead, p: Project): DOT =
    implicationFocused(focus, fd, p.content.implications, p.content.reqs, p.config.reqTypes)

  def implicationFocused(focus   : ReqId,
                         fd      : FilterDead,
                         imps    : Implications.BiDir,
                         reqs    : Requirements,
                         reqTypes: ReqTypes): DOT =
    GraphViz.digraph { implicit b =>

      val impHelpers = new ImpHelpers(reqs, reqTypes)
      import impHelpers._

      def declareNode(id: ReqId): Unit = {
        node(id)
        b.labelAttr(pubid(id))
        deadNodeStyleIfDead(live(id))
      }

      val filterIdSet: Set[ReqId] => Set[ReqId] =
        fd(_)(live)

      val Focus = "F"
      val focusLive = live(focus)

      def traverse(dir: Direction) = {
        val graph    = imps(dir)
        val direct   = filterIdSet(graph(focus))
        val indirect = DeclAndFlow(List.newBuilder[ReqId], List.newBuilder[Content])

        def flow(from: String, fromLive: Live, to: ReqId, unconstrain: Boolean): Content =
          () => {
            b.flowS(from, dir, nodeName(to))

            if (fromLive.is(Dead) || live(to).is(Dead))
              deadLink()

            if (unconstrain)
              b append "[constraint=0]"
            else
              b.eol()
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

      b.rankdir(GraphDir.LeftToRight)
      styleSubsequentNodesAsImplications()

      // Focus
      b append Focus
      b append """[style=bold fillcolor="#cccccc" """
      b.setLabel(pubid(focus))
      b append ']'

      b append """node[fillcolor="#FFEDE2"]""";
      backwards.indirect.decl.foreach(declareNode)

      b.attrGroup("""rank=same;node[fillcolor="#FFC19C"]""")(
        backwards.direct.decl.foreach(declareNode))

      b.attrGroup("""rank=same;node[fillcolor="#7692B7" fontcolor=white]""")(
        forwards.direct.decl.foreach(declareNode))

      b append """node[fillcolor="#D6E1EF"]""";
      forwards.indirect.decl.foreach(declareNode)

      b append """edge[color="#FFC19C"]"""
      backwards.indirect.flow.foreach(_ ())

      b append """edge[color="#C27040"]"""
      backwards.direct.flow.foreach(_ ())

      b append """edge[color="#31537F"]"""
      forwards.direct.flow.foreach(_ ())

      b append """edge[color="#7692B7"]"""
      forwards.indirect.flow.foreach(_ ())
    }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def implicationAll(project   : Project,
                     plainText : PlainText.ForProject.NoCtx,
                     filterDead: FilterDead,
                     scope     : Option[Set[ReqId]],
                     config    : ImpGraphConfig): DOT =
    GraphViz.digraph { implicit b =>

      val imps           = project.content.implications
      val reqs           = project.content.reqs
      val reqTypes       = project.config.reqTypes
      val impHelpers     = new ImpHelpers(reqs, reqTypes)
      val reqIdFilter    = OptionalBoolFn[ReqId](scope.map(s => s.contains _))
      val reqIdSetFilter = reqIdFilter.setFilter
      val reqFilter      = reqIdFilter.contramap[Req](_.id)

      import impHelpers._

      val drawBoxes =
        config.labelFormat match {
          case LabelFormat.Pubid         => false
          case LabelFormat.PubidAndTitle => true
        }

      val multiColourStyle =
        if (drawBoxes) "striped" else "wedged"

      val label: ReqId => String =
        config.labelFormat match {
          case LabelFormat.Pubid =>
            pubid

          case LabelFormat.PubidAndTitle =>
            id => {
              val p = pubid(id)
              val t = plainText.reqTitleById(id)
              WrapText(s"$p:\n$t", CharWidths('m') * 26)
            }
        }

      def declareNodeLabel(id: ReqId): Unit =
        b.labelAttr(label(id))

      def declareNodes: () => Unit =
        config.colours match {

          case Colours.ByReqType => () => {

            // Add to reqTypesWithReqs regardless of live status so that colours don't change when user toggles the
            // FilterDead setting. Colours jumping around it's a needless cognitive burden when you're trying to analyse
            // the graph.
            val reqTypesWithReqs: Map[ReqTypeId, Int] =
              MutableArray(reqs.reqsByType.keys)
                .sortBy(reqTypes.order) // Deterministic (and stable until config changes) order of colours
                .iterator
                .zipWithIndex
                .toMap

            val reqsByReqType: Multimap[ReqTypeId, Vector, Req] =
              reqFilter.value match {
                case Some(f) =>
                  var m = reqs.reqsByType
                  for (rt <- m.keys)
                    m = m.mod(rt, _.filter(f))
                  m
                case None =>
                  reqs.reqsByType
              }

            val colourFn =
              DistinctColours("ffffff", reqTypesWithReqs.size, "ffffff")

            def nodeData =
              MutableArray(reqsByReqType.iterator)
                .sortBy(x => reqTypes.order(x._1))
                .iterator

            def declareNode(id: ReqId): Unit = {
              node(id)
              declareNodeLabel(id)
              deadNodeStyleIfDead(live(id))
            }

            for ((reqType, reqs) <- nodeData) {
              val color = colourFn(reqTypesWithReqs(reqType))
              b append """node[fillcolor="#"""
              b append color
              b append """"]"""
              for (req <- reqs)
                declareNode(req.id)
            }
          }

          case Colours.ByTag(tagGroupId) => () => {

            val coloursByReqId: Map[ReqId, ArraySeq[Colour]] = {
              val tagLookup = project.dataLogic.tagLookup(filterDead)
              val tags      = project.config.tags
              val tagScope  = project.config.tagFieldDistribution(filterDead).inTagGroup(tagGroupId)
              val reqIds    = scope.fold(project.content.reqs.idIterator())(_.iterator)
              reqIds.map { reqId =>
                val colours =
                  tagLookup(reqId).all
                    .iterator
                    .filter(tagScope.contains)
                    .map(tags.needApplicableTag)
                    .map(t => t.colour.getOrElse(Colour.tagDefault).live(t.live))
                    .to(ArraySeq)
                reqId -> colours
              }.toMap
            }

            def declareNode(id: ReqId): Unit = {
              node(id)
              declareNodeLabel(id)
              live(id) match {
                case Live =>
                  val colours = coloursByReqId.get(id).filter(_.nonEmpty).getOrElse(ArraySeq(Colour.tagDefault))
                  val style =
                    if (colours.length == 1) {
                      // style=wedged requires at least 2 colours
                      val c = colours(0)
                      s"""[style=filled fillcolor="${c.`#rrggbb`}" fontcolor="${c.foreground.`#rrggbb`}"]"""
                    } else {
                      val fill = colours.iterator.map(_.`#rrggbb`).mkString(":")
                      val font = Colour.chooseForegroundOverMultipleBackgroundColours(colours)
                      s"""[style=$multiColourStyle fillcolor="$fill" fontcolor="${font.`#rrggbb`}"]"""
                    }
                  b.append(style)
                case Dead =>
                  deadNodeStyle()
              }
            }

            b.append("""node[color="#111111"]""")
            for (reqId <- reqIdFilter.iterator(reqIdsSortedByPubId.iterator)) {
              declareNode(reqId)
              coloursByReqId
            }
          }
        }

      val impReqResult = DataLogic.requiringImplication(reqTypes, imps, reqs)

      def flow(fromId: ReqId, fromLive: Live, toIds: IterableOnce[ReqId], toLive: Live): Unit = {
        val atEnd: () => Unit =
          fromLive & toLive match {
            case Live => () => ()
            case Dead => () => deadLink()
          }
        b.flowOneToMany(fromId, toIds)(node, atEnd())
      }

      def allFlow(graph: Implications.UniDir): Unit =
        for ((fromId, toIds) <- graph.iterator)
          if (reqIdFilter(fromId)) {
            val fromLive = live(fromId)
            val (l, d) = reqIdSetFilter(toIds).partition(live(_) is Live)
            flow(fromId, fromLive, l, Live)
            flow(fromId, fromLive, d, Dead)
          }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      b.rankdir(config.graphDir)
      styleSubsequentNodesAsImplications()
      b append """edge[color="#333333"]"""
      if (drawBoxes)
        b append "node[shape=box]"

      declareNodes()

      // Implication required
      if (impReqResult.badIds.nonEmpty)
        b.attrGroup("edge[color=\"#dd0000\"]") {
          b append "R[shape=octagon fillcolor=red fontcolor=white margin=0 fontsize=18 label=\"?\"]"
          b.flowOneToMany("R", impReqResult.badIds.iterator map nodeName)(b.append, ())
          allFlow(impReqResult.badImpGraph.forwards)
        }

      // Flow
      allFlow(impReqResult.goodImpGraph.forwards)
    }

}
