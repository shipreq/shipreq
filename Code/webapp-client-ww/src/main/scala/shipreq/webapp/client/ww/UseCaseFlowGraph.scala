package shipreq.webapp.client.ww

import japgolly.microlibs.stdlib_ext.StdlibExt._
import scala.collection.mutable
import shipreq.base.util.VectorTree.PartialLocation
import shipreq.base.util._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview.ImpGraphConfig.GraphDir
import shipreq.webapp.member.project.text.{PlainText, ProjectText}

/**
 * Creates a graph of the flow of steps in a given UseCase.
 *
 * Currently only graphs intra-usecase flow. Flow to or from other UseCases is currently ignored.
 */
final class UseCaseFlowGraph(id     : UseCaseId,
                             project: Project,
                             ctx    : ProjectText.Context) extends AbstractGraph(project, HideDead) {

  import AbstractGraph._
  import StaticField.{ExceptionStepTree => E, NormalAltStepTree => NA, UseCaseStepTree => F}
  import UseCaseFlowGraph._

  override protected def create()(implicit b: GraphViz.Builder): Unit = {
    val ptext    = PlainText.ForProject(project, ctx)
    val useCases = project.content.reqs.useCases
    val uc       = useCases.imap.need(id)
    val stepsNA  = NA.useCaseSteps get uc
    val stepsE   = E .useCaseSteps get uc
    val flow     = useCases.stepFlow.forwards : Digraph.UniDir[UseCaseStepId]
    val flowBack = useCases.stepFlow.backwards: Digraph.UniDir[UseCaseStepId]

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

    def initSubtreeNodes(steps: UseCaseSteps, field: F, tf: UseCaseSteps.Tree => Range): Iterator[(PartialLocation, Thunk)] = {
      steps.tree.subtreeLocAndValueIterator[(PartialLocation, Thunk)](tf(steps.tree), (loc, step) => {
        val ploc = steps.partialLocs.forward(loc)
        if (ploc.validity is Valid) {
          val label = field.stepLabel(uc.pos, ploc, UseCaseStepLabelFmt.`N.m`)
          val node = step.id.value.toString
          register(step.id, node)
          val nodeDOT: Thunk = () => {
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

    def initSubtreeNodesHT(headAttr: String, tailAttr: String, ns: Iterator[(PartialLocation, Thunk)]): Unit = {
      val h = Vector.newBuilder[Thunk]
      val t = Vector.newBuilder[Thunk]
      for (x <- ns)
        (if (x._1.value.tail.isEmpty) h else t) += x._2
      execWithAttr(headAttr, h.result())
      execWithAttr(tailAttr, t.result())
    }

    def execWithAttr(attr: String, fs: IterableOnce[Thunk]): Unit =
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

object UseCaseFlowGraph {
  final val StartNode        = "S"
  final val EndNode          = "E"
  final val terminalStyleEnd = " style=filled color=black fontsize=1 height=.3]"

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
}
