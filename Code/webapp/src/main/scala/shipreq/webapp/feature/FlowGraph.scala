package shipreq.webapp
package feature

import scalaz.{Cord, Monoid, Foldable, Functor, NonEmptyList}
import lib.ScalazSubset._
import lib.Types._
import uc.UseCase
import uc.field._
import uc.step.{StepNode, StepTreeZipper}

/**
 * Creates a DOT graph of the steps in a UseCase, and how one flows through them.
 * The DOT graph can then be turned into a graphic via Graphviz, Viz.js, et al.
 *
 * Usage: `render(model(uc))`
 */
object FlowGraph {

  /** Convenience method for `render . model` */
  def render(uc: UseCase) = Renderer.render(Modeller.model(uc))

  // =====================================================================================================================
  // Data types

  type Node = StepLabel
  type ExplicitFlow = (Node, Node)
  type ImplicitFlow = NonEmptyList[Node]

  sealed trait Category {
    def render(d: IntraCatData): Cord
  }

  case class FlowGraphModel(
    intraCatData: Map[Category, IntraCatData],
    socialData: SocialData)

  case class IntraCatData(
    headNodes: List[Node],
    implicitFlows: List[ImplicitFlow])

  case class SocialData(
    explicitFlows: List[ExplicitFlow],
    startNodes: List[Node],
    endNodes: List[Node])

  implicit object SocialDataMonoid extends Monoid[SocialData] {
    override val zero = SocialData(List.empty, List.empty, List.empty)
    override def append(a: SocialData, b2: => SocialData) = {
      val b = b2
      SocialData(
        a.explicitFlows ++ b.explicitFlows,
        a.startNodes ++ b.startNodes,
        a.endNodes ++ b.endNodes
      )
    }
  }

  implicit object FlowGraphModelMonoid extends Monoid[FlowGraphModel] {
    override val zero = FlowGraphModel(Map.empty, SocialDataMonoid.zero)
    override def append(a: FlowGraphModel, b2: => FlowGraphModel) = {
      val b = b2
      if (a eq zero) b
      else if (b eq zero) a
      else FlowGraphModel(
        a.intraCatData ++ b.intraCatData,
        a.socialData |+| b.socialData)
    }
  }

  // ===================================================================================================================
  // Model generation

  object Modeller {
    import Renderer.{NC, AC, EC}
    import FlowGraphModelMonoid.zero
    import StepTreeZipper._

    implicit def focus2node(f: AnyFocus): Node = f.label
    implicit def focusS2nodeS(s: List[AnyFocus]): List[Node] = s map focus2node
    implicit def focusL2implicitFL(s: List[NonEmptyList[AnyFocus]]): List[ImplicitFlow] = s map (_ map focus2node)

    def model(uc: UseCase): FlowGraphModel = {
      val labels = uc.stepsAndLabels.value.ab
      def zipBuilder(sfv: StepFieldValue) = DeepBuilder(sfv.textmap, labels)

      uc.fieldValues.toList foldMap (p => {
        val fv = p._2
        p._1 match {
          case TextField(_, _) => zero

          case f: NormalCourseField =>
            val sfv = f.castV(fv)
            val b = zipBuilder(sfv)
            val ncNode :: acNodes = sfv.tree.nodes
            val nc = processZ(NC, b.build(ncNode, Nil))
            val ac = processUnlessEmpty(AC, acNodes, b)
            nc |+| ac

          case f: ExceptionCourseField =>
            val sfv = f.castV(fv)
            processUnlessEmpty(EC, sfv.tree.nodes, zipBuilder(sfv))

          case FlowGraphField(_) => zero
        }
      })
    }

    private def processUnlessEmpty(c: Category, nodes: List[StepNode], b: => DeepBuilder) = nodes match {
      case Nil => zero
      case h :: t => processZ(c, b.build(h, t))
    }

    private def processZ(implicit c: Category, dz: DeepZipper): FlowGraphModel = {
      implicit val fzs = flattenTopNodes(dz)
      implicit val dzL = dz.toList
      val implicitFlowsF = implicitFlows
      val i = IntraCatData(headNodes, implicitFlowsF)
      val sd = SocialData(explicitFlows, startNodes, endNodes(implicitFlowsF))
      FlowGraphModel(Map(c -> i), sd)
    }

    def flattenTopNodes(dz: DeepZipper): List[FlatZipper] = dz map (_.flat) toList

    def headNodes(implicit z: List[DeepFocus]): List[Node] = z

    def implicitFlows(implicit z: List[DeepFocus]): List[NonEmptyList[AnyFocus]] = {
      def implicitFlowL0(f: DeepFocus) = NonEmptyList[AnyFocus](f, implicitFlowLn(f.down, true): _*)
      def implicitFlowLn(o: Option[DeepZipper], force: Boolean): List[AnyFocus] = o match {
        case None => Nil
        case Some(z) =>
          val children = z.toStream.map(f => implicitFlowLn(f.down, false))
          if (force || displayN(z) || children.exists(_.nonEmpty))
            z.toStream.zip(children).foldRight(List.empty[AnyFocus]) {case ((n, ns), acc) => n :: ns ::: acc}
          else
            Nil
      }
      def displayN(z: DeepZipper) = display1(z.focus) || z.rights.exists(display1)
      def display1(f: DeepFocus) = f.flowFromClause.isDefined || f.flowToClause.isDefined
      z map implicitFlowL0
    }

    def explicitFlows(implicit tops: List[FlatZipper]): List[ExplicitFlow] = tops map explicitFlow flatten
    def explicitFlow(z: FlatZipper): List[ExplicitFlow] = z.toList map explicitFlow flatten
    def explicitFlow(y: AnyFocus): List[ExplicitFlow] =
      y.flowToClause map (_.refs.values.toList strengthL focus2node(y)) getOrElse List.empty

    def startNodes(implicit c: Category, dzL: List[DeepFocus]): List[Node] = c match {
      case NC => List(dzL.head.label)
      case AC | EC => dzL filter (_.flowFromClause.isEmpty)
    }

    def endNodes(implicitFlows: List[NonEmptyList[AnyFocus]])(implicit c: Category): List[Node] = {
      val ends = implicitFlows map (_.last)
      c match {
        case NC => ends
        case AC | EC => ends filter (_.flowToClause.isEmpty)
      }
    }
  }

  // ===================================================================================================================
  // Rendering

  object Renderer {
    val ToSymbol = Cord("->")
    val SepSymbol = Cord(";")
    val GrpStart = Cord("{")
    val GrpEnd = Cord("}")

    val StartSymbol = Cord("S")
    val StartDecl = Cord(StartSymbol + "[shape=circle style=filled color=black fontsize=1 height=.3]")
    val EndSymbol = Cord("E")
    val EndDecl = Cord(EndSymbol + "[shape=doublecircle style=filled color=black fontsize=1 height=.3]")

    val GraphGroup = group("digraph G{rankdir=LR;ranksep=0.28;") _
    //val NcGroup = group("subgraph clusterN{style=invis edge[weight=9] node[style=filled fillcolor=lawngreen shape=ellipse]") _
    val NcGroup = anonGroup("edge[weight=9] node[style=filled fillcolor=lawngreen shape=ellipse]") _
    val NcHeadNodeGroup = anonGroup("node[style=filled fillcolor=lawngreen shape=invhouse]") _
    val AcGroup = anonGroup("""node[style="filled,rounded" fillcolor=skyblue shape=box]""") _
    val AcHeadNodeGroup = anonGroup("node[style=filled fillcolor=skyblue shape=invhouse]") _
    val EcGroup = anonGroup("node[style=filled fillcolor=tomato shape=octagon]") _
    val TerminalsGroup = anonGroup("edge[weight=9]") _

    case object NC extends Category {
      override def render(d: IntraCatData) =
        NcHeadNodeGroup(nodeDecls(d.headNodes)) ++
        NcGroup(renderI(d.implicitFlows))
    }

    case object AC extends Category {
      override def render(d: IntraCatData) =
        AcHeadNodeGroup(nodeDecls(d.headNodes)) ++
        AcGroup(renderI(d.implicitFlows))
    }

    case object EC extends Category {
      override def render(d: IntraCatData) =
        EcGroup(renderI(d.implicitFlows))
    }

    def mapIfNonEmpty(c: Cord)(f: Cord => Cord): Cord = if (c.size == 0) c else f(c)

    def group(start: => Cord)(inner: Cord): Cord = mapIfNonEmpty(inner)(start ++ _ ++ GrpEnd)

    def anonGroup(cust: => Cord)(inner: Cord): Cord = group(GrpStart ++ cust)(inner)

    def nodeDotId(n: Node): Cord = '"' -: Cord(n) :- '"'

    def mkStmts[M[_] : Functor : Foldable, A](ma: M[A])(f: A => Cord): Cord = ma map f intercalate SepSymbol

    // Eg. a;b;c
    def nodeDecls(nodes: List[Node]): Cord = nodes match {
      case Nil => Cord.empty
      case h :: Nil => nodeDotId(h)
      case _ => GrpStart ++ mkStmts(nodes)(nodeDotId) ++ GrpEnd
    }

    def renderE(eflows: List[ExplicitFlow]): Cord = mkStmts(eflows)(renderE)

    def renderE(e: ExplicitFlow): Cord = nodeDotId(e._1) ++ ToSymbol ++ nodeDotId(e._2)

    def renderI(iflows: List[ImplicitFlow]): Cord = mkStmts(iflows)(renderI)

    def renderI(iflow: ImplicitFlow): Cord = iflow map nodeDotId intercalate ToSymbol

    def renderC(m: Map[Category, IntraCatData])(c: Category): Cord =
      m.get(c) match {
        case None => Cord.empty
        case Some(d) => c.render(d)
      }

    def renderTerminals(d: SocialData): Cord =
      TerminalsGroup(
        mapIfNonEmpty(nodeDecls(d.startNodes))(StartSymbol ++ ToSymbol ++ _ ++ SepSymbol) ++
        mapIfNonEmpty(nodeDecls(d.endNodes))(_ ++ ToSymbol ++ EndSymbol)
      )

    def render(m: FlowGraphModel): Cord = {
      val renderCat = renderC(m.intraCatData) _
      GraphGroup(
        renderCat(NC) ++
        renderCat(AC) ++
        renderCat(EC) ++
        renderE(m.socialData.explicitFlows) ++ SepSymbol ++
        StartDecl ++ EndDecl ++
        renderTerminals(m.socialData)
      )
    }
  }
}