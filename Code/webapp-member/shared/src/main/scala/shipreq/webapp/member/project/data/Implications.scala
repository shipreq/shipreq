package shipreq.webapp.member.project.data

import monocle.Lens
import monocle.macros.Lenses
import shipreq.base.util._

@Lenses
final case class Implications(graph: Implications.Graph) {

  @inline def apply(dir: Direction): Implications.Graph.UniDir =
    graph(dir)

  @inline def forwards: Implications.Graph.UniDir =
    graph.forwards

  @inline def backwards: Implications.Graph.UniDir =
    graph.backwards

  private[this] lazy val _roots =
    Digraph.Roots.derive(graph)

  def members: Set[ReqId] =
    _roots.members

  def roots: Set[ReqId] =
    _roots.roots
}

object Implications {

  val  Graph = new Digraph.FixAcyclic[ReqId, Int]("implications", _.value)
  type Graph = Graph.BiDir

  val empty: Implications =
    apply(Graph.emptyBiDir)

  val srcToTgt: Lens[Implications, Graph.UniDir] =
    graph ^<-> Graph.biToUni

  implicit def univEq: UnivEq[Implications] = UnivEq.derive
}
