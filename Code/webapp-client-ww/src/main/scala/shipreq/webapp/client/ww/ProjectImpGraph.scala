package shipreq.webapp.client.ww

import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.text.PlainText

final class ProjectImpGraph(project   : Project,
                            plainText : PlainText.ForProject.NoCtx,
                            filterDead: FilterDead,
                            _scope    : Option[Set[ReqId]],
                            config    : ImpGraphConfig) extends AbstractGraph(project, filterDead) {
  import AbstractGraph._

  override protected def scope = _scope

  override protected def create()(implicit b: GraphViz.Builder): Unit = {
    implicit val lblFmt = LabelFormatter(config.labelFormat, plainText)
    implicit val shape  = Shape(config.labelFormat)
    val colourProvider  = ColourProvider(config.colours)
    val impReqResult    = DataLogic.requiringImplication(reqTypes, imps.graph, reqs)

    def flow(fromId: ReqId, fromLive: Live, toIds: IterableOnce[ReqId], toLive: Live): Unit = {
      val atEnd: () => Unit =
        fromLive & toLive match {
          case Live => () => ()
          case Dead => () => edgeStyleDead()
        }
      b.flowOneToMany(fromId, toIds)(nodeDeclare, atEnd())
    }

    def allFlow(graph: Implications.Graph.UniDir): Unit =
      for ((fromId, toIds) <- graph.iterator)
        if (reqIdFilter(fromId)) {
          val fromLive = live(fromId)
          val (l, d) = reqIdSetFilter(toIds).partition(live(_) is Live)
          flow(fromId, fromLive, l, Live)
          flow(fromId, fromLive, d, Dead)
        }

    // -----------------------------------------------------------------------------------------------------------------

    b.rankdir(config.graphDir)
    subsequentNodesStyleAsImplications(shape)
    b.append(s"""edge[color="$blackish"]""")

    colourProvider.declareAllReqsInScope()

    // Implication required
    val relevantReqsWithoutImp = reqIdFilter.setFilter(impReqResult.badIds)
    if (relevantReqsWithoutImp.nonEmpty)
      b.attrGroup("edge[color=\"#dd0000\"]") {
        b append "R[shape=octagon fillcolor=red fontcolor=white margin=0 fontsize=18 label=\"?\"]"
        b.flowOneToMany("R", relevantReqsWithoutImp.iterator.map(nodeId))(b.append, ())
        allFlow(impReqResult.badImpGraph.forwards)
      }

    // Flow
    allFlow(impReqResult.goodImpGraph.forwards)
  }
}

