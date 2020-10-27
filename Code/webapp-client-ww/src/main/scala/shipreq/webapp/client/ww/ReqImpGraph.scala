package shipreq.webapp.client.ww

import scala.collection.mutable
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview.ImpGraphConfig.{Colours, GraphDir}

final class ReqImpGraph(focus     : ReqId,
                        filterDead: FilterDead,
                        project   : Project,
                        colours   : Option[Colours]
                       ) extends AbstractGraph(project, filterDead) {
  import AbstractGraph._
  import ReqImpGraph._

  override protected def create()(implicit b: GraphViz.Builder): Unit = {
    implicit val lblFmt = LabelFormatter.pubid
    implicit val shape  = Shape.Ellipse
    val focusedReq      = reqs.need(focus)
    val colourProvider  = colours.map(ColourProvider(_, this).apply(scope))

    val declared = mutable.Set.empty[ReqId]

    def declareNode(id: ReqId): Unit = {
      assert(!declared.contains(id))
      colourProvider match {
        case None =>
          nodeDeclare(id)
          nodeAddIdAndLabel(id)
          nodeStyleLiveOrDead(live(id))
        case Some(c) =>
          c.declareNode(id)
      }
      declared += id
    }

    val filterIdSet: Set[ReqId] => Set[ReqId] =
      filterDead(_)(live)

    val focusLive = live(focus)

    def edgeThunk(from        : String,
                  fromLive    : Live,
                  to          : ReqId,
                  unconstrain : Boolean = false)
                 (implicit dir: Direction): Thunk =
      () => {
        b.flowS(from, dir, nodeId(to))

        if (fromLive.is(Dead) || live(to).is(Dead))
          edgeStyleDead()

        if (unconstrain)
          b append "[constraint=0]"
        else
          b.eol()
      }

    def traverse(implicit dir: Direction) = {
      val graph    = imps(dir)
      val direct   = filterIdSet(graph(focus))
      val indirect = DeclAndFlow(List.newBuilder[ReqId], List.newBuilder[Thunk])

      mutableGraphTraversal(direct) { fromId =>
        if (!direct.contains(fromId))
          indirect.decl += fromId

        val toIds = filterIdSet(graph(fromId))
        for (toId <- toIds)
          indirect.flow += edgeThunk(nodeId(fromId), live(fromId), toId, direct contains toId)

        toIds
      }

      val d = DeclAndFlow(direct, direct.iterator.map(edgeThunk(Focus, focusLive, _)))
      val i = indirect.bimap(_.result(), _.result())
      DirectAndIndirect(d, i)
    }

    val forwards  = traverse(Forwards)
    val backwards = traverse(Backwards)

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    // Externals

    val isInternal: ReqId => Boolean = {
      val everythingBackwards = project.implicationTgtToSrcTC(focus)
      val everythingForwards  = project.implicationSrcToTgtTC(focus)
      i => everythingBackwards.contains(i) || everythingForwards.contains(i)
    }

    val externals: Set[ReqId] = {
      var results = Set.empty[ReqId]

      @inline def impFieldIterator() =
        project.config.fieldsForReqTypeIterator(focusedReq.reqTypeId, filterDead)
          .collect { case f: CustomField.Implication => f }

      val impFieldLookup = project.dataLogic.customFieldImps(filterDead)

      for {
        impField <- impFieldIterator()
        id       <- impFieldLookup(impField.id).getReqIds(focus)
      } {
        if (!isInternal(id))
          results += id
      }

      results
    }

    var externalEdges = List.empty[Thunk]
    val externalEdgesSeen = mutable.Set.empty[(ReqId, ReqId)]

    def declareExternals(): Unit = {
      implicit val dir = Forwards

      def follow(from: ReqId, history: List[Thunk]): Unit =
        if (isInternal(from)) {
          // We found a link from an external root back to the internal graph
          history.foreach(_())

        } else {
          val fromId = nodeId(from)
          val fromLive = live(from)

          var children = imps.forwards(from).iterator
          children = filterDead.filterFn.iteratorBy(children)(live)

          for (to <- children) {
            val add = () => {
              if (!declared.contains(from)) declareNode(from)
              if (!declared.contains(to)) declareNode(to)
              val key = ((from, to))
              if (!externalEdgesSeen.contains(key)) {
                externalEdgesSeen += key
                externalEdges ::= edgeThunk(fromId, fromLive, to)
              }
            }
            follow(to, add :: history)
          }
        }

      for (e <- externals)
        follow(e, Nil)
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    b.rankdir(GraphDir.LeftToRight)
    subsequentNodesStyleAsImplications(shape)

    // Focus
    b append Focus
    b append """[style=bold fillcolor="#cccccc" """
    b.setLabel(lblFmt(focus))
    b append ']'
    declared += focus

    // Nodes

    b append """node[fillcolor="#FFEDE2"]""";
    backwards.indirect.decl.foreach(declareNode)

    b.attrGroup("""rank=same;node[fillcolor="#FFC19C"]""")(
      backwards.direct.decl.foreach(declareNode))

    var forwardsAttr = "rank=same;"
    if (colours.isEmpty)
      forwardsAttr += """node[fillcolor="#7692B7" fontcolor=white]"""
    b.attrGroup(forwardsAttr)(
      forwards.direct.decl.foreach(declareNode))

    b append """node[fillcolor="#D6E1EF"]"""
    forwards.indirect.decl.foreach(declareNode)

    b append """node[fillcolor="#eeeeee" color="#aaaaaa" fontcolor="#444444"]"""
    declareExternals()

    // Edges

    b append """edge[color="#FFC19C"]"""
    backwards.indirect.flow.foreach(_())

    b append """edge[color="#C27040"]"""
    backwards.direct.flow.foreach(_())

    b append """edge[color="#31537F"]"""
    forwards.direct.flow.foreach(_())

    b append """edge[color="#7692B7"]"""
    forwards.indirect.flow.foreach(_())

    b append """edge[color="#aaaaaa" style=dashed]"""
    externalEdges.foreach(_())
  }
}

object ReqImpGraph {
  final val Focus = "F"

  final case class DirectAndIndirect[D, I](direct: D, indirect: I)
}