package shipreq.webapp.client.ww

import scala.collection.mutable
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview.ImpGraphConfig.GraphDir
import shipreq.webapp.client.ww.GraphViz.DOT

object ImplicationGraph {
  import GraphUtil._

  private final val Focus = "F"

  def apply(focus  : ReqId,
            fd     : FilterDead,
            project: Project): DOT =
    GraphViz.digraph { implicit b =>
      val imps       = project.content.implications
      val reqs       = project.content.reqs
      val reqTypes   = project.config.reqTypes
      val focusedReq = reqs.need(focus)

      val impHelpers = new ImpHelpers(reqs, reqTypes)
      import impHelpers._

      val declared = mutable.Set.empty[ReqId]

      def declareNode(id: ReqId): Unit = {
        assert(!declared.contains(id))
        node(id)
        declareId(id)
        b.labelAttr(pubid(id))
        deadNodeStyleIfDead(live(id))
        declared += id
      }

      val filterIdSet: Set[ReqId] => Set[ReqId] =
        fd(_)(live)

      val focusLive = live(focus)

      def edgeThunk(from        : String,
                    fromLive    : Live,
                    to          : ReqId,
                    unconstrain : Boolean = false)
                   (implicit dir: Direction): Content =
        () => {
          b.flowS(from, dir, nodeName(to))

          if (fromLive.is(Dead) || live(to).is(Dead))
            deadLink()

          if (unconstrain)
            b append "[constraint=0]"
          else
            b.eol()
        }

      def traverse(implicit dir: Direction) = {
        val graph    = imps(dir)
        val direct   = filterIdSet(graph(focus))
        val indirect = DeclAndFlow(List.newBuilder[ReqId], List.newBuilder[Content])

        mutableGraphTraversal(direct) { fromId =>
          if (!direct.contains(fromId))
            indirect.decl += fromId

          val toIds = filterIdSet(graph(fromId))
          for (toId <- toIds)
            indirect.flow += edgeThunk(nodeName(fromId), live(fromId), toId, direct contains toId)

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
          project.config.fieldsForReqTypeIterator(focusedReq.reqTypeId, fd)
            .collect { case f: CustomField.Implication => f }

        val impFieldLookup = project.dataLogic.customFieldImps(fd)

        for {
          impField <- impFieldIterator()
          id       <- impFieldLookup(impField.id).getReqIds(focus)
        } {
          if (!isInternal(id))
            results += id
        }

        results
      }

      var externalEdges = List.empty[Content]

      def declareExternals() = {
        implicit val dir = Forwards
        mutableGraphTraversal(externals) { id =>
          if (declared.contains(id))
            Set.empty
          else {
            declareNode(id)

            val from = nodeName(id)
            val fromLive = live(id)

            var children = imps.forwards(id).iterator
            children = fd.filterFn.iteratorBy(children)(live)

            // [Order is important] Add edges to both internal & external nodes...
            children = children.tapEach(c => externalEdges ::= edgeThunk(from, fromLive, c))

            // [Order is important] ...but stop traversing once we reach the internal nodes.
            children = children.filterNot(isInternal)

            children.toSet
          }
        }
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      b.rankdir(GraphDir.LeftToRight)
      styleSubsequentNodesAsImplications(Shape.Ellipse)

      // Focus
      b append Focus
      b append """[style=bold fillcolor="#cccccc" """
      b.setLabel(pubid(focus))
      b append ']'
      declared += focus

      // Nodes

      b append """node[fillcolor="#FFEDE2"]""";
      backwards.indirect.decl.foreach(declareNode)

      b.attrGroup("""rank=same;node[fillcolor="#FFC19C"]""")(
        backwards.direct.decl.foreach(declareNode))

      b.attrGroup("""rank=same;node[fillcolor="#7692B7" fontcolor=white]""")(
        forwards.direct.decl.foreach(declareNode))

      b append """node[fillcolor="#D6E1EF"]""";
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
