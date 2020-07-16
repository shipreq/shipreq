package shipreq.webapp.client.ww

import japgolly.microlibs.stdlib_ext.MutableArray
import nyaya.util.Multimap
import scala.collection.immutable.ArraySeq
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.data.savedview.ImpGraphConfig.{Colours, LabelFormat}
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.ww.GraphViz.DOT

object ReqGraph {
  import GraphUtil._

  private val maxWidth = CharWidths('m') * 28

  def apply(project   : Project,
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

      val shape: Shape =
        config.labelFormat match {
          case LabelFormat.Pubid         => Shape.Ellipse
          case LabelFormat.PubidAndTitle => Shape.Box
        }
      import shape._

      val label: ReqId => String =
        config.labelFormat match {

          case LabelFormat.Pubid =>
            pubid

          case LabelFormat.PubidAndTitle =>
            id => {
              val p = pubid(id)
              val t = plainText.reqTitleWithoutMarkupById(id)
              val l = p + "\n" + t
              WrapText(l, maxWidth)
            }
        }

      def declareNodeAttrs(id: ReqId): Unit = {
        b.append('[')
        b.setId(pubid(id))
        b.append(' ')
        b.setLabel(label(id))
        b.append(']')
      }

      def declareNodes: () => Unit =
        config.colours match {

          case Colours.ByReqType => () => {

            // Add to reqTypesWithReqs regardless of live status so that colours don't change when user toggles the
            // FilterDead setting. Colours jumping around it's a needless cognitive burden when you're trying to analyse
            // the graph.
            val reqTypesWithReqs: Map[ReqTypeId, Int] =
              MutableArray(reqs.reqsByType.keys)
                .sortBy(reqTypes.order) // Deterministic (and stable until config changes) order of colours
                .iterator()
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
                .iterator()

            def declareNode(id: ReqId): Unit = {
              node(id)
              declareNodeAttrs(id)
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
              val tags    = project.config.tags
              val reqTags = project.reqTagsFn(tagGroupId, filterDead)
              val reqIds  = scope.fold(project.content.reqs.idIterator())(_.iterator)
              reqIds.map { reqId =>
                val colours =
                  reqTags(reqId)
                    .iterator
                    .map(tags.needApplicableTag)
                    .map(t => t.colour.getOrElse(Colour.tagDefault).live(t.live))
                    .to(ArraySeq)
                reqId -> colours
              }.toMap
            }

            def declareNode(id: ReqId): Unit = {
              node(id)
              declareNodeAttrs(id)
              live(id) match {
                case Live =>
                  val colours = coloursByReqId.get(id).filter(_.nonEmpty).getOrElse(ArraySeq(Colour.tagDefault))
                  val style =
                    if (colours.length == 1) {
                      // style=wedged requires at least 2 colours
                      val c = colours(0)
                      s"""[$styleFilled fillcolor="${c.`#rrggbb`}" fontcolor="${c.foreground.`#rrggbb`}"]"""
                    } else {
                      val fill = colours.iterator.map(_.`#rrggbb`).mkString(":")
                      val font = Colour.chooseForegroundOverMultipleBackgroundColours(colours)
                      s"""[$styleMultiColour fillcolor="$fill" fontcolor="${font.`#rrggbb`}"]"""
                    }
                  b.append(style)
                case Dead =>
                  deadNodeStyle()
              }
            }

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
      styleSubsequentNodesAsImplications(shape)
      b.append(s"""edge[color="$blackish"]""")

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
