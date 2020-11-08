package shipreq.webapp.client.ww.graph

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import nyaya.util.Multimap
import shipreq.base.util.OptionalBoolFn
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview.ImpGraphConfig.{Colours, LabelFormat}
import shipreq.webapp.member.project.text.PlainText

abstract class AbstractGraph(protected val project   : Project,
                             protected val filterDead: FilterDead) {
  import AbstractGraph._
  import GraphViz.Builder

  protected def scope: Option[Set[ReqId]] =
    None

  protected def create()(implicit b: Builder): Unit

  final def dot: GraphViz.DOT =
    GraphViz.digraph(create()(_))

  @inline final def svg(implicit g: GraphViz) =
    g.render(dot)

  // -------------------------------------------------------------------------------------------------------------------

  protected final val reqTypes       = project.config.reqTypes
  protected final val reqs           = project.content.reqs
  protected final val imps           = project.content.implications
  protected final val live           = Memo[ReqId, Live](reqs.need(_).live(reqTypes))
  protected final val reqIdFilter    = OptionalBoolFn[ReqId](scope.map(s => s.contains _))
  protected final val reqIdSetFilter = reqIdFilter.setFilter
  protected final val reqFilter      = reqIdFilter.contramap[Req](_.id)

  protected final lazy val reqIdsSortedByPubId =
    Project.reqIdsSortedByPubId(reqs, reqTypes)

  protected final object LabelFormatter {
    val pubid: ReqId => String = PlainText.pubidByReqId(_, reqs, reqTypes)

    def apply(f: LabelFormat, pt: PlainText.ForProject.NoCtx): LabelFormatter = f match {

      case LabelFormat.Pubid =>
        pubid

      case LabelFormat.PubidAndTitle =>
        id => {
          val p = pubid(id)
          val t = pt.reqTitleWithoutMarkupById(id)
          val l = p + "\n" + t
          WrapText(l, maxLabelWidth)
        }
    }
  }

  protected final val nodeId: ReqId => String =
    _.value.toString

  protected final val nodeIdAttr =
    LabelFormatter.pubid

  protected final def edgeStyleDead()(implicit b: Builder): Unit =
    b append """[color="#bbbbbb" style=dashed]"""

  protected final def nodeStyleDead()(implicit b: Builder): Unit =
    b append """[fillcolor="#dddddd" color="#777777" fontcolor="#666666"]"""

  @inline protected final def nodeStyleLiveOrDead(live: Live)(implicit b: Builder): Unit =
    if (live is Dead) nodeStyleDead()

  protected final def subsequentNodesStyleAsImplications(shape: Shape)(implicit b: Builder): Unit =
    b append s"""node[${shape.styleFilled} ${shape.declareShape} color="$blackish"]"""

  protected final def nodeDeclare(id: ReqId)(implicit b: Builder): Unit =
    b.append(nodeId(id))

  protected final def nodeAddIdAndLabel(id: ReqId)(implicit b: Builder, f: LabelFormatter): Unit = {
    b.append('[')
    b.setIdAttr(nodeIdAttr(id))
    b.append(' ')
    b.setLabel(f(id))
    b.append(']')
  }
}

// =====================================================================================================================

object AbstractGraph {

  sealed trait Shape {
    def declareShape: String
    def style_=(s: String): String

    val styleFilled =
      style_=("filled")

    val styleMultiColour =
      style_=(this match {
        case Shape.Ellipse => "wedged"
        case Shape.Box     => "striped"
      })
  }

  object Shape {
    case object Ellipse extends Shape {
      override def declareShape: String =
        "shape=ellipse"
      override def style_=(s: String): String =
        "style=" + s
    }

    case object Box extends Shape {
      override def declareShape: String =
        "shape=box"
      override def style_=(s: String): String =
        s"""style="rounded,$s""""
    }

    def apply(f: LabelFormat): Shape =
      f match {
        case LabelFormat.Pubid         => Shape.Ellipse
        case LabelFormat.PubidAndTitle => Shape.Box
      }
  }

  // ===================================================================================================================

  private[AbstractGraph] val maxLabelWidth = CharWidths('m') * 28

  type LabelFormatter = ReqId => String

  type Thunk = () => Unit

  trait ColourProvider {
    def declareNode(id: ReqId): Unit
    def declareAllReqsInScope(): Unit
  }

  object ColourProvider {

    trait WithoutScope {
      def apply(scope: Option[Set[ReqId]]): ColourProvider
    }

    def apply(configColours: Colours,
              graph        : AbstractGraph)
             (implicit b   : GraphViz.Builder,
              shape        : Shape,
              lf           : LabelFormatter): WithoutScope = {

      import graph.{filterDead, live, project, reqIdsSortedByPubId, reqs, reqTypes}

      configColours match {

        // =============================================================================================================
        case Colours.ByReqType =>

          // Add to reqTypesWithReqs regardless of live status so that colours don't change when user toggles the
          // FilterDead setting. Colours jumping around it's a needless cognitive burden when you're trying to analyse
          // the graph.
          val reqTypesWithReqs: Map[ReqTypeId, Int] =
            MutableArray(reqs.reqsByType.keys)
              .sortBy(reqTypes.order) // Deterministic (and stable until config changes) order of colours
              .iterator()
              .zipWithIndex
              .toMap

          val colourFn =
            DistinctColours("ffffff", reqTypesWithReqs.size, "ffffff")

          new WithoutScope {
            override def apply(scope: Option[Set[ReqId]]): ColourProvider = {
              val reqIdFilter = OptionalBoolFn[ReqId](scope.map(s => s.contains _))
              val reqFilter = reqIdFilter.contramap[Req](_.id)

              lazy val reqsByReqType: Multimap[ReqTypeId, Vector, Req] =
                reqFilter.value match {
                  case Some(f) =>
                    var m = reqs.reqsByType
                    for (rt <- m.keys)
                      m = m.mod(rt, _.filter(f))
                    m
                  case None =>
                    reqs.reqsByType
                }

              @inline def nodeData =
                MutableArray(reqsByReqType.iterator)
                  .sortBy(x => reqTypes.order(x._1))
                  .iterator()

              new ColourProvider {
                private def declareNodeWithoutColour(id: ReqId): Unit = {
                  graph.nodeDeclare(id)
                  graph.nodeAddIdAndLabel(id)
                  graph.nodeStyleLiveOrDead(live(id))
                }

                private def nodeStyle(reqType: ReqTypeId): Unit = {
                  val color = colourFn(reqTypesWithReqs(reqType))
                  b append """[fillcolor="#"""
                  b append color
                  b append """"]"""
                }

                override def declareAllReqsInScope(): Unit =
                  for ((reqType, reqs) <- nodeData) {
                    b append "node"
                    nodeStyle(reqType)
                    for (req <- reqs)
                      declareNodeWithoutColour(req.id)
                  }

                override def declareNode(id: ReqId): Unit = {
                  declareNodeWithoutColour(id)
                  val reqType = reqs.need(id).reqTypeId
                  nodeStyle(reqType)
                }
              }
            }
          }

        // =============================================================================================================
        case Colours.ByTag(tagGroupId) =>
          new WithoutScope {
            override def apply(scope: Option[Set[ReqId]]): ColourProvider = {
              val reqIdFilter = OptionalBoolFn[ReqId](scope.map(s => s.contains _))

              val coloursByReqId: Map[ReqId, ArraySeq[Colour]] = {
                val tags    = project.config.tags
                val reqTags = project.virtualTags.underTagGroup(tagGroupId, filterDead)
                val reqIds  = scope.fold(reqs.idIterator())(_.iterator)
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

              new ColourProvider {
                override def declareNode(id: ReqId): Unit = {
                  graph.nodeDeclare(id)
                  graph.nodeAddIdAndLabel(id)
                  live(id) match {
                    case Live =>
                      val colours = coloursByReqId.get(id).filter(_.nonEmpty).getOrElse(ArraySeq(Colour.tagDefault))
                      val style =
                        if (colours.length == 1) {
                          // style=wedged requires at least 2 colours
                          val c = colours(0)
                          s"""[${shape.styleFilled} fillcolor="${c.`#rrggbb`}" fontcolor="${c.foreground.`#rrggbb`}"]"""
                        } else {
                          val fill = colours.iterator.map(_.`#rrggbb`).mkString(":")
                          val font = Colour.chooseForegroundOverMultipleBackgroundColours(colours)
                          s"""[${shape.styleMultiColour} fillcolor="$fill" fontcolor="${font.`#rrggbb`}"]"""
                        }
                      b.append(style)
                    case Dead =>
                      graph.nodeStyleDead()
                  }
                }

                override def declareAllReqsInScope(): Unit =
                  for (reqId <- reqIdFilter.iterator(reqIdsSortedByPubId.iterator))
                    declareNode(reqId)
              }
            }
          }

        // =============================================================================================================
      }
    }
  }

  /** Declaration of node(s), and flow(s). */
  final case class DeclAndFlow[D, F](decl: D, flow: F) {
    def bimap[DD, FF](d: D => DD, f: F => FF) =
      DeclAndFlow(d(decl), f(flow))
  }

  final val blackish = "#222222"

  /** Traverses a graph processing each node once.
    *
    * Root nodes are also processed.
    *
    * @param process 1. Mutate some external state to record the A.
    *                2. Return the argument node's child-nodes.
    */
  def mutableGraphTraversal[A](roots: Set[A])(process: A => Set[A]): Unit = {
    @tailrec
    def go(queue: List[A], queueNext: Set[A], seen: Set[A]): Unit =
      queue match {
        case Nil =>
          if (queueNext.nonEmpty)
            go(queueNext.toList, Set.empty, seen)

        case fromId :: queue2 =>
          if (seen.contains(fromId))
            go(queue2, queueNext, seen)
          else {
            val toIds = process(fromId)
            go(queue2, queueNext ++ toIds, seen + fromId)
          }
      }
    go(Nil, roots, Set.empty)
  }
}