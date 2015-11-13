package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.vdom.TagMod
import org.scalajs.dom.raw.DragEffect
import shipreq.base.util.{Memo, UnivEq, univEqOps}
import shipreq.base.util.UnivEq.Implicits._
import shipreq.webapp.client.util.DND
import shipreq.webapp.client.util.DomUtil._

object DragToReorder {

  sealed trait Status

  case object Normal extends Status

  /** Item is being dragged. You'd normally draw it with a dashed border, or `visibility: hidden`. */
  case object DragSource extends Status

  /**
   * Item has been dragged out to indicate deletion.
   *
   * You must still render a node for this, do not omit it because it will prevent expected events from firing.
   * Use `display: none` instead.
   */
  case object Tombstone extends Status

  case class Item[A](data: A, mod: TagMod, status: Status)

  case class Content[A](rootMod: TagMod, items: Vector[Item[A]])

  type Props[A] = Vector[A]

  /** Where the drag cursor is currently located. */
  private[DragToReorder] sealed trait DragLoc
  private[DragToReorder] case object Outside        extends DragLoc
  private[DragToReorder] case object InParent       extends DragLoc
  private[DragToReorder] case class InChild(i: Int) extends DragLoc

  private[DragToReorder] implicit def dragLocEquality: UnivEq[DragLoc] = UnivEq.deriveAuto

  private[DragToReorder] var instanceCount = 0
}

// =====================================================================================================================
final class DragToReorder[A](updateItems: Vector[A] => Callback,
                             renderFn   : DragToReorder.Content[A] => CallbackTo[ReactElement]) {
  import DragToReorder._

  type Item    = DragToReorder.Item [A]
  type Props   = DragToReorder.Props[A]
  type Content = DragToReorder.Content[A]

  case class DragState(items       : Vector[A],
                       dragSource  : Int,
                       dragLoc     : DragLoc,
                       currentOrder: Vector[Int]) {

    override def toString = s"DragState($dragSource, $dragLoc, $currentOrder)"

    def originalOrder = items.indices.toVector

    def orderWithoutTombstone: Vector[Int] =
      dragLoc match {
        case Outside               => currentOrder.filterNot(_ ==* dragSource)
        case InParent | InChild(_) => currentOrder
      }
  }

  type State = Option[DragState]

  final class Backend($: BackendScope[Props, State]) {
    type EH = ReactDragEvent => Callback

    import CallbackOption.{require, unless}

    val ownMimeType = {
      instanceCount += 1
      "application/null/" + instanceCount
    }

    @inline private implicit def autoSomeState(s: DragState): State = Some(s)

//    scalajs.js.timers.setInterval(2000)(println(s"~~ state = ${loc}"))
//    def loc = $.state.flatMap(_.map(_.dragLoc))
//    private def withLogging(ctx: Any, eh: EH): EH = {
//        e =>
//          Callback.byName(Callback.log(s"$ctx.${e.eventType} : ${e.target.nodeName} ⇒ ${e.currentTarget.nodeName}. DP=${e.defaultPrevented}. Loc=$loc")) >>
//          eh(e) >>
//          Callback.byName(Callback.log(s"              DP=${e.defaultPrevented}. Loc=$loc"))
//    }

    def requireOwnMimeType(e: ReactDragEvent) =
      require(e.dataTransfer.types.exists(ownMimeType == _))

    // This prevents the need to check e.dataTransfer.types
    val getDragState: CallbackOption[DragState] =
      $.state.asCBO

    val dragOver: EH =
      e => for {
        _ ← unless(e.defaultPrevented)
        _ ← requireOwnMimeType(e)
        _ ← getDragState
        _ ← e.preventDefaultCB
      } yield ()

    // Consume the event (so that Firefox doesn't perform a cancel animation)
    // and do nothing (because the move is performed in onDragEnd so that drags outside work without the outside needing
    // an onDrop handler).
    val drop: EH =
      e => for {
        _ ← unless(e.defaultPrevented)
        _ ← requireOwnMimeType(e)
        _ ← e.preventDefaultCB
      } yield ()

    def detectIfDraggedOutside(expectedLoc: Option[DragLoc]): EH =
      e => for {
        _ ← requireOwnMimeType(e)
        s ← getDragState
        //_ ← Callback.log(s"   Leave check... e.client: ${e.clientX}x${e.clientY}, e.screen: ${e.screenX}x${e.screenY} node:", e.currentTarget.castHtml.getBoundingClientRect())
        _ ← require(expectedLoc.forall(_ ==* s.dragLoc))
        _ ← unless(isDragWithinNode(e, e.currentTarget))
        _ ← $ setState s.copy(dragLoc = Outside, currentOrder = s.originalOrder)
      } yield ()

    val parentTagMod: TagMod = {
      val dragEnter: EH =
        e => for {
          _ ← unless(e.defaultPrevented)
          _ ← requireOwnMimeType(e)
          s ← getDragState
          _ ← e.preventDefaultCB
          _ ← $ setState s.copy(dragLoc = InParent)
        } yield ()

      val dragLeave: EH =
        detectIfDraggedOutside(None)

      TagMod(
        ^.onDragEnter ==> dragEnter,
        ^.onDragLeave ==> dragLeave,
        ^.onDragOver  ==> dragOver,
        ^.onDrop      ==> drop)
    }

    val childTagMod: Int => TagMod =
      Memo.int { i =>
        def dragStart: EH =
          e => for {
            _  ← unless(e.defaultPrevented)
            is ← $.props
            _  ← $.setState(DragState(is, i, InChild(i), is.indices.toVector)).async
          } yield {
            val dt = e.dataTransfer
            dt.setData(ownMimeType, "")
            dt.effectAllowed = DragEffect.Move
          }

        def dragEnd: EH =
          e => for {
            s ← getDragState
            p ← $.props.toCBO
            _ ← $ setState None
            o = s.orderWithoutTombstone
            _ ← unless(o ==* s.originalOrder)
            _ ← updateItems(o map s.items.apply)
          } yield ()

        def dragEnter: EH =
          e => for {
            _ ← unless(e.defaultPrevented)
            _ ← requireOwnMimeType(e)
            s ← getDragState
            _ ← e.preventDefaultCB
            _ ← Callback(e.dataTransfer.dropEffect = DragEffect.Move)
            l = InChild(i)
            _ ← unless((s.dragSource ==* i) && (s.dragLoc ==* l))
            _ ← $ setState s.copy(dragLoc = l, currentOrder = DND.moveE(s.dragSource, i)(s.currentOrder))
          } yield ()

        val dragLeave: EH =
          detectIfDraggedOutside(Some(InChild(i)))

        TagMod(
          ^.key          := i,
          ^.draggable    := true,
          ^.onDragStart ==> dragStart,
          ^.onDragEnd   ==> dragEnd,
          ^.onDragEnter ==> dragEnter,
          ^.onDragLeave ==> dragLeave,
          ^.onDragOver  ==> dragOver,
          ^.onDrop      ==> drop)
      }

    def render(p: Props, s: State): ReactElement = {
      def mkItems(order: Iterable[Int], as: Vector[A], status: Int => Status): Vector[Item] = {
        val v = Vector.newBuilder[Item]
        for (i <- order)
          v += Item(as(i), childTagMod(i), status(i))
        v.result()
      }

      val items: Vector[Item] =
        s match {
          case None =>
            mkItems(p.indices, p, _ => Normal)

          case Some(ds) =>
            val onDragSrc = ds.dragLoc match {
              case Outside               => Tombstone
              case InParent | InChild(_) => DragSource
            }
            mkItems(ds.currentOrder, ds.items, i => if (i ==* ds.dragSource) onDragSrc else Normal)
        }

      renderFn(Content(parentTagMod, items)).runNow()
    }
  }

  val Component = ReactComponentB[Props]("DND")
    .initialState[State](None)
    .renderBackend[Backend]
    .build
}
