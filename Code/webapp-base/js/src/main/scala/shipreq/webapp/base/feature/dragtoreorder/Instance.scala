package shipreq.webapp.base.feature.dragtoreorder

import japgolly.microlibs.utils.Memo
import japgolly.univeq._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.raw.DragEffect
import shipreq.webapp.base.feature.DragToReorderFeature
import shipreq.webapp.base.feature.DragToReorderFeature.Item
import shipreq.webapp.base.lib.DomUtil
import shipreq.webapp.base.util.Reorder

object Instance {
  var instanceCount = 0
  var dragging = List.empty[Instance[_]]
}

private[feature] final class Instance[A](getData            : CallbackTo[Vector[A]],
                                         updateData         : Vector[A] => Callback,
                                         updateUI           : Callback,
                                         dragOutsideToRemove: Boolean,
                                         addKeysToChildren  : Boolean,
                                        ) extends DragToReorderFeature[A] { self =>

  private object Internals {
    type State = Option[DragState[A]]

    private var _state: State =
      None

    def unsafeState(): State =
      _state

    // This prevents the need to check e.dataTransfer.types
    val getDragState: CallbackOption[DragState[A]] =
      CallbackOption.liftOption(_state)

    def setState(s: State): Callback =
      Callback { _state = s } >> updateUI

    @inline def setState(s: DragState[A]): Callback =
      setState(Some(s))

    val ownMimeType = {
      import Instance.instanceCount
      instanceCount += 1
      "application/shipreq/dnd/" + instanceCount
    }

    def requireOwnMimeType(e: ReactDragEvent) =
      CallbackOption.require(e.dataTransfer.types.contains(ownMimeType))

    def detectIfDraggedOutside(expectedLoc: Option[DragLoc])(e: ReactDragEvent): Callback =
      for {
        _ ← requireOwnMimeType(e)
        s ← getDragState
      //_ ← Callback.log(s"   Leave check... e.client: ${e.clientX}x${e.clientY}, e.screen: ${e.screenX}x${e.screenY} node:", e.currentTarget.castHtml.getBoundingClientRect())
        _ ← CallbackOption.require(expectedLoc.forall(_ ==* s.dragLoc))
        _ ← CallbackOption.unless(DomUtil.isDragWithinNode(e, e.currentTarget))
        _ ← setState(s.copy(dragLoc = DragLoc.Outside, currentOrder = s.originalOrder))
      } yield ()

    val dragOver: ReactDragEvent => Callback =
      e => for {
        _ ← CallbackOption.unless(e.defaultPrevented)
        _ ← requireOwnMimeType(e)
        _ ← getDragState
        _ ← e.preventDefaultCB
      } yield ()

    // Consume the event (so that Firefox doesn't perform a cancel animation)
    // and do nothing (because the move is performed in onDragEnd so that drags outside work without the outside needing
    // an onDrop handler).
    val drop: ReactDragEvent => Callback =
      e => for {
        _ ← CallbackOption.unless(e.defaultPrevented)
        _ ← requireOwnMimeType(e)
        _ ← e.preventDefaultCB
      } yield ()

    val parentTagMod: TagMod = {
      val dragEnter: ReactDragEvent => Callback =
        e => for {
          _ ← CallbackOption.unless(e.defaultPrevented)
          _ ← requireOwnMimeType(e)
          s ← getDragState
          _ ← e.preventDefaultCB
          _ ← setState(s.copy(dragLoc = DragLoc.InParent))
        } yield ()

      val dragLeave: ReactDragEvent => Callback =
        detectIfDraggedOutside(None)

      TagMod(
        ^.onDragEnter ==> dragEnter,
        ^.onDragLeave ==> dragLeave,
        ^.onDragOver  ==> dragOver,
        ^.onDrop      ==> drop)
    }

    val dragSourceTagMods: Int => TagMod =
      Memo.int { i =>
        def dragStart: ReactDragEvent => Callback =
          e => for {
            _  ← CallbackOption.unless(e.defaultPrevented)
            is ← getData.toCBO
            _  ← setState(DragState(is, i, DragLoc.InChild(i), is.indices.toVector)).async.toCallback
            _  ← Callback { Instance.dragging ::= self }.toCBO
          } yield {
            val dt = e.dataTransfer
            dt.setData(ownMimeType, "")
            dt.effectAllowed = DragEffect.Move
          }

        def dragEnd: ReactDragEvent => Callback =
          _ => for {
            s ← getDragState
            _ ← setState(None)
            _ ← Callback { Instance.dragging = Instance.dragging.filter(_ ne self) }.toCBO
            o = s.orderWithoutTombstone
            _ ← CallbackOption.unless(o.length < s.items.length && !dragOutsideToRemove)
            _ ← CallbackOption.unless(o ==* s.originalOrder)
            _ ← updateData(o map s.items.apply)
          } yield ()

        val t =
          TagMod(
            ^.draggable    := true,
            ^.onDragStart ==> dragStart,
            ^.onDragEnd   ==> dragEnd,
            ^.onDrop      ==> drop)

        if (addKeysToChildren)
          TagMod(t, ^.key := i)
        else
          t
      }

    val dropTargetTagMods: Int => TagMod =
      Memo.int { i =>

        def dragEnter: ReactDragEvent => Callback =
          e => for {
            _ ← CallbackOption.unless(e.defaultPrevented)
            _ ← requireOwnMimeType(e)
            s ← getDragState
            _ ← e.preventDefaultCB
            _ ← Callback(e.dataTransfer.dropEffect = DragEffect.Move)
            l = DragLoc.InChild(i)
            _ ← CallbackOption.unless((s.dragSource ==* i) && (s.dragLoc ==* l))
            _ ← setState(s.copy(dragLoc = l, currentOrder = Reorder.usingUnivEq(s.dragSource, i)(s.currentOrder)))
          } yield ()

        val dragLeave: ReactDragEvent => Callback =
          detectIfDraggedOutside(Some(DragLoc.InChild(i)))

        TagMod(
          ^.onDragEnter ==> dragEnter,
          ^.onDragLeave ==> dragLeave,
          ^.onDragOver  ==> dragOver)
      }

    def mkItems(order: Iterable[Int], as: IndexedSeq[A], status: Int => Status): Vector[Item[A]] = {
      val v = Vector.newBuilder[Item[A]]
      for (i <- order)
        v += Item(as(i), dragSourceTagMods(i), dropTargetTagMods(i), status(i))
      v.result()
    }

    def createItems(p: IndexedSeq[A], s: State): Vector[Item[A]] =
      s match {
        case None =>
          mkItems(
            p.indices,
            p,
            _ => Status.Normal)

        case Some(ds) =>
          val onDragSrc = ds.dragLoc match {
            case DragLoc.Outside if dragOutsideToRemove => Status.Tombstone
            case _                                      => Status.DragSource
          }
          mkItems(
            ds.currentOrder,
            ds.items,
            i => if (i ==* ds.dragSource) onDragSrc else Status.Normal)
      }
  }

  // ===================================================================================================================
  import Internals._

  override val container: TagMod =
    Internals.parentTagMod

  override def dragInProgress(): Boolean =
    Instance.dragging.contains(self)

  override def items(): Vector[Item[A]] =
    items(getData.runNow())

  override def items(as: IndexedSeq[A]): Vector[Item[A]] = {
    assert(as.toVector == getData.runNow(),
      s"""
         |Items passed to DND don't match what getData can generate for itself.
         |
         |.items(): ${as.toVector}
         |
         | getData: ${getData.runNow()}
         |""".stripMargin.trim)
    createItems(as, unsafeState())
  }

}