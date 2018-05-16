package shipreq.webapp.client.project.lib

import scalaz._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._, vdom.html_<^._, ScalazReact._
import shipreq.base.util.Util

/**
 * Usage.
 *
 * 1. Include in parent component state: DND.Parent.PState[A]
 * 2. Initialise parent component state using DND.Parent.initialState[A]
 * 3. Create component for draggable children using DND.Child.dndItemComponent[A]
 * 4. Prepare a DND success callback `move(from: A, to: A): Callback`
 * 5. Render draggable children using DND.Parent.cProps2
 */
object DND { // TODO Remove? DragToReorder makes this redundant?

  def moveE[A](from: A, to: A)(as: Vector[A])(implicit e: Equal[A]): Vector[A] =
    move(from, to, as)(e.equal)

  def move[A, B](from: A, to: A, bs: Vector[B])(equal: (A, B) => Boolean): Vector[B] = {
    var tmp = bs.companion.newBuilder[B]
    var putLater = -1
    var fromB: Option[B] = None
    var i = 0
    bs.foreach { b =>
      if (fromB.isEmpty && equal(from, b)) {
        if (equal(to, b))
          return bs // nothing to do
        fromB = Some(b)
      } else {
        tmp += b
        if (equal(to, b)) {
          fromB match {
            case None =>
              putLater = i
              tmp += b // This is the correct b, we will replace this-1
            case Some(ins) =>
              tmp += ins
          }
          i += 1
        }
        i += 1
      }
    }
    val tmp2 = tmp.result()
    val result =
      if (putLater == -1)
        tmp2
      else fromB match {
        case Some(b) => tmp2.updated(putLater, b)
        case None    => tmp2.delete(putLater).getOrElse(tmp2)
      }
    assert(result.size == bs.size, s"DND Move failure.\nBefore: $bs\n After: $result")
    result
  }

  sealed trait DragEvent[+A]
  object DragEvent {
    case class  Start[A](a: A) extends DragEvent[A]
    case class  Over [A](a: A) extends DragEvent[A]
    case object Leave          extends DragEvent[Nothing]
    case object End            extends DragEvent[Nothing]
    case object Move           extends DragEvent[Nothing]
  }

  // ===================================================================================================================
  object Parent {

    sealed trait PState[+A]
    object PState {
      case object Inactive                    extends PState[Nothing]
      case class  Started [A](src: A)         extends PState[A]
      case class  Possible[A](src: A, tgt: A) extends PState[A]

      implicit def equality[A](implicit E: Equal[A]): Equal[PState[A]] =
        new Equal[PState[A]] {
          override def equalIsNatural = E.equalIsNatural
          override def equal(sa: PState[A], sb: PState[A]): Boolean = (sa, sb) match {
            case (Inactive,       Inactive      ) => true
            case (Started(a),     Started(b)    ) => E.equal(a, b)
            case (Possible(a, c), Possible(b, d)) => E.equal(a, b) && E.equal(c, d)
            case _                                => false
          }
        }
    }

    import PState._

    def initialState[A]: PState[A] = PState.Inactive

    def setTarget[A](tgt: A): PState[A] => PState[A] = {
      case Started(s)     => Possible(s, tgt)
      case Possible(s, _) => Possible(s, tgt)
      case Inactive       => Inactive
    }

    def clearTarget[A]: PState[A] => PState[A] = {
      case p@ Started(_)  => p
      case Possible(s, _) => Started(s)
      case Inactive       => Inactive
    }

    implicit def changeFilter[A: Equal] = ChangeFilter.equal[PState[A]]

    def eventHandler[M[_]: Monad, A](moveFn: (A, A) => M[Unit]): DragEvent[A] => ReactST[M, PState[A], Unit] = {
      val ST = ReactS.FixT[M, PState[A]]
      val nop = Applicative[M].point(())
      (event: DragEvent[A]) => event match {
        case DragEvent.Start(a) => ST setT PState.Started(a)
        case DragEvent.End      => ST setT PState.Inactive
        case DragEvent.Over(a)  => ST modT setTarget(a)
        case DragEvent.Leave    => ST modT clearTarget
        case DragEvent.Move     =>
          ST.gets {
            case Possible(s, t) => moveFn(s, t)
            case _              => nop
          }
      }
    }

    def cProps[A]($: StateAccessPure[PState[A]], a: A, moveFn: (A, A) => Callback)(implicit E: Equal[A]): Child.CProps[A] =
      Child.CProps(
        $.state.runNow() match {
          case Possible(_, tgt) => E.equal(a, tgt)
          case _                => false
        },
        $ runStateFnF eventHandler(moveFn))

    def cProps2[M[_], A]($: StateAccessPure[PState[A]], a: A, moveFn: (A, A) => Callback)(implicit E: Equal[A]): (A, Child.CProps[A]) =
      (a, cProps($, a, moveFn))
  }

  // ===================================================================================================================
  object Child {
    case class CProps[A](dragover: Boolean, eventHandler: DragEvent[A] => Callback)
    type CState = Boolean

    val ST = ReactS.FixCB[Boolean]
    type ST = ST.T[Unit]

    val setStateT = ST setT true
    val setStateF = ST setT false

    def initialState: CState = false

    def dragStart[A](a: A, p: CProps[A]): ReactDragEvent => ST =
      e => {
        val io1 = p.eventHandler(DragEvent.Start(a))
        val io2 = Callback{ e.dataTransfer.setData("text", "managed"); () }
        ST.ret(io1 >> io2) >> setStateT
      }

    def dragEnd[A](p: CProps[A]): ST =
      ST.ret(p.eventHandler(DragEvent.End)) >> setStateF

    def dragOver[A](a: A, p: CProps[A], s: => CState): ReactDragEvent => Callback =
      e => Callback {
        //console.log(s"dragOver: dragging = $s / dragover = ${p.dragover}")
        if (!s) {
          e.preventDefault()
          e.dataTransfer.dropEffect = "move"
          p.eventHandler(DragEvent.Over(a)).runNow()
        }
      }

    def drop[A](p: CProps[A]): ReactDragEvent => Callback =
      _.preventDefaultCB >> p.eventHandler(DragEvent.Move)

    def renderDragHandle[S, A](p: CProps[A], a: A, $: StateAccessPure[CState]): VdomTag =
      <.span(
        ^.className    := "draghandle",
        ^.draggable    := true,
        ^.onDragStart ==> $.runStateFn(dragStart(a, p)),
        ^.onDragEnd   --> $.runState(dragEnd(p)),
        // onMouseDown={typeof window.isIE9 != 'undefined' && this.handleIE9DragHack}
        "\u2630")

    def outerAttrs[A](p: CProps[A], a: A, state: CState): TagMod =
      TagMod(
        ^.classSet("dragging" -> state, "dragover" -> p.dragover),
        ^.onDragEnter ==> ((_: ReactEvent).preventDefaultCB),
        ^.onDragOver  ==> dragOver(a, p, state),
        ^.onDragLeave --> p.eventHandler(DragEvent.Leave),
        ^.onDrop      ==> drop(p))

    def dndItemComponent[A](f: (TagMod, VdomTag, A) => VdomElement) =
      ScalaComponent.builder[(A, DND.Child.CProps[A])]("DndItem")
        .initialState(DND.Child.initialState)
        .renderPS { ($, props, s) =>
          val (a, p) = props
          f(outerAttrs(p, a, s), renderDragHandle(p, a, $.mountedPure), a)
        }.build

    def dndItemComponentB[A, B](f: (TagMod, VdomTag, A, B) => VdomElement) =
      ScalaComponent.builder[(A, DND.Child.CProps[A], B)]("DndItem")
        .initialState(DND.Child.initialState)
        .renderPS { ($, props, s) =>
          val (a, p, b) = props
          f(outerAttrs(p, a, s), renderDragHandle(p, a, $.mountedPure), a, b)
        }.build
  }
}
