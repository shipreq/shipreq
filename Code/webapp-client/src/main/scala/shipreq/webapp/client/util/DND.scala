package shipreq.webapp.client.util

import org.scalajs.dom
import org.scalajs.dom.console
import scala.scalajs.js
import scalaz._
import scalaz.syntax.bind.ToBindOps
import scalaz.effect.IO
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._

/**
 * Usage.
 *
 * 1. Include in parent component state: DND.Parent.PState[A]
 * 2. Initialise parent component state using DND.Parent.initialState[A]
 * 3. Create component for draggable children using DND.Child.dndItemComponent[A]
 * 4. Prepare a DND success callback `move(from: A, to: A): IO[Unit]`
 * 5. Render draggable children using DND.Parent.cProps2
 */
object DND {

  def move[A](from: A, to: A)(as: Vector[A])(implicit E: Equal[A]): Vector[A] = {
    var result = Vector.empty[A]
    var finding = true
    as.foreach { a =>
      if (finding && E.equal(from, a))
        finding = false
      else {
        val e = E.equal(to, a)
        if (e && finding) result :+= from
        result :+= a
        if (e && !finding) result :+= from
      }
    }
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
          override def equal(a: PState[A], b: PState[A]): Boolean = (a, b) match {
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

    def eventHandler[M[_]: Applicative, A](moveFn: (A, A) => M[Unit]): DragEvent[A] => ReactST[M, PState[A], Unit] = {
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

    def cProps[M[_], A](c: ComponentStateFocus[PState[A]], a: A, moveFn: (A, A) => M[Unit])
                       (implicit A: Applicative[M], M: M ~> IO, E: Equal[A]): Child.CProps[A] =
      Child.CProps(
        c.state match {
          case Possible(_, tgt) => E.equal(a, tgt)
          case _                => false
        },
        c _runStateF eventHandler(moveFn))

    def cProps2[M[_], A](c: ComponentStateFocus[PState[A]], a: A, moveFn: (A, A) => M[Unit])
                        (implicit A: Applicative[M], M: M ~> IO, E: Equal[A]): (A, Child.CProps[A]) =
      (a, cProps(c, a, moveFn))
  }

  // ===================================================================================================================
  object Child {
    case class CProps[A](dragover: Boolean, eventHandler: DragEvent[A] => IO[Unit])
    type CState = Boolean

    val ST = ReactS.FixT[IO, Boolean]
    type ST = ST.T[Unit]

    val setStateT = ST setT true
    val setStateF = ST setT false

    def initialState: CState = false

    def dragStart[A](a: A, p: CProps[A]): ReactDragEvent => ST =
      e => {
        val io1 = p.eventHandler(DragEvent.Start(a))
        val io2 = IO[Unit]{ e.dataTransfer.setData("text", "managed") }
        ST.ret(io1 >> io2) >> setStateT
      }

    def dragEnd[A](p: CProps[A]): ST =
      ST.ret(p.eventHandler(DragEvent.End)) >> setStateF

    def dragOver[A](a: A, p: CProps[A], s: => CState): ReactDragEvent => IO[Unit] =
      e => IO {
        //console.log(s"dragOver: dragging = $s / dragover = ${p.dragover}")
        if (!s) {
          e.preventDefault()
          e.dataTransfer.dropEffect = "move"
          p.eventHandler(DragEvent.Over(a)).unsafePerformIO()
        }
      }

    def drop[A](p: CProps[A]): ReactDragEvent => IO[Unit] =
      _.preventDefaultIO >> p.eventHandler(DragEvent.Move)

    def renderDragHandle[S, A](p: CProps[A], a: A, T: ComponentStateFocus[CState]): ReactTag =
      <.span(
        ^.className    := "draghandle",
        ^.draggable    := "true",
        ^.onDragStart ~~> T._runState(dragStart(a, p)),
        ^.onDragEnd   ~~> T.runState(dragEnd(p)),
        // onMouseDown={typeof window.isIE9 != 'undefined' && this.handleIE9DragHack}
        "\u2630")

    def outerAttrs[A](p: CProps[A], a: A, state: CState): TagMod = (
      ^.classSet("dragging" -> state, "dragover" -> p.dragover)
        + (^.onDragEnter ~~> preventDefaultIO)
        + (^.onDragOver  ~~> dragOver(a, p, state))
        + (^.onDragLeave ~~> p.eventHandler(DragEvent.Leave))
        + (^.onDrop      ~~> drop(p))
      )

    def dndItemComponent[A](f: (TagMod, ReactTag, A) => ReactElement) =
      ReactComponentB[(A, DND.Child.CProps[A])]("DndItem")
        .initialState(DND.Child.initialState)
        .render { c =>
          val (a, p) = c.props
          f(outerAttrs(p, a, c.state), renderDragHandle(p, a, c), a)
        }.build

  }
}
