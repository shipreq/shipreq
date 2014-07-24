import org.scalajs.dom
import org.scalajs.dom.console
import scala.scalajs.js
import scalaz.{State, StateT, Scalaz, Bind}
import scalaz.syntax.bind._
import scalaz.effect.IO
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._
import japgolly.scalajs.react.ScalazReact._

object DND {

  def move[A](from: A, to: A, cmp: (A, A) => Boolean)(l: List[A]): List[A] = {
    console.log(s"DND Move: $from ⇒ $to") // TODO del
    l.find(cmp(from, _)) match {
      case None => l
      case Some(f) =>
        l.flatMap(i => {
          var x = if (cmp(from, i)) Nil else i :: Nil
          if (cmp(to, i)) x = x :+ f
          x
        })
    }
  }

  object Parent {
    type PState[A] = Option[(A, Option[A])] // src & target

    def initialState[A]: PState[A] = None

    // optimise (many needless state changes)
    private def setStateDrop[A](s: Option[A]): State[PState[A], Unit] = State.modify(_.map(x => (x._1, s)))

    def dragEnd[A] = State.put[PState[A]](None)
    def dragStart[A](a: A) = State.put[PState[A]](Some(a, None))

    def dragOver[A](a: A) = setStateDrop(Some(a))
    def dragLeave[A] = setStateDrop[A](None)

    def cProps[A](T: ComponentStateFocus[PState[A]], a: A, cmp: (A, A) => Boolean, move: (A,A) => IO[Unit]) =
      Child.CProps[A](
        T.state match {
          case Some((_, Some(d))) => cmp(a, d)
          case _ => false
        },
        T _runStateIO dragStart,
        T _runStateIO dragOver,
        T runStateIO dragLeave,
        T runStateIO dragEnd,
        T.state match {
          case Some((from, Some(to))) => move(from, to)
          case _ => IO(())
        }
      )
  }

  object Child {
    case class CProps[A](dragover: Boolean,
                         onDragStart: A => IO[Unit],
                         onDragOver: A => IO[Unit],
                         onDragLeave: IO[Unit],
                         onDragEnd: IO[Unit],
                         onMove: IO[Unit])
    type CState = Boolean

    type StateIO[A] = StateT[IO, CState, A]

    def initialState: CState = false

    def dragStart[A](a: A, p: CProps[A]): SyntheticEvent[dom.Node] => StateIO[Unit] =
      e => StateT(_ => p.onDragStart(a) >> IO {
        //console.log(s"dragStart: $p")
        e.dragEvent.get.dataTransfer.setData("text", "managed")
        (true, ())
      })

    def dragEnd[A](p: CProps[A]): StateIO[Unit] =
      StateT(_ => p.onDragEnd >> IO(false, ()))

    def dragOver[A](a: A, p: CProps[A], s: => CState): SyntheticEvent[dom.Node] => IO[Unit] =
      e => IO {
        //console.log(s"dragOver: dragging = $s / dragover = ${p.dragover}")
        if (!s) {
          e.preventDefault()
          e.dragEvent.get.dataTransfer.asInstanceOf[js.Dynamic].updateDynamic("dropEffect")("move")
          p.onDragOver(a).unsafePerformIO()
        }
      }

    def drop[A](p: CProps[A]): SyntheticEvent[dom.Node] => IO[Unit] =
      _.preventDefaultIO >> p.onMove

    def renderDragHandle[S, A](p: CProps[A], a: A, T: ComponentStateFocus[CState]) =
      span(
        className     := "draghandle"
        ,draggable    := "true"
        ,onDragStart ~~> T._runStateIO(dragStart(a, p))
        ,onDragEnd   ~~> T.runStateIO(dragEnd(p))
        // onMouseDown={typeof window.isIE9 != 'undefined' && this.handleIE9DragHack}
      )("\u2630")

    def renderRow[A](p: CProps[A], a: A, T: ComponentStateFocus[CState]) =
      div(
        classSet("dragging" -> T.state, "dragover" -> p.dragover)
        ,onDragEnter ~~> preventDefaultIO
        ,onDragOver  ~~> dragOver(a, p, T.state)
        ,onDragLeave ~~> p.onDragLeave
        ,onDrop      ~~> drop(p)
      )

    def dndItemComponent[A](r: (A, Tag) => Modifier ) = ReactComponentB[(A, DND.Child.CProps[A])]("DndItem")
      .initialState(DND.Child.initialState)
      .render(T => {
      val (i,p) = T.props
      DND.Child.renderRow(p, i, T)(
        r(i, DND.Child.renderDragHandle(p, i, T))
      )
    }).create
  }
}