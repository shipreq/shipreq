package shipreq.webapp.client.util.ui

import scalaz.effect.IO
import scalaz.syntax.bind._
import scalaz.Isomorphism.<=>
import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import Util._
import Editor.Error

trait Editor[D, V] {
  def renderRW[S](data     : D,
                  error    : Error,
                  onChange : D => ReactST[IO, S, Unit],
                  onCancel : IO[Unit] => ReactST[IO, S, Unit],
                  onEditEnd: ReactST[IO, S, Unit],
                  T        : ComponentStateFocus[S]): V

  def renderRO[S](data: D, T: ComponentStateFocus[S]): V

  final def map[E](iso: D <=> E): Editor[E, V] =
    new Editor.Mapped(iso, this)
}

// ---------------------------------------------------------------------------------------------------------------------
object Editor {
  type Error = Option[String]

  final class Mapped[A, B, V](iso: A <=> B, underlying: Editor[A, V]) extends Editor[B, V] {
    override def renderRW[S](data     : B,
                             error    : Error,
                             onChange : B => ReactST[IO, S, Unit],
                             onCancel : IO[Unit] => ReactST[IO, S, Unit],
                             onEditEnd: ReactST[IO, S, Unit],
                             T        : ComponentStateFocus[S]) = {
      val data2 = iso from data
      val onChange2 = (a: A) => onChange(iso to a)
      underlying.renderRW(data2, error, onChange2, onCancel, onEditEnd, T)
    }
    override def renderRO[S](data: B, T: ComponentStateFocus[S]): V = {
      val data2 = iso from data
      underlying.renderRO(data2, T)
    }
  }
}

// =====================================================================================================================
object Editors {

  class TextEditor(node: Tag) extends Editor[String, Modifier] {
    override def renderRW[S](data     : String,
                             error    : Error,
                             onChange : String => ReactST[IO, S, Unit],
                             onCancel : IO[Unit] => ReactST[IO, S, Unit],
                             onEditEnd: ReactST[IO, S, Unit],
                             T        : ComponentStateFocus[S]) = {

      val cancelOnEscape: ReactKeyboardEventH => ReactST[IO, S, Unit] =
        e => e.key match {
          case "Escape" => // TODO use KeyValue
            val t = e.target
            ReactS.retM[IO, S, Unit](e.preventDefaultIO >> e.stopPropagationIO) >> onCancel(IO(t.blur()))
          case _ =>
            ReactS.retT[IO, S, Unit](())
        }

      div(
        node(
          value := data,
          error.isDefined && (cls := "error"),
          onchange ~~> T._runState(textChangeRecv(onChange)),
          onkeydown ~~> T._runState(cancelOnEscape),
          onblur ~~> T.runState(onEditEnd)),
        error.fold(EmptyTag)(e => div(cls := "errorMsg", e)))
    }

    override def renderRO[S](data: String, T: ComponentStateFocus[S]) =
      div(node(value := data, readonly := true))
  }

  val TextInputEditor = new TextEditor(input)
  val TextareaEditor = new TextEditor(textarea)

  // -------------------------------------------------------------------------------------------------------------------
  object CheckboxEditor extends Editor[Boolean, Modifier] {
    override def renderRW[S](data     : Boolean,
                             error    : Error,
                             onChange : Boolean => ReactST[IO, S, Unit],
                             onCancel : IO[Unit] => ReactST[IO, S, Unit],
                             onEditEnd: ReactST[IO, S, Unit],
                             T        : ComponentStateFocus[S]) = {

      def ch(e: InputEvent) =
        onChange(e.target.checked) >> onEditEnd

      div(
        checkbox(data)(onchange ~~> T._runState(ch)),
        error.isDefined && (cls := "error"),
        error.fold(EmptyTag)(e => div(cls := "errorMsg", e)))
    }

    override def renderRO[S](data: Boolean, T: ComponentStateFocus[S]) =
      div(checkbox(data)(readonly := true))
  }
}