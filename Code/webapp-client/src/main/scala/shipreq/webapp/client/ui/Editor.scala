package shipreq.webapp.client.ui

import scalaz.effect.IO
import scalaz.syntax.bind._
import scalaz.Isomorphism.<=>
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._
import japgolly.scalajs.react.ScalazReact._
import Util._
import Editor.Error

trait Editor[D, V] {
  def render[S](data: D,
                error: Error,
                onChange: D => ReactST[IO, S, Unit],
                onCancel: IO[Unit] => ReactST[IO, S, Unit],
                onEditEnd: ReactST[IO, S, Unit],
                T: ComponentStateFocus[S]): V

  final def map[E](iso: D <=> E): Editor[E, V] =
    new Editor.Mapped(iso, this)
}

object Editor {
  type Error = Option[String]

  class Mapped[A, B, V](iso: A <=> B, underlying: Editor[A, V]) extends Editor[B, V] {
    override def render[S](data: B,
                           error: Error,
                           onChange: B => ReactST[IO, S, Unit],
                           onCancel: IO[Unit] => ReactST[IO, S, Unit],
                           onEditEnd: ReactST[IO, S, Unit],
                           T: ComponentStateFocus[S]) = {
      val data2 = iso from data
      val onChange2 = (a: A) => onChange(iso to a)
      underlying.render(data2, error, onChange2, onCancel, onEditEnd, T)
    }
  }
}

object Editors {

  class TextEditor(node: Tag) extends Editor[String, Modifier] {
    override def render[S](data: String,
                           error: Error,
                           onChange: String => ReactST[IO, S, Unit],
                           onCancel: IO[Unit] => ReactST[IO, S, Unit],
                           onEditEnd: ReactST[IO, S, Unit],
                           T: ComponentStateFocus[S]) = {

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
  }

  object CheckboxEditor extends Editor[Boolean, Modifier] {
    override def render[S](data: Boolean,
                           error: Error,
                           onChange: Boolean => ReactST[IO, S, Unit],
                           onCancel: IO[Unit] => ReactST[IO, S, Unit],
                           onEditEnd: ReactST[IO, S, Unit],
                           T: ComponentStateFocus[S]) = {

      def ch(e: InputEvent) =
        onChange(e.target.checked) >> onEditEnd

      div(
        checkbox(data)(
          onchange ~~> T._runState(ch),
          error.isDefined && (cls := "error")),
        error.fold(EmptyTag)(e => div(cls := "errorMsg", e)))
    }
  }

  val TextInputEditor = new TextEditor(input)
  val TextareaEditor = new TextEditor(textarea)
}