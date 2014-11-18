package shipreq.webapp.client.util.ui.tablespec2

import tryagain._
import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import monocle._
import monocle.function.{first, second}
import monocle.std.tuple2._
import shipreq.webapp.base.validation._
import shipreq.webapp.client.util.ui.Util.textChangeRecv
import scalajs.js.UndefOr
import scala.util.Try
import scalaz._, Scalaz._
import scalaz.effect.IO

object impls2 {

  def nopCB[S] = ReactS.retT[IO, S, Unit](())

  type RST[S, A] = ReactST[IO, S, A]

  type StrCf    = ComponentStateFocus[String]
  type StrCb[A] = RST[String, A]

  def cancelOnEscape[S](cb: RST[S, Unit]): ReactKeyboardEventH => RST[S, Unit] =
    e => e.key match {
      case "Escape" => // TODO use KeyValue
        val t = e.target
        ReactS.retM[IO, S, Unit](e.preventDefaultIO >> e.stopPropagationIO) >> cb.addCallback(IO(t.blur()))
      case _ =>
        nopCB
    }

  abstract class ECB2 {
    type S
    type A = Unit
    def f: ComponentStateFocus[S]
    def cb: ReactST[IO, S, A]

    final def run: IO[A] = f.runState(cb)
  }
  object ECB2 {
    def apply[_S](_f: => ComponentStateFocus[_S], _cb: => ReactST[IO, _S, Unit]): ECB2 =
      new ECB2 {
        override type S = _S
        override def f = _f
        override def cb = _cb
      }
  }

  def textEditor(node: Tag): Editor[String, String, ECB2, Modifier] =
    Editor(ei => {
      val base = node(cls := ei.cssClass, value := ei.data)
      ei.editable match {
        case None =>
          base(readonly := true)
        case Some(cb) =>
          base(
            onchange  ~~> textChangeRecv(cb.onChange(_).run),
            onkeydown ~~> cb.onCancel.f._runState(cancelOnEscape(cb.onCancel.cb)),
            onblur    ~~> cb.onEditFinished.run)
      }
    })

  val textInputEditor = textEditor(input)
  val textareaEditor  = textEditor(textarea)

  def renderWithError[A, B, C](editor: Editor[A, B, C, Modifier])(err: String): Editor[A, B, C, Modifier] =
    Editor(ei => div(editor render ei, div(cls := "errorMsg", err)))

  def editorWithError[A, B, C](editor: Editor[A, B, C, Modifier]): EditorE[Option[String], A, B, C, Modifier] =
    _.fold(editor)(renderWithError(editor))

  def editorV[E, A, B, C, V](f: A => E, e: EditorE[E, A, B, C, V]): Editor[A, B, C, V] =
    Editor(i => e(f(i.data)) render i)

  def validateAndDisplayError[A, B, C](f: A => Option[String], e: Editor[A, B, C, Modifier]): Editor[A, B, C, Modifier] =
    Editor(i => editorV(f, editorWithError(e)) render i)

  @deprecated("Need external validation (S⇒VP)", "")
  def composeEditorValidator[I, C](v: ValidatorPlus[I, _, _], e: Editor[I, I, C, Modifier]): Editor[I, I, C, Modifier] = {
    type E = Editor[I, I, C, Modifier]
    val e1: E = e.mapOutput(v.liveCorrect)
    val e2: E = validateAndDisplayError(i => v.correctAndValidate(i).swap.toOption.map(_.toText), e1)
    e2
  }

  // ===================================================================================================================
  // example

  case class Age(value: Int)
  case class Person(id: Long, name: String, age: Age)

  val nameV: ValidatorPlus[String, String, String] = ???

  val ageV =
    ValidatorPlus[String, Option[Int], Age](
      CorrectionPart[String, Option[Int]](s => Try(Option(s.toInt)).getOrElse(None))(_.fold("")(_.toString)),
      ValidationPart[Option[Int], Age](???),
      _.replaceAll("\\D", ""))

  val bothV = nameV *** ageV

  val nameE = textInputEditor
  val ageE = textInputEditor

  val nameE2 = composeEditorValidator(nameV, nameE)
  val ageE2 = composeEditorValidator(ageV, ageE)

  val e2 = nameE2 compose ageE2

}