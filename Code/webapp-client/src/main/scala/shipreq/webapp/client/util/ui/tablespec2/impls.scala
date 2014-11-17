package shipreq.webapp.client.util.ui.tablespec2

import design._
import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import shipreq.webapp.base.validation.ValidatorPlus
import shipreq.webapp.client.util.ui.Util.textChangeRecv
import scalaz._
import scalaz.effect.IO
import scalaz.syntax.bind._

object impls {

  def nopCB[S] = ReactS.retT[IO, S, Unit](())

  type ECB[S] = ReactST[IO, S, Unit]

  def cancelOnEscape[S](cb: ECB[S]): ReactKeyboardEventH => ECB[S] =
    e => e.key match {
      case "Escape" => // TODO use KeyValue
        val t = e.target
        ReactS.retM[IO, S, Unit](e.preventDefaultIO >> e.stopPropagationIO) >> cb.addCallback(IO(t.blur()))
      case _ =>
        nopCB
    }

//  def textEditor[S](node: Tag, T: ComponentStateFocus[S]): Editor[String, ECB[S], Modifier] =
//    ei => {
//      val base = node(cls := ei.cssClass, value := ei.data)
//      ei.editable match {
//        case None =>
//          base(readonly := true)
//        case Some(cb) =>
//          base(
//            onchange  ~~> T._runState(textChangeRecv(cb.onChange)),
//            onkeydown ~~> T._runState(cancelOnEscape(cb.onCancel)),
//            onblur    ~~> T.runState(cb.onEditFinished))
//      }
//    }

  abstract class ECB2 {
    type S
    type ST = ReactST[IO, S, Unit]
    val f: ComponentStateFocus[S]
    val cb: ST
    def run = f.runState(cb)
  }

  def textEditor2(node: Tag): Editor[String, String, ECB2, Modifier] =
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
}