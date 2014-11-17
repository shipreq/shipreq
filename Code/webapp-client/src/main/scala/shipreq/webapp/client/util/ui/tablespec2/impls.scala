package shipreq.webapp.client.util.ui.tablespec2

import design._
import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import shipreq.webapp.client.util.ui.Util.textChangeRecv
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

  def textEditor[S](node: Tag, T: ComponentStateFocus[S]): Editor[String, ECB[S], Modifier] =
    ei => {
      val base = node(cls := ei.cssClass, value := ei.data)
      ei.editable match {
        case None =>
          base(readonly := true)
        case Some(cb) =>
          base(
            onchange  ~~> T._runState(textChangeRecv(cb.onChange)),
            onkeydown ~~> T._runState(cancelOnEscape(cb.onCancel)),
            onblur    ~~> T.runState(cb.onEditFinished))
      }
    }

  abstract class ECB2 {
    type S
    type ST = ReactST[IO, S, Unit]
    val f: ComponentStateFocus[S]
    val cb: ST
    def run = f.runState(cb)
  }

  def textEditor2(node: Tag): Editor[String, ECB2, Modifier] =
    ei => {
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
    }

  def renderWithError[D, CB](editor: Editor[D, CB, Modifier])(err: String): Editor[D, CB, Modifier] =
    ei => div(editor(ei), div(cls := "errorMsg", err))

  def editorWithError[D, CB](editor: Editor[D, CB, Modifier]): EditorE[Option[String], D, CB, Modifier] =
    _.fold(editor)(renderWithError(editor))

//  def editorLiveCorrect[D, Callback, V](lc: D => D, e: Editor[D, Callback, V]): Editor[D, Callback, V]
}