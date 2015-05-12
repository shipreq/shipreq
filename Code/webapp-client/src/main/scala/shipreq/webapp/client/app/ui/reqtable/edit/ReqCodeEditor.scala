package shipreq.webapp.client.app.ui.reqtable.edit

import scalaz.{\/-, -\/}
import scalaz.effect.IO
import shipreq.base.util.{Util, Px}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.app.ui.TextSeqEditor, TextSeqEditor._
import shipreq.webapp.client.app.ui.reqtable._
import shipreq.webapp.client.lib.ui.TextEditor
import shipreq.webapp.client.util.ReusableVal
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt
import Validators.{reqCode => V}

object ReqCodeEditor {

  type A = ReqCode.Value

  def mkAutoComplete(validationState: Px[V.VS]): Px[AutoComplete] =
    validationState.map(vs => ReusableVal(
      AutoComplete.reqCode.prefixes(vs.trie)))

  def mkParser(validationState: Px[V.VS]): Parser[A] = () => {
    val vs = validationState.value()
    V.code.correctAndValidate(vs, _)
  }

  // ===================================================================================================================
  object ForGroup {
    val editor = new TextSeqEditor[A, A]("ReqCode editor", Stream(_), TextEditor.Input, cellStyle, cellErrorMsgStyle)

    def apply(initial        : A,
              validationState: Px[V.VS],
              setState       : Option[Cell.State] => IO[Unit]): Cell.State = {

      def init         = PlainText reqCode initial
      val autoComplete = mkAutoComplete(validationState)
      val parser       = mkParser(validationState)

      val validate: Vector[A] => ParseResult[A] =
        _.headOption match {
          case None    => -\/(Some(UiText.FieldNames.reqCode + " cannot be blank.")) // english
          case Some(c) => \/-(c)
        }

      val abort: IO[Unit] =
        setState(None)

      val commit: A => IO[Unit] =
      // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
        s => setState(None) >>> IO { println("Sent to ze server: " + s) }

      Cell.selfManageC(setState, liveCorrect)(
        init, editor.Props(_, _, abort, parser, validate, commit, autoComplete.value()).apply)
    }

    @inline def liveCorrect(t: String) = V.code.liveCorrect(t)
  }

  // ===================================================================================================================
  object ForReqs {
    val lineSplitter = "\\s*[\n\r]\\s*".r.pattern

    val editor = textSetEditor[A]("ReqCode editor",
      s => lineSplitter.split(s.trim).toStream.filter(_.nonEmpty),
      TextEditor.TextArea)

    def apply(initial        : Set[A],
              validationState: Px[V.VS],
              setState       : Option[Cell.State] => IO[Unit]): Cell.State = {

      def init         = initial.toVector.map(PlainText.reqCode).sorted mkString "\n"
      val autoComplete = mkAutoComplete(validationState)
      val parser       = mkParser(validationState)

      val validate: Vector[A] => ParseResult[Set[A]] =
        as => V.codeSet.correctAndValidateU(as.toSet)

      val abort: IO[Unit] =
        setState(None)

      val commit: Set[A] => IO[Unit] =
      // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
        s => setState(None) >>> IO { println("Sent to ze server: " + s) }

      Cell.selfManageC(setState, liveCorrect)(
        init, editor.Props(_, _, abort, parser, validate, commit, autoComplete.value()).apply)
    }

    def liveCorrect(txt: String): String =
      if (txt.trim.isEmpty)
        ""
      else {
        val r = txt.split("[\n\r]").map(V.code.liveCorrect).mkString("\n")
        Util.fixBeforeAfter(txt, r)(_ endsWith "\n", _ + "\n")
      }
  }
}
