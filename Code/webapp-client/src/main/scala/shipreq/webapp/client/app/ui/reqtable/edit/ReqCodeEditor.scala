package shipreq.webapp.client.app.ui.reqtable.edit

import japgolly.scalajs.jquery.TextComplete
import shipreq.base.util.{UnivEq, Must, Px}
import shipreq.webapp.base.{TextMod, UiText}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar, PlainText}
import shipreq.webapp.client.app.ui.TextSeqEditor._
import shipreq.webapp.client.app.ui.reqtable._
import shipreq.webapp.client.lib.ui.TextEditor
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt
import scalaz.{-\/, Success, Failure, \/-}
import scalaz.effect.IO
import Validators.{reqCode => V}

object ReqCodeEditor {

  type A = ReqCode.Value

  object ForGroup {
  }

  object ForReqs {

    val lineSplitter = "\\s*[\n\r]\\s*".r.pattern

    val editor = textSeqEditor[A]("ReqCode editor",
      s => lineSplitter.split(s.trim).toStream.filter(_.nonEmpty),
      TextEditor.TextArea)

    def apply(initial        : Set[A],
              validationState: Px[V.VS],
              setState       : Option[Cell.State] => IO[Unit]): Cell.State = {

      def init: String =
        initial.toVector.map(PlainText.reqCode).sorted mkString "\n"

      val autoComplete: AutoComplete =
        Px.const(TextComplete.Strategies())
      // TODO .xxx should have meaning to auto-complete

      val parser: Parser[A] = () => {
        val vs = validationState.value()
        codeStr => V.code.correctAndValidate(vs, codeStr) match {
          case Success(c) => \/-(c)
          case Failure(f) => -\/(Some(f.toText))
        }
      }

      val abort: IO[Unit] =
        setState(None)

      val commit: Vector[A] => IO[Unit] =
      // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
        s => setState(None) >>> IO { println("Sent to ze server: " + s) }

      Cell.selfManageC(setState, liveCorrect)(
        init, editor.Props(_, _, abort, parser, commit, autoComplete).apply)
    }

    def fixit(before: String, after: String)(test: String => Boolean, fix: String => String): String =
      if (test(before) && !test(after))
        fix(after)
      else
        after

    def liveCorrect(txt: String): String =
      if (txt.trim.isEmpty)
        ""
      else {
        val r = txt.split("[\n\r]")
          .map { code =>
            val c1 = TextMod.noWhitespace(code)
            val c2 = c1.split('.').map(V.node.liveCorrect).mkString(".")
            fixit(c1, c2)(_ endsWith ".", _ + ".")
          }.mkString("\n")
        fixit(txt, r)(_ endsWith "\n", _ + "\n")
      }
  }
}
