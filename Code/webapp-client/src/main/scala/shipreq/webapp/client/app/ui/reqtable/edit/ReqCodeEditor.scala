package shipreq.webapp.client.app.ui.reqtable.edit

import japgolly.scalajs.react.extra.{ReusableVal, Px}
import scalaz.{\/-, -\/}
import shipreq.base.util.{SetDiff, Util}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.ProjectChange
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.app.ui.TextSeqEditor, TextSeqEditor._
import shipreq.webapp.client.app.ui.reqtable._
import shipreq.webapp.client.lib.ui.TextEditor
import Validators.{reqCode => V}
import ProjectChange.{PatchReqCodes, SetReqCodeGroupCode}

object ReqCodeEditor {

  type A = ReqCode.Value

  def mkAutoComplete(validationState: Px[V.VS]): Px[AutoComplete] =
    validationState.map(vs => ReusableVal.byRef(
      AutoComplete.reqCode.prefixes(vs.trie)))

  def mkParser(validationState: Px[V.VS]): Parser[A] = () => {
    val vs = validationState.value()
    V.code.correctAndValidate(vs, _)
  }

  // ===================================================================================================================
  object ForGroup {
    val editor = new TextSeqEditor[A, A]("ReqCode editor", Stream(_), TextEditor.Input, cellStyle, cellErrorMsgStyle)

    def apply(initial        : A,
              subjectId      : ReqCodeId,
              validationState: Px[V.VS])
             (modCell        : Cell.ModCell,
              editIO         : EditIO[ProjectChange]): Cell.Cmd = {

      def init         = PlainText reqCode initial
      val autoComplete = mkAutoComplete(validationState)
      val parser       = mkParser(validationState)

      val validate: Vector[A] => ParseResult[A] =
        _.headOption match {
          case None    => -\/(Some(UiText.FieldNames.reqCode + " cannot be blank.")) // english
          case Some(c) => \/-(c)
        }

      val (abort, commit) = editIO.cmapToInitial(initial)(SetReqCodeGroupCode(subjectId, _)).abortCommit

      Cell.selfManageC(modCell, liveCorrect)(init, (v, s, e) =>
        editor.Props(v, s, abort, parser, validate, commit(e), autoComplete.value()).apply)
    }

    @inline def liveCorrect(t: String) = V.code.liveCorrect(t)
  }

  // ===================================================================================================================
  object ForReqs {
    val lineSplitter = "\\s*[\n\r]\\s*".r.pattern

    val editor = textSetEditor[A, SetDiff[A]]("ReqCode editor",
      s => lineSplitter.split(s.trim).toStream.filter(_.nonEmpty),
      TextEditor.TextArea)

    def apply(initial        : Set[A],
              subjectId      : ReqId,
              validationState: Px[V.VS])
             (modCell        : Cell.ModCell,
              editIO         : EditIO[ProjectChange]): Cell.Cmd = {

      def init         = initial.toVector.map(PlainText.reqCode).sorted mkString "\n"
      val autoComplete = mkAutoComplete(validationState)
      val parser       = mkParser(validationState)

      val validate: Vector[A] => ParseResult[SetDiff[A]] =
        as => V.codeSet.correctAndValidateU(as.toSet).map(SetDiff.compare(initial, _))

      val (abort, commit) = editIO.setDiff[A](PatchReqCodes(subjectId, _)).abortCommit

      Cell.selfManageC(modCell, liveCorrect)(init, (v, s, e) =>
        editor.Props(v, s, abort, parser, validate, commit(e), autoComplete.value()).apply)
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
