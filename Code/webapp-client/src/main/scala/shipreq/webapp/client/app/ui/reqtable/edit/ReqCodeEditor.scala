package shipreq.webapp.client.app.ui.reqtable.edit

import japgolly.scalajs.react.extra.{ReusableVal, Px}
import scalaz.{\/-, -\/}
import shipreq.base.util.{SetDiff, Util}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.app.ui.{RemoteDataEditor, TextSeqEditor}
import shipreq.webapp.client.app.ui.reqtable._
import shipreq.webapp.client.lib.ui.TextEditor
import TextSeqEditor._
import Validators.{reqCode => V}
import UpdateContentCmd.{PatchReqCodes, SetReqCodeGroupCode}

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
              onCommit0      : UpdateContentOnCommit): Cell.State = {

      def init         = PlainText reqCode initial
      val autoComplete = mkAutoComplete(validationState)
      val parser       = mkParser(validationState)

      val validate: Vector[A] => ParseResult[A] =
        _.headOption match {
          case None    => -\/(Some(UiText.FieldNames.reqCode + " cannot be blank.")) // english
          case Some(c) => \/-(c)
        }

      val onCommit = onCommit0.cmapToInitial(initial)(SetReqCodeGroupCode(subjectId, _))

      Some(RemoteDataEditor.default[String, String](
        init, liveCorrect, modCell,
        (s, u, abort, commit) =>
          editor.Props(s, u, abort, parser, validate, v => commit(onCommit(v)), autoComplete.value()).apply))
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
              onCommit0      : UpdateContentOnCommit): Cell.State = {

      def init         = initial.toVector.map(PlainText.reqCode).sorted mkString "\n"
      val autoComplete = mkAutoComplete(validationState)
      val parser       = mkParser(validationState)

      val validate: Vector[A] => ParseResult[SetDiff[A]] =
        as => V.codeSet.correctAndValidateU(as.toSet).map(SetDiff.compare(initial, _))

      val onCommit = onCommit0.setDiff[A](PatchReqCodes(subjectId, _))

      Some(RemoteDataEditor.default[String, String](
        init, liveCorrect, modCell,
        (s, u, abort, commit) =>
          editor.Props(s, u, abort, parser, validate, v => commit(onCommit(v)), autoComplete.value()).apply))
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
