package shipreq.webapp.client.app.ui.reqtable.edit

import japgolly.scalajs.react.extra.{ReusableVal, Px}
import scalaz.{\/-, -\/}
import shipreq.base.util.{SetDiff, Util}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.app.ui.{RemoteDataEditor, TextSeqEditor, VUCA}
import shipreq.webapp.client.lib.ui.TextEditor
import TextSeqEditor._
import Validators.{reqCode => V}
import UpdateContentCmd.{PatchReqCodes, SetReqCodeGroupCode}

object ReqCodeEditor {

  type A = ReqCode.Value

  def mkAutoComplete(validationState: Px[V.VS]): Px[AutoComplete] =
    validationState.map(vs => ReusableVal.byRef(
      AutoComplete.reqCode.prefixes(vs.trie)))

  def mkParser(validationState: Px[V.VS]): Parser[A] = str => {
    val vs = validationState.value()
    V.code.correctAndValidate(vs, str)
  }

  // ===================================================================================================================
  object ForGroup {
    val editor = new TextSeqEditor[A, A]("ReqCode editor", Stream(_), TextEditor.Input)

//    @inline def liveCorrect(t: String) = V.code.liveCorrect(t)

    def prepare(initial: Option[A], trie: Px[ReqCode.Trie]): VUCA[String, A] => editor.Props = {
      val validationState = trie.map(V.VS(_, initial.toSet))
      val autoComplete    = mkAutoComplete(validationState)
      val parser          = mkParser(validationState)

      val validate: Vector[A] => ParseResult[A] =
        _.headOption match {
          case None    => -\/(Some(UiText.FieldNames.reqCode + " cannot be blank.")) // english
          case Some(c) => \/-(c)
        }

      editor.Props(_, parser, validate, autoComplete.value(), cellStyle, cellErrorMsgStyle)
    }

    def selfManaged(initial : Option[A],
                    trie    : Px[ReqCode.Trie],
                    commitFn: A => RemoteDataEditor.OnCommit): InitSelfManagedA[String] = {

      def init     = initial.fold("")(PlainText.reqCode)
      val props    = prepare(initial, trie)
      val onCommit = RemoteDataEditor.CommitFilter(commitFn).ignoreIfEqualO(initial)

      (init, (s, u, a, commit) => props(VUCA(s, u, v => commit(onCommit(v)), a)).render)
    }

    def edit(subjectId: ReqCodeId,
             initial  : A,
             trie     : Px[ReqCode.Trie],
             commitFn : UpdateContentOnCommit) =
      selfManaged(Some(initial), trie, commitFn.cmap[A](SetReqCodeGroupCode(subjectId, _)))
  }

  // ===================================================================================================================
  object ForReqs {
    val lineSplitPat = "\\s*[\n\r]\\s*".r.pattern
    val lineSplit = (s: String) => lineSplitPat.split(s.trim).toStream.filter(_.nonEmpty)

    val editor = new TextSeqEditor[A, SetDiff[A]]("ReqCode editor", lineSplit, TextEditor.TextArea)

//    def liveCorrect(txt: String): String =
//      if (txt.trim.isEmpty)
//        ""
//      else {
//        val r = txt.split("[\n\r]").map(V.code.liveCorrect).mkString("\n")
//        Util.fixBeforeAfter(txt, r)(_ endsWith "\n", _ + "\n")
//      }

    def prepare(initial: Set[A], trie: Px[ReqCode.Trie]): VUCA[String, SetDiff[A]] => editor.Props = {
      val validationState = trie.map(V.VS(_, initial))
      val autoComplete    = mkAutoComplete(validationState)
      val parser          = mkParser(validationState)

      val validate: Vector[A] => ParseResult[SetDiff[A]] =
        as => V.codeSet.correctAndValidateU(as.toSet).map(SetDiff.compare(initial, _))

      editor.Props(_, parser, validate, autoComplete.value(), cellStyle, cellErrorMsgStyle)
    }

    def edit(subjectId: ReqId,
             initial  : Set[A],
             trie     : Px[ReqCode.Trie],
             commitFn : UpdateContentOnCommit): InitSelfManagedA[String] = {

      def init     = initial.toVector.map(PlainText.reqCode).sorted mkString "\n"
      val props    = prepare(initial, trie)
      val onCommit = commitFn.setDiff[A](PatchReqCodes(subjectId, _))

      (init, (s, u, a, commit) => props(VUCA(s, u, v => commit(onCommit(v)), a)).render)
    }
  }
}
