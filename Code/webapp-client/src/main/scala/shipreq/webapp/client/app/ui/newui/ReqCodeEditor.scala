package shipreq.webapp.client.app.ui.newui

import japgolly.scalajs.react.extra._
import japgolly.scalajs.react._, vdom.prefix_<^._
import shipreq.webapp.base.validation.Validator
import shipreq.webapp.client.app.ui.reqtable.edit.AutoComplete
import shipreq.webapp.client.lib.ui.feature._
import shipreq.base.util.{UnivEq, Util}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.client.lib.ui.TextEditor
import Validators.{reqCode => V}
import shipreq.webapp.client.data.DataReusability._

sealed abstract class ReqCodeEditor[Data: Reusability] {

  val textEditor: TextEditor

  val validator: Validator[V.VS, String, _, Data]

  def liveCorrect(t: String): String

  def dataToSet(d: Option[Data]): Set[ReqCode.Value]

  case class Props(edit        : ExternalVar[String],
                   initialValue: Option[Data],
                   trie        : ReqCode.Trie,
                   tagMod      : Option[Data] => TagMod) {

    val parseResult =
      validator.correctAndValidate(V.VS(trie, dataToSet(initialValue)), edit.value)

    def render = Component(this)
  }

  private val editorRef = Ref[textEditor.Dom]("i")

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxTrie = Px.bs($).propsA(_.trie)

    val pxAutoComplete = pxTrie.map(t =>
      AutoCompleteFeature.Strategies( // TODO Fix AutoComplete
        AutoComplete.reqCode.prefixes(t): _*))

    def render(p: Props) = {
      val validated = EditValidationFeature(p.parseResult)

      <.div(
        textEditor.tag(
          ^.ref        := editorRef,
          ^.value      := p.edit.value,
          ^.onChange  ==> ((e: ReactEventI) => p.edit.set(liveCorrect(e.target.value))),
          p.tagMod(validated.validated)),
        validated.renderFailure)
    }
  }

  @inline private implicit def impTextEditor = textEditor.asImplicit

  // lazy else there'll be a FieldNotInitialised error via .configure → impTextEditor → textEditor
  lazy val Component =
    ReactComponentB[Props]("ReqCodeEditor")
      .renderBackend[Backend]
      // TODO .configure(Reusability.shouldComponentUpdate)
      .configure(AutoCompleteFeature.installBP(editorRef, _.pxAutoComplete.value(), _.edit.set))
      .build
}

object ReqCodeEditor {

  // ===================================================================================================================

  /**
    * Editor for a single ReqCode (as is the case in ReqCodeGroups).
    */
  object Single extends ReqCodeEditor[ReqCode.Value] {
    override val textEditor                          = TextEditor.Input
    override val validator                           = V.code
    override def liveCorrect(txt: String)            = V.code.liveCorrect(txt)
    override def dataToSet(d: Option[ReqCode.Value]) = d.toSet
  }

  // ===================================================================================================================

  /**
    * Editor for multiple ReqCodes.
    */
  object Multiple extends ReqCodeEditor[Set[ReqCode.Value]] {

    override val textEditor = TextEditor.TextArea

    val seqFmt = Grammar.SeqFormat(_.trim, "\\s*[\n\r]\\s*".r.pattern, identity, _.isEmpty, _ mkString "\n")

    override val validator =
      Validator.seqText(seqFmt)(V.code.correctAndValidate _ curried)
        .map(_.toSet)
        .andThen(V.codeSet.liftS)

    override def liveCorrect(txt: String): String =
      if (txt.trim.isEmpty)
        ""
      else {
        val r = txt.split("[\n\r]").map(V.code.liveCorrect).mkString("\n")
        Util.fixBeforeAfter(txt, r)(_ endsWith "\n", _ + "\n")
      }

    override def dataToSet(d: Option[Set[ReqCode.Value]]) =
      d getOrElse UnivEq.emptySet
  }
}
