package shipreq.webapp.client.project.widgets.high

import japgolly.scalajs.react.extra._
import japgolly.scalajs.react._, vdom.prefix_<^._
import shipreq.base.util.{Ref => _, _}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{LineCardinality, MultiLine, SingleLine}
import shipreq.webapp.base.text.GrammarSpec.SeqFormat
import shipreq.webapp.base.validation.{ValidationResult, Validator}
import shipreq.webapp.client.base.feature.EditorStatus
import shipreq.webapp.client.base.lib.{KeyboardTheme, AbortCommit => AbortCommit2}
import shipreq.webapp.client.base.ui.{AutosizeTextarea, EditTheme}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.{AutoComplete, TextEditor}
import Validators.{reqCode => V}

sealed abstract class ReqCodeEditor[In: Reusability, Out] {
  final type Output = Out

  val textEditor: TextEditor

  val validator: Validator[V.VS, String, _, In]

  def liveCorrect(t: String): String

  def dataToSet(d: Option[In]): Set[ReqCode.Value]

  val validate: ValidationResult[In] => Option[In] => EditValidationFeature.Result[Out]

  val lineCardinality: LineCardinality

  type CommitFn    = Out ~=> Callback
  type AbortCommit = Option[AbortCommit2[Callback, CommitFn]]

  case class Props(edit        : ReusableVar[String],
                   initialValue: Option[In],
                   trie        : ReqCode.Trie,
                   asyncStatus : Option[EditorStatus.Async],
                   abortCommit : AbortCommit) {

    val parseResult = validator.correctAndValidate(V.VS(trie, dataToSet(initialValue)), edit.value)
    val validated   = validate(parseResult)(initialValue)
    def abort       = abortCommit.fold(Callback.empty)(_.abort)
    def commit      = (r: Out) => abortCommit.fold(Callback.empty)(_ commit r)
    val status      = asyncStatus getOrElse EditorStatus.validUpdateV(validated)(commit, abort)

    def render = Component(this)
  }

  implicit lazy val reusabilityProps: Reusability[Props] =
    Reusability.never // TODO Reusability.caseClass

  private val editorRef = Ref.to(AutosizeTextarea.Component, "i")

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxTrie = Px.bs($).propsA(_.trie)

    val pxAutoComplete = pxTrie.map(t =>
      AutoComplete.reqCode.prefixes(t))

    def getTextarea() =
      editorRef($).get.getDOMNode()

    val textareaConst: TagMod = {
      val keys =
        KeyboardTheme.abortCriterion.handle($.props.flatMap(_.abort)) +
        KeyboardTheme.commitCO($.props.map(_.status.getCommit), lineCardinality)

      val updateState: ReactEventTA => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.set(liveCorrect(e.target.value))))

      TagMod(
        ^.autoFocus := true,
        ^.onChange ==> updateState,
        RichTextEditor.minRows(lineCardinality),
        keys)
    }

    def render(p: Props) = {
      def editor(validity: Validity): ReactElement =
        EditTheme.autosizeTextarea(editorRef, validity, p.edit.value, textareaConst)

      def instructions =
        KeyboardTheme.instructionsForCommitAbort(
          lineCardinality,
          p.status.getCommit,
          p.abort,
          None)

      EditTheme.renderEditor(p.status, editor, p.edit.value, instructions)
    }
  }

  @inline private implicit def impTextEditor = textEditor.asImplicit

  // lazy else there'll be a FieldNotInitialised error via .configure → impTextEditor → textEditor
  lazy val Component =
    ReactComponentB[Props]("ReqCodeEditor")
      .renderBackend[Backend]
      .configure(
        Reusability.shouldComponentUpdate,
        AutoCompleteFeature.installBP(_.backend.getTextarea(), _.pxAutoComplete.value(), _.edit.set))
      .build
}

object ReqCodeEditor {

  // ===================================================================================================================

  /**
    * Editor for a single ReqCode (as is the case in ReqCodeGroups).
    */
  object Single extends ReqCodeEditor[ReqCode.Value, ReqCode.Value] {
    override val textEditor                          = TextEditor.Input
    override val validator                           = V.code
    override def liveCorrect(txt: String)            = V.code.liveCorrect(txt)
    override def dataToSet(d: Option[ReqCode.Value]) = d.toSet
    override val validate                            = EditValidationFeature.compareOption[ReqCode.Value] _
    override val lineCardinality                     = SingleLine
  }

  // ===================================================================================================================

  /**
    * Editor for multiple ReqCodes.
    */
  object Multiple extends ReqCodeEditor[Set[ReqCode.Value], SetDiff.NE[ReqCode.Value]] {

    override val textEditor = TextEditor.TextArea

    val seqFmt = SeqFormat(_.trim, "\\s*[\n\r]\\s*".r.pattern, identity, _.isEmpty, _ mkString "\n")

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

    override val validate = EditValidationFeature.compareSetOption[ReqCode.Value] _

    override val lineCardinality = MultiLine
  }
}
