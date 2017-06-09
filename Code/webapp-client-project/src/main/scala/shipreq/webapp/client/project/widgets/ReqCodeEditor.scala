package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._, vdom.html_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom.html
import scalaz.\/
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{LineCardinality, MultiLine, SingleLine}
import shipreq.webapp.base.text.GrammarSpec.SeqFormat
import shipreq.webapp.base.validation.Simple._
import shipreq.webapp.client.base.feature.EditorStatus
import shipreq.webapp.client.base.lib.{KeyboardTheme, AbortCommit => AbortCommit2}
import shipreq.webapp.client.base.ui.{AutosizeTextarea, EditTheme}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.{AutoComplete, TextEditor}
import DataValidators.{reqCode => V}

sealed abstract class ReqCodeEditor[In: Reusability, Out] {
  final type Output = Out

  val textEditor: TextEditor

  val validator: V.State => Validator[String, _, In]

  def liveCorrect(t: String): String

  def dataToSet(d: Option[In]): Set[ReqCode.Value]

  val validate: (Invalidity \/ In, Option[In]) => PotentialChange[Invalidity, Out]

  val lineCardinality: LineCardinality

  type CommitFn    = Out ~=> Callback
  type AbortCommit = Option[AbortCommit2[Callback, CommitFn]]

  case class Props(edit            : StateSnapshot[String],
                   initialValue    : Option[In],
                   trie            : ReqCode.Trie,
                   asyncStatus     : Option[EditorStatus.Async],
                   abortCommit     : AbortCommit,
                   showInstructions: Boolean) {

    val parseResult = validator(V.State(trie, dataToSet(initialValue)))(edit.value)
    val validated   = validate(parseResult, initialValue)
    def abort       = abortCommit.map(_.abort)
    def commit      = (r: Out) => abortCommit.map(_ commit r)
    val status      = asyncStatus getOrElse EditorStatus.fromValidatedChange(validated)(commit, abort)

    def render: VdomElement = Component(this)
  }

//  implicit lazy val reusabilityProps: Reusability[Props] =
//    Reusability.never // TODO Reusability.caseClass

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxTrie = Px.props($).map(_.trie).withReuse.autoRefresh

    val pxAutoComplete = pxTrie.map(t =>
      AutoComplete.reqCode.prefixes(t))

    private val editorRef = ScalaComponent.mutableRefTo(AutosizeTextarea.Component)

    def getTextarea(): html.TextArea =
      editorRef.value.getDOMNode.domCast

    val textareaConst: TagMod = {
      val keys =
        KeyboardTheme.abortCriterion.handleWhenDefined($.props.map(_.abort)) +
        KeyboardTheme.commitCO($.props.map(_.status.getCommit), lineCardinality)

      val updateState: ReactEventFromTextArea => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.setState(liveCorrect(e.target.value))))

      TagMod(
        ^.autoFocus := true,
        ^.onChange ==> updateState,
        RichTextEditor.minRows(lineCardinality),
        keys)
    }

    def render(p: Props) = {
      def editor(validity: Validity): VdomElement =
        editorRef.component(EditTheme.autosizeTextareaProps(validity, p.edit.value, textareaConst))

      def instructions: TagMod =
        TagMod.when(p.showInstructions)(
          KeyboardTheme.instructionsForCommitAbort(
            lineCardinality,
            p.status.getCommit,
            p.abort,
            None))

      EditTheme.renderEditor(p.status, editor, p.edit.value, instructions)
    }
  }

  @inline private implicit def impTextEditor = textEditor.asImplicit

  // lazy else there'll be a FieldNotInitialised error via .configure → impTextEditor → textEditor
  lazy val Component =
    ScalaComponent.builder[Props]("ReqCodeEditor")
      .renderBackend[Backend]
      .configure(
//        Reusability.shouldComponentUpdate,
        AutoCompleteFeature.installBP(_.backend.getTextarea(), _.pxAutoComplete.value(), _.edit.setState))
      .build
}

object ReqCodeEditor {

  // ===================================================================================================================

  /**
    * Editor for a single ReqCode (as is the case in CodeGroups).
    */
  object Single extends ReqCodeEditor[ReqCode.Value, ReqCode.Value] {
    override val textEditor                          = TextEditor.Input
    override val validator                           = V.code.unnamedFn
    override def liveCorrect(txt: String)            = V.code.stateless.corrector.live(txt)
    override def dataToSet(d: Option[ReqCode.Value]) = d.toSet
    override val validate                            = PotentialChange.fromDisjunction(_).ignoreOption(_)
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
      s => seqFmt.validator(V.code.unnamedFn(s).toAuditor).mapValid(_.toSet).appendInvalidator(V.codeSet)

    override def liveCorrect(txt: String): String =
      if (txt.trim.isEmpty)
        ""
      else {
        val r = txt.split("[\n\r]").map(V.code.stateless.corrector.live).mkString("\n")
        Util.fixBeforeAfter(txt, r)(_ endsWith "\n", _ + "\n")
      }

    override def dataToSet(d: Option[Set[ReqCode.Value]]) =
      d getOrElse UnivEq.emptySet

    override val validate = PotentialChange.fromDisjunction(_).setDiffOption(_)

    override val lineCardinality = MultiLine
  }
}
