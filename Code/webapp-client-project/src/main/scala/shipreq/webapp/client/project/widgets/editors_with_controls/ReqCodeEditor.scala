package shipreq.webapp.client.project.widgets.editors_with_controls

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util._
import shipreq.webapp.base.data.DataValidators.{reqCode => V}
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.feature.{EditControlsFeature, EditorStatus}
import shipreq.webapp.member.text.GrammarSpec.SeqFormat
import shipreq.webapp.member.text.{LineCardinality, MultiLine, SingleLine}
import shipreq.webapp.base.util.KeyHandlers
import shipreq.webapp.base.validation.lib.Simple._
import shipreq.webapp.client.project.feature.EditorFeature.PotentialValueAcceptor
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.TextEditor

sealed abstract class ReqCodeEditor[In: Reusability, Out] {
  final type Output = Out

  val textEditor: TextEditor

  val validator: V.State => Validator[String, _, In]

  def liveCorrect(t: String): String

  def dataToSet(d: Option[In]): Set[ReqCode.Value]

  val validate: (Invalidity \/ In, Option[In]) => PotentialChange[Invalidity, Out]

  val lineCardinality: LineCardinality

  type CommitFn = Out => Callback

  final lazy val potentialValueAcceptor: PotentialValueAcceptor[String] =
    PotentialValueAcceptor.correct(liveCorrect)

  case class Props(edit            : StateSnapshot[String],
                   initialValue    : Option[In],
                   trie            : ReqCode.Trie,
                   asyncStatus     : Option[EditorStatus.Async],
                   abort           : Option[Callback],
                   abortVerb       : String,
                   autoFocus       : Boolean,
                   commitFn        : Option[CommitFn],
                   commitVerb      : String,
                   extraControls   : EditControlsFeature.ExtraControls,
                   showInstructions: Boolean) {

    val parseResult = validator(V.State(trie, dataToSet(initialValue)))(edit.value)
    val validated   = validate(parseResult, initialValue)
    def commit      = (r: Out) => commitFn.map(_ apply r)
    val status      = asyncStatus getOrElse EditorStatus.fromValidatedChange(validated)(commit, abort)

    def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.byRef // because Props are memo'ised in NewEditor

  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.EditorBackend {
    private val pxTrie = Px.props($).map(_.trie).withReuse.autoRefresh

    override val pxAutoComplete = pxTrie.map(t =>
      AutoComplete.Project.reqCodePrefixes(t))

    private val editControls =
      EditControlsFeature.Controls[Props](lineCardinality)
        .abortWhenDefined(_.abort, _.abortVerb)
        .commitWhenDefined(_.status.getCommit, _.commitVerb)
        .addDynamicExtras(_.extraControls)

    private val keyHandlerBase =
      KeyHandlers.base(autoCompleteKeyHandlers)

    val textareaConst: TagMod = {
      val updateState: ReactEventFromTextArea => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.setState(liveCorrect(e.target.value))))

      TagMod(
        ^.onBlur   --> autoCompleteOnBlur,
        ^.onClick  ==> autoCompleteOnClick,
        ^.onChange ==> updateState,
        RichTextEditor.minRows(lineCardinality))
    }

    def render(p: Props) = {
      def editor(validity: Validity): VdomElement = {
        val keys = keyHandlerBase(editControls.keyHandlers(p))
        val base = TagMod(
          textareaConst,
          keys,
          ^.autoFocus  := p.autoFocus)
        val autosizeProps = EditControlsFeature.autosizeTextareaProps(
          position = Some(EditControlsFeature.Style.default.position),
          mode     = EditControlsFeature.Mode.Inline,
          enabled  = Enabled,
          validity = validity,
          value    = p.edit.value,
          tagMod   = base,
        )
        editorRef.component(autosizeProps)
      }

      def instructions: TagMod =
        TagMod.when(p.showInstructions)(
          editControls.instructions(p))

      EditControlsFeature.renderEditor(p.status, editor, p.edit.value, instructions)
    }

    def onMount(p: Props): Callback =
      EditControlsFeature.onTextareaEditorMount(editorRef, p.autoFocus)
  }

  // lazy else there'll be a FieldNotInitialised error via .configure -> impTextEditor -> textEditor
  lazy val Component =
    ScalaComponent.builder[Props]
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .configure(AutoComplete.install)
      .componentDidMount($ => $.backend.onMount($.props))
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

    val seqFmt = SeqFormat(_.trim, "\\s*[\n\r]\\s*".r.pattern, identity, _.isEmpty, _.iterator mkString "\n")

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
