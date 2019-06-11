package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalaz.\/
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.feature.EditorStatus
import shipreq.webapp.base.lib.{KeyHandlers, KeyboardTheme}
import shipreq.webapp.base.text.Grammar.{hashRefKey => G}
import shipreq.webapp.base.text.SingleLine
import shipreq.webapp.base.ui.EditTheme
import shipreq.webapp.base.validation.Simple._
import shipreq.webapp.client.project.lib.DataReusability._

object TagEditor {

  /**
   * Lookup of tags by their names.
   * Required for validation.
   */
  type Lookup = IMap[String, ApplicableTag]

  object Lookup {
    def empty: Lookup =
      IMap.empty(_.key.value)

    def apply(tags: TraversableOnce[ApplicableTag]): Lookup =
      empty ++ tags.toIterator.filter(_.live is Live)

    def all(p: Project): Lookup =
      apply(p.config.liveTagFieldDistribution.tags.all)

    def forTagField(f: CustomField.Tag.Id)(p: Project): Lookup =
      apply(p.config.liveTagFieldDistribution.tags inField f)

    def notUsedInTagFields(p: Project): Lookup =
      apply(p.config.liveTagFieldDistribution.tags.notUsedInFields)
  }

  def initialValues(initial: Set[ApplicableTagId], pc: ProjectConfig, l: Lookup): (Set[ApplicableTagId], String) = {
    val ls = l.valuesIterator.map(_.id).toSet
    val ids = initial & ls
    val text =
      ids.toVector
        .map(a => pc.tags.atag(a).key.value)
        .sorted |>
        G.seqFormat.merge
    (ids, text)
  }

  type Output   = SetDiff.NE[ApplicableTagId]
  type CommitFn = Output ~=> Callback

  val validator: Lookup => Validator[String, Stream[String], Stream[ApplicableTag]] =
    l => G.seqFormat.validator(Auditor.optionFn(l.get)(i => Invalidity(s"Invalid tag: $i")))

  case class Props(preEditValue    : Option[Set[ApplicableTagId]],
                   edit            : StateSnapshot[String],
                   lookup          : Lookup,
                   asyncStatus     : Option[EditorStatus.Async],
                   abort           : Option[Callback],
                   commitFn        : Option[CommitFn],
                   commitVerb      : String,
                   extraKbShortcuts: KeyboardTheme.Shortcuts,
                   showInstructions: Boolean) {

    // TODO Really? Stream?
    val parseResult: Invalidity \/ Stream[ApplicableTag] =
      validator(lookup)(edit.value)

    val parseResultSet: Invalidity \/ Set[ApplicableTagId] =
      parseResult.map(_.map(_.id)(collection.breakOut))

    val validated = PotentialChange.fromDisjunction(parseResultSet).setDiffOption(preEditValue)
    def commit    = (r: Output) => commitFn.map(_ apply r)
    val status    = asyncStatus getOrElse EditorStatus.fromValidatedChange(validated)(commit, abort)

    def render: VdomElement = Component(this)
  }

  implicit val reusabilityLookup: Reusability[Lookup] =
    Reusability.byRef[Lookup] || Reusability.byUnivEq(_.underlyingMap)

//  implicit val reusabilityProps: Reusability[Props] =
//    Reusability.never // TODO Reusability.derive

  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.EditorBackend {
    private val pxLookup = Px.props($).map(_.lookup).withReuse.autoRefresh

    override val pxAutoComplete = pxLookup.map(l =>
      AutoComplete.Project.tag(l.values.toStream, HideDead)(Plain))

    @inline private def lineCardinality = SingleLine

    private val keyHandlerBase =
      KeyHandlers.base(
        KeyboardTheme.abortCriterion.handleWhenDefined($.props.map(_.abort)) +
        KeyboardTheme.commitCO($.props.map(_.status.getCommit), lineCardinality))

    val textareaConst: TagMod = {
      val updateState: ReactEventFromTextArea => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.setState(e.target.value.replace("\n", ""))))

      TagMod(
        ^.autoFocus := true,
        ^.spellCheck := false,
        ^.onBlur   --> autoCompleteBlur,
        ^.onChange ==> updateState,
        RichTextEditor.minRows(lineCardinality))
    }

    def render(p: Props) = {

      def editor(validity: Validity): VdomElement = {
        val keys = keyHandlerBase(p.extraKbShortcuts.keyHandlers)
        val base = TagMod(textareaConst, keys)
        editorRef.component(EditTheme.autosizeTextareaProps(validity, p.edit.value, base))
      }

      def instructions: TagMod =
        TagMod.when(p.showInstructions)(
          KeyboardTheme.Instructions(
            p.extraKbShortcuts.instructions ::: KeyboardTheme.Instructions.Clauses.forTextEditor(
              lineCardinality,
              commit = p.status.getCommit,
              commitVerb = p.commitVerb,
              abort = p.abort),
            help = None))

      EditTheme.renderEditor(p.status, editor, p.edit.value, instructions)
    }
  }

  val Component =
    ScalaComponent.builder[Props]("TagEditor")
      .renderBackend[Backend]
      .configure(
//        Reusability.shouldComponentUpdate,
        AutoComplete.install)
      .build
}
