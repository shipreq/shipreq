package shipreq.webapp.client.project.widgets.high

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.SingleLine
import shipreq.webapp.base.text.Grammar.{hashRefKey => G}
import shipreq.webapp.base.validation._
import shipreq.webapp.client.base.data.Plain
import shipreq.webapp.client.base.feature.EditorStatus
import shipreq.webapp.client.base.lib.{KeyboardTheme, AbortCommit => AbortCommit2}
import shipreq.webapp.client.base.ui.{AutosizeTextarea, EditTheme}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.AutoComplete
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
      empty ++ tags.toIterator.filter(_.live :: Live)

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
        .map(a => pc.atag(a).key.value)
        .sorted |>
        G.seqFormat.merge
    (ids, text)
  }

  type Output      = SetDiff.NE[ApplicableTagId]
  type CommitFn    = Output ~=> Callback
  type AbortCommit = Option[AbortCommit2[Callback, CommitFn]]

  case class Props(preEditValue: Option[Set[ApplicableTagId]],
                   edit        : StateSnapshot[String],
                   lookup      : Lookup,
                   asyncStatus : Option[EditorStatus.Async],
                   abortCommit : AbortCommit) {

    // TODO Really? Stream?
    val parseResult: ValidationResult[Stream[ApplicableTag]] =
      validator.correctAndValidate(lookup, edit.value)

    val parseResultSet: ValidationResult[Set[ApplicableTagId]] =
      parseResult.map(_.map(_.id)(collection.breakOut))

    val validated = EditValidationFeature.compareSetOption(parseResultSet)(preEditValue)
    def abort     = abortCommit.fold(Callback.empty)(_.abort)
    def commit    = (r: Output) => abortCommit.fold(Callback.empty)(_ commit r)
    val status    = asyncStatus getOrElse EditorStatus.validUpdateV(validated)(commit, abort)

    def render = Component(this)
  }

  implicit val reusabilityLookup: Reusability[Lookup] =
    Reusability.byRef[Lookup] || Reusability.byUnivEq(_.underlyingMap)

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.never // TODO Reusability.caseClass

  val validator =
    Validator.seqText(G.seqFormat)((l: Lookup) =>
      i => ValidationResult.option(l get i, VFailure looseMsg s"Invalid tag: $i"))

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxLookup = Px.props($).map(_.lookup).withReuse.autoRefresh

    val pxAutoComplete = pxLookup.map(l =>
      AutoComplete.tag(l.values.toStream, HideDead)(Plain))

    @inline private def lineCardinality = SingleLine

    val textareaConst: TagMod = {
      val keys =
        KeyboardTheme.abortCriterion.handle($.props.flatMap(_.abort)) +
        KeyboardTheme.commitCO($.props.map(_.status.getCommit), lineCardinality)

      val updateState: ReactEventFromTextArea => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.setState(e.target.value.replace("\n", ""))))

      TagMod(
        ^.autoFocus := true,
        ^.spellCheck := false,
        ^.onChange ==> updateState,
        RichTextEditor.minRows(lineCardinality),
        keys)
    }

    val editorRef = ScalaComponent.mutableRefTo(AutosizeTextarea.Component)

    def getTextarea(): html.TextArea =
      editorRef.value.getDOMNode.domCast

    def render(p: Props) = {

      def editor(validity: Validity): VdomElement =
        editorRef.component(EditTheme.autosizeTextareaProps(validity, p.edit.value, textareaConst))

      def instructions =
        KeyboardTheme.instructionsForCommitAbort(
          lineCardinality,
          p.status.getCommit,
          p.abort,
          None)

      EditTheme.renderEditor(p.status, editor, p.edit.value, instructions)
    }
  }

  val Component =
    ScalaComponent.builder[Props]("TagEditor")
      .renderBackend[Backend]
      .configure(
        Reusability.shouldComponentUpdate,
        AutoCompleteFeature.installBP(_.backend.getTextarea(), _.pxAutoComplete.value(), _.edit.setState))
      .build
}
