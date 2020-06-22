package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation.NaTags
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.feature.EditorStatus
import shipreq.webapp.base.lib.{KeyHandlers, KeyboardTheme}
import shipreq.webapp.base.text.Grammar.{hashRefKey => G}
import shipreq.webapp.base.text.SingleLine
import shipreq.webapp.base.ui.EditTheme
import shipreq.webapp.base.validation.Simple._
import shipreq.webapp.client.project.feature.EditorFeature.PotentialValueAcceptor
import shipreq.webapp.client.project.lib.DataReusability._

object TagEditor {

  /**
   * Lookup of tags by their names.
   * Required for validation.
   */
  final case class Lookup(lookup: String => Option[Invalidity \/ ApplicableTag],
                          legal : List[ApplicableTag])

  object Lookup {

    private def make(config: ProjectConfig, legal: IterableOnce[ApplicableTag]): Lookup = {
      val legalStream = legal.iterator.filter(_.live is Live).toList
      val legalIds    = legalStream.iterator.map(_.id).toSet
      val tagDist     = config.liveTagFieldDistribution
      val tagTree     = config.tags.tree

      val map: Map[String, Invalidity \/ ApplicableTag] =
        config.tags.applicableTagIterator().map { tag =>
          val value: Invalidity \/ ApplicableTag =
            if (tag.live is Dead)
              -\/(Invalidity(s"${tag.key.with_#} is deleted."))
            else if (legalIds contains tag.id)
              \/-(tag)
            else {
              def tagFields = config.liveCustomTagFields.iterator.filter(f => tagDist.inField(f.id).contains(tag.id))
              tagFields.soleElement match {
                case Some(f) => -\/(Invalidity(s"${tag.key.with_#} belongs in the ${f.name(tagTree)} field."))
                case None    => -\/(Invalidity(s"${tag.key.with_#} is not applicable here."))
              }
            }

          tag.name.toLowerCase -> value
        }.toMap

      Lookup(s => map.get(s.toLowerCase), legalStream)
    }

    def all(p: Project): Lookup =
      make(p.config, p.config.liveTagFieldDistribution.tags.all)

    def forTagField(f: CustomField.Tag.Id)(p: Project): Lookup =
      make(p.config, p.config.liveTagFieldDistribution.tags inField f)

    def notUsedInTagFields(p: Project): Lookup =
      make(p.config, p.config.liveTagFieldDistribution.tags.notUsedInFields)
  }

  def initialValues(initial: Set[ApplicableTagId], pc: ProjectConfig, l: Lookup, naTags: NaTags): (Set[ApplicableTagId], String) = {
    val ls = l.legal.iterator.map(_.id).toSet -- naTags.set
    val ids = initial & ls
    val text =
      ids.toVector
        .map(a => pc.tags.needApplicableTag(a).key.value)
        .sorted |>
        G.seqFormat.merge
    (ids, text)
  }

  type Output   = SetDiff.NE[ApplicableTagId]
  type CommitFn = Output ~=> Callback

  val liveCorrect: String => String =
    _.replace("\n", "")

  val potentialValueAcceptor: PotentialValueAcceptor[String] =
    PotentialValueAcceptor.correct(liveCorrect)

  case class Props(preEditValue    : Option[Set[ApplicableTagId]],
                   naTags          : NaTags,
                   edit            : StateSnapshot[String],
                   lookup          : Lookup,
                   asyncStatus     : Option[EditorStatus.Async],
                   abort           : Option[Callback],
                   autoFocus       : Boolean,
                   commitFn        : Option[CommitFn],
                   commitVerb      : String,
                   extraKbShortcuts: KeyboardTheme.Shortcuts,
                   showInstructions: Boolean) {

    private val auditor: Auditor[String, ApplicableTag] =
      Auditor { str =>
        lookup.lookup(str) match {
          case Some(\/-(tag))    => naTags.auditor(tag)
          case Some(err@ -\/(_)) => err
          case None              => -\/(Invalidity(s"${str.replaceFirst("^#?", "#")} doesn't exist."))
        }
      }

    val validator: Validator[String, List[String], List[ApplicableTag]] =
      G.seqFormat.validator(auditor)

    val parseResult: Invalidity \/ List[ApplicableTag] =
      validator(edit.value)

    val parseResultSet: Invalidity \/ Set[ApplicableTagId] =
      parseResult.map(_.iterator.map(_.id).toSet)

    val validated = PotentialChange.fromDisjunction(parseResultSet).setDiffOption(preEditValue)
    def commit    = (r: Output) => commitFn.map(_ apply r)
    val status    = asyncStatus getOrElse EditorStatus.fromValidatedChange(validated)(commit, abort)

    def render: VdomElement = Component(this)
  }

  implicit val reusabilityLookup: Reusability[Lookup] =
    Reusability.byRef[Lookup]

//  implicit val reusabilityProps: Reusability[Props] =
//    Reusability.never // TODO Reusability.derive

  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.EditorBackend {
    private val pxLookup = Px.props($).map(_.lookup).withReuse.autoRefresh

    private val pxTagsToExcludeFromAutoComplete =
      Px.props($).map { p =>
        val na    = p.naTags.set
        val added = p.parseResultSet.getOrElse(Set.empty)
        na ++ added
      }.withReuse.autoRefresh

    override val pxAutoComplete =
      for {
        lookup    <- pxLookup
        naTags    <- pxTagsToExcludeFromAutoComplete
      } yield {
        val legal = lookup.legal.filter(tag => !naTags.contains(tag.id))
        AutoComplete.Project.tag(legal, HideDead)(Plain)
      }

    @inline private def lineCardinality = SingleLine

    private val keyHandlerBase =
      KeyHandlers.base(
        KeyboardTheme.abortCriterion.handleWhenDefined($.props.map(_.abort)) +
        KeyboardTheme.commitCO($.props.map(_.status.getCommit)))

    val textareaConst: TagMod = {
      val updateState: ReactEventFromTextArea => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.setState(liveCorrect(e.target.value))))

      TagMod(
        ^.spellCheck := false,
        ^.onBlur   --> autoCompleteBlur,
        ^.onChange ==> updateState,
        RichTextEditor.minRows(lineCardinality))
    }

    def render(p: Props) = {

      def editor(validity: Validity): VdomElement = {
        val keys = keyHandlerBase(p.extraKbShortcuts.keyHandlers)
        val base = TagMod(
          textareaConst,
          keys,
          ^.autoFocus  := p.autoFocus)
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

    val onMount: Callback =
      EditTheme.onTextareaEditorMount(editorRef, $.props.map(_.autoFocus)).toCallback >>
        trigger(ta => Some(ta.value))
  }

  val Component =
    ScalaComponent.builder[Props]
      .renderBackend[Backend]
      .configure(
        //Reusability.shouldComponentUpdate,
        AutoComplete.install)
      .componentDidMount(_.backend.onMount)
      .build
}
