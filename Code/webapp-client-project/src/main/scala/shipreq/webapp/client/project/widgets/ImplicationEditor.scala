package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalaz.syntax.either._
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data.{Plain, _}
import shipreq.webapp.base.feature.AutoCompleteFeature.AutoComplete.Project.{ReqItem, ReqItems}
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.feature.{EditControlsFeature, EditorStatus}
import shipreq.webapp.base.lib.{KeyHandlers, KeyboardTheme}
import shipreq.webapp.base.text.{Grammar, PlainText, SingleLine, TextSearch}
import shipreq.webapp.base.validation.Simple._
import shipreq.webapp.base.validation._
import shipreq.webapp.client.project.lib.DataReusability._

object ImplicationEditor {

  final case class Lookup(legal                : ReqItems,
                          illegal              : Map[String, Invalidity],
                          excludeFromSuggestion: Set[ReqId]) {

    lazy val legalm: Map[String, ReqItem] =
      legal.iterator.map(_.mapStrengthL(_.pubidStrNorm)).toMap

    lazy val suggestions: ReqItems =
      if (excludeFromSuggestion.isEmpty)
        legal
      else
        legal.filter(i => !excludeFromSuggestion.contains(i.reqId))

    def outlaw(isBad: ReqItem => Boolean, rej: ReqItem => Invalidity): Lookup = {
      val (ko, ok) = legal.partition(isBad)
      val illegal2 = ko.foldLeft(illegal)((m, i) => m.updated(i.pubidStrNorm, rej(i)))
      Lookup(ok, illegal2, excludeFromSuggestion)
    }

    def dontSuggest(reqId: ReqId): Lookup =
      copy(excludeFromSuggestion = excludeFromSuggestion + reqId)
  }

  object Lookup {
    def all(p: Project, pt: PlainText.ForProject.AnyCtx): Lookup =
      Lookup(AutoComplete.Project.reqItems(p, pt), UnivEq.emptyMap, UnivEq.emptySet)

    def forCustomColumn(p: Project, l: Lookup, fid: CustomField.Implication.Id): Lookup = {
      val f = p.config.fields.custom(fid)
      l.outlaw(_.reqType.reqTypeId !=* f.reqTypeId, i => Invalidity(i.pubidStr + " is not applicable in this column"))
    }
  }

  implicit def univEqLookup: UnivEq[Lookup] =
    UnivEq.derive

  def initialValue(p: Project, dir: Direction, id: ReqId): Vector[Pubid] =
    MutableArray(p.content.implications(dir)(id))
      .map(p.content.reqs.need(_).pubid)
      .sortBySchwartzian(p.dataLogic.pubidSortKeyFn)
      .iterator()
      .to(Vector)

  def initialValueAndText(initial: Option[(ReqId, Iterable[Pubid])], p: Project, l: Lookup): (Set[ReqId], String) = {
    val reqs: Iterable[Req] = {
      val legal = initial.foldLeft(l.legal.map(_.reqId).toSet)(_ - _._1)
      initial match {
        case Some((_, pubIds)) => pubIds.view.map(p.content.reqs.needByPubid).filter(legal contains _.id)
        case None              => Iterable.empty[Req]
      }
    }

    val text =
      Grammar.pubid.seqFormat.merge(
        MutableArray(reqs.iterator.map(r => PlainText.pubid(r.pubid, p)))
          .sort
          .iterator()
      )

    (reqs.map(_.id).toSet, text)
  }

  type Output   = SetDiff.NE[ReqId]
  type CommitFn = Output ~=> Callback

  case class Props(edit            : StateSnapshot[String],
                   lookup          : Lookup,
                   validationFn    : ValidationFn,
                   asyncStatus     : Option[EditorStatus.Async],
                   abort           : Option[Callback],
                   autoFocus       : Boolean,
                   commitFn        : Option[CommitFn],
                   commitVerb      : String,
                   textSearch      : TextSearch,
                   extraKbShortcuts: KeyboardTheme.Shortcuts,
                   showInstructions: Boolean) {

    val parseResult = validationFn(lookup)(edit.value)
    val validated   = PotentialChange.fromDisjunction(parseResult).ignoreEmpty
    def commit      = (r: Output) => commitFn.map(_ apply r)
    val status      = asyncStatus getOrElse EditorStatus.fromValidatedChange(validated)(commit, abort)

    @inline def render: VdomElement = Component(this)
  }

  type ValidationFn = Lookup => Simple.Validator[String, _, SetDiff[ReqId]]

  private def validator1(l: Lookup): Validator[String, List[String], List[ReqId]] = {
    val parse: Auditor[String, ReqId] =
      Auditor(s =>
        l.legalm.get(s).map(_.reqId.right) orElse
          l.illegal.get(s).map(-\/.apply) getOrElse
          -\/(Invalidity("Invalid: " + s)))
    Grammar.pubid.seqFormat.validator(parse)
  }

  private def validator2(p: Project, subject: Option[ReqId], initialValues: Set[ReqId], dir: Direction): Auditor[Set[ReqId], SetDiff[ReqId]] =
    Auditor { in =>
      val newValues = subject.foldLeft(in)(_ - _) // Tolerate reflexivity
      val diff = SetDiff.compare(initialValues, newValues)

      val pi = p.content.implications
      var is = pi(dir)
      for (i <- subject)
        is = is.mod(i, diff.apply)

      if (Implications.cycleDetector.hasCycle(is.m))
        -\/(Invalidity("That would cause a cycle in your implication graph."))
      else
        \/-(diff)
    }

  def validationFn(p: Project, subject: Option[ReqId], initialValues: Set[ReqId], dir: Direction): ValidationFn =
    l => validator1(l)
      .mapValid(_.toSet)
      .andThenAuditor(validator2(p, subject, initialValues, dir))

  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.EditorBackend {
    private val pxLookup = Px.props($).map(_.lookup).withReuse.autoRefresh
    private val pxTextSearch = Px.props($).map(_.textSearch).withReuse.autoRefresh

    override val pxAutoComplete =
      for {
        l <- pxLookup
        s <- pxTextSearch
      } yield
        AutoComplete.Project.req(l.suggestions, s)(Plain)

    @inline private def lineCardinality = SingleLine

    private val keyHandlerBase =
      KeyHandlers.base(
        autoCompleteKeyHandlers
          + KeyboardTheme.abortCriterion.handleWhenDefined($.props.map(_.abort))
          + KeyboardTheme.commitCO($.props.map(_.status.getCommit))
      )

    val textareaConst: TagMod = {
      val updateState: ReactEventFromTextArea => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.setState(e.target.value.replace("\n", ""))))

      TagMod(
        ^.onBlur    --> autoCompleteOnBlur,
        ^.onClick   ==> autoCompleteOnClick,
        ^.onChange  ==> updateState,
        ^.spellCheck := false,
        RichTextEditor.minRows(lineCardinality))
    }

    def render(p: Props) = {
      def editor(validity: Validity): VdomElement = {
        val keys = keyHandlerBase(p.extraKbShortcuts.keyHandlers)
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
          KeyboardTheme.Instructions(
            p.extraKbShortcuts.instructions ::: KeyboardTheme.Instructions.Clauses.forTextEditor(
              lineCardinality,
              commit = p.status.getCommit,
              commitVerb = p.commitVerb,
              abort = p.abort),
            help = None,
            fullscreen = None,
            monospace = None,
          ))

      EditControlsFeature.renderEditor(p.status, editor, p.edit.value, instructions)
    }

    val onMount: Callback =
      EditControlsFeature.onTextareaEditorMount(editorRef, $.props.map(_.autoFocus)).toCallback
  }

  implicit val reusabilityLookup: Reusability[Lookup] =
    Reusability.byRef[Lookup] || Reusability.byUnivEq

//  implicit val reusabilityProps: Reusability[Props] =
//    Reusability.never // TODO Reusability.derive

  val Component =
    ScalaComponent.builder[Props]
      .renderBackend[Backend]
      .configure(
        //Reusability.shouldComponentUpdate,
        AutoComplete.install)
      .componentDidMount(_.backend.onMount)
      .build
}
