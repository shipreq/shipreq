package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react._
import vdom.html_<^._
import org.scalajs.dom.html
import scalaz.{-\/, \/-}
import scalaz.syntax.either._
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar, PlainText, SingleLine, TextSearch}
import shipreq.webapp.base.validation._
import shipreq.webapp.base.validation.Simple._
import shipreq.webapp.client.base.data.Plain
import shipreq.webapp.client.base.feature.EditorStatus
import shipreq.webapp.client.base.lib.{KeyboardTheme, AbortCommit => AbortCommit2}
import shipreq.webapp.client.base.ui.{AutosizeTextarea, EditTheme}
import shipreq.webapp.client.project.lib.AutoComplete
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.feature.AutoCompleteFeature
import AutoComplete.ReqItem
import DataImplicits._

object ImplicationEditor {

  case class Lookup(legal: Stream[ReqItem], illegal: Map[String, Invalidity]) {
    lazy val legalm = legal.map(_.mapStrengthL(_.pubidStrNorm)).toMap

    def outlaw(isBad: ReqItem => Boolean, rej: ReqItem => Invalidity): Lookup = {
      val (ko, ok) = legal.partition(isBad)
      val illegal2 = ko.foldLeft(illegal)((m, i) => m.updated(i.pubidStrNorm, rej(i)))
      Lookup(ok, illegal2)
    }
  }

  object Lookup {
    def all(p: Project, pt: PlainText.ForProject): Lookup =
      Lookup(AutoComplete.reqItems(p, pt), UnivEq.emptyMap)

    def forCustomColumn(p: Project, l: Lookup, fid: CustomField.Implication.Id): Lookup = {
      val f = p.config.customField(fid)
      l.outlaw(_.reqType.reqTypeId !=* f.reqTypeId, i => Invalidity(i.pubidStr + " is not applicable in this column"))
    }
  }

  implicit def univEqLookup: UnivEq[Lookup] =
    UnivEq.derive

  def initialValue(p: Project, dir: Direction, id: ReqId): Vector[Pubid] =
    MutableArray(p.implications(dir)(id))
      .map(p.reqs.need(_).pubid)
      .sortBySchwartzian(DataLogic.pubidSortKeyFn(p.config))
      .to[Vector]

  def initialValueAndText(initial: Option[(ReqId, Traversable[Pubid])], p: Project, l: Lookup): (Set[ReqId], String) = {
    val reqs = {
      val legal = initial.foldLeft(l.legal.map(_.reqId).toSet)(_ - _._1)
      initial.fold(Stream.empty[Pubid])(_._2.toStream)
        .map(p.reqs.needByPubid)
        .filter(legal contains _.id)
    }

    val text =
      reqs.map(r => PlainText.pubid(r.pubid, p))
        .sorted |>
        Grammar.pubid.seqFormat.merge

    (reqs.map(_.id).toSet, text)
  }

  type Output      = SetDiff.NE[ReqId]
  type CommitFn    = Output ~=> Callback
  type AbortCommit = Option[AbortCommit2[Callback, CommitFn]]

  case class Props(edit          : StateSnapshot[String],
                   lookup        : Lookup,
                   validationFn  : ValidationFn,
                   asyncStatus   : Option[EditorStatus.Async],
                   abortCommit   : AbortCommit,
                   textSearch    : TextSearch) {

    val parseResult = validationFn(lookup)(edit.value)
    val validated   = PotentialChange.fromDisjunction(parseResult).ignoreEmpty
    def abort       = abortCommit.fold(Callback.empty)(_.abort)
    def commit      = (r: Output) => abortCommit.fold(Callback.empty)(_ commit r)
    val status      = asyncStatus getOrElse EditorStatus.fromValidatedChange(validated)(commit, abort)

    @inline def render: VdomElement = Component(this)
  }

  type ValidationFn = Lookup => Simple.Validator[String, _, SetDiff[ReqId]]

  private def validator1(l: Lookup): Validator[String, Stream[String], Stream[ReqId]] = {
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

      val pi = p.implications
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

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxLookup = Px.props($).map(_.lookup).withReuse.autoRefresh
    private val pxTextSearch = Px.props($).map(_.textSearch).withReuse.autoRefresh

    val pxAutoComplete =
      for {
        l <- pxLookup
        s <- pxTextSearch
      } yield
        AutoComplete.req(s, l.legal, Plain)

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

    private val editorRef = ScalaComponent.mutableRefTo(AutosizeTextarea.Component)

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

  private implicit def reusabilityValidationFn: Reusability[ValidationFn] = Reusability.byRef

  implicit val reusabilityLookup: Reusability[Lookup] =
    Reusability.byRef[Lookup] || Reusability.byUnivEq

//  implicit val reusabilityProps: Reusability[Props] =
//    Reusability.never // TODO Reusability.caseClass

  val Component =
    ScalaComponent.builder[Props]("ImpEditor")
      .renderBackend[Backend]
      .configure(
//        Reusability.shouldComponentUpdate,
        AutoCompleteFeature.installBP(_.backend.getTextarea(), _.pxAutoComplete.value(), _.edit.setState))
      .build
}
