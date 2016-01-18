package shipreq.webapp.client.widgets

import japgolly.scalajs.react.extra._
import japgolly.scalajs.react._, vdom.prefix_<^._
import org.scalajs.dom
import scalaz.{\/, \/-, -\/}
import scalaz.syntax.either._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{SetDiff, UnivEq, univEqOps}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar, PlainText, TextSearch}
import shipreq.webapp.base.validation.{ValidationPartU, VFailure, ValidationResult, Validator}
import shipreq.webapp.client.app.reqtable.Column
import shipreq.webapp.client.data.Plain
import shipreq.webapp.client.lib.AutoComplete
import shipreq.webapp.client.lib.DataReusability._
import shipreq.webapp.client.feature.{EditValidationFeature, AutoCompleteFeature}
import AutoComplete.ReqItem
import DataImplicits._

object ImplicationEditor {

  case class Lookup(legal: Stream[ReqItem], illegal: Map[String, String]) {
    lazy val legalm = legal.map(_.mapStrengthL(_.pubidStrNorm)).toMap

    def outlaw(isBad: ReqItem => Boolean, rej: ReqItem => String): Lookup = {
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
      l.outlaw(_.reqType.reqTypeId !=* f.reqTypeId, _.pubidStr + " is not applicable in this column")
    }
  }

  implicit def univEqLookup: UnivEq[Lookup] =
    UnivEq.derive

  def initialValueForCustomColumn(p: Project, fid: CustomField.Implication.Id, id: ReqId): Stream[Pubid] =
    p.implications.backwards(id)
      .toStream
      .map(p.reqs.req(_).pubid)

  def initialValueAndText(initial: Option[(ReqId, Seq[Pubid])], p: Project, l: Lookup): (Set[ReqId], String) = {
    val reqs = {
      val legal = initial.foldLeft(l.legal.map(_.reqId).toSet)(_ - _._1)
      initial.fold(Stream.empty[Pubid])(_._2.toStream)
        .map(p.reqs.reqByPubid)
        .filter(legal contains _.id)
    }

    val text =
      reqs.map(r => PlainText.pubid(p, r.pubid))
        .sorted |>
        Grammar.pubid.seqFormat.merge

    (reqs.map(_.id).toSet, text)
  }

  /**
   * If true, the user edits what this subject implies (ie. subject → edit-specified).
   * If false, then it's what implies this subject     (ie. subject ← edit-specified).
   */
  def isDeclFwd(column: Column): Boolean =
    column match {
      case Column.ImplicationTgt => true
      case _                     => false
    }

  /** Extra properties to apply to the tag. */
  type Extra = Option[SetDiff[ReqId]] ~=> TagMod

  case class Props(edit        : ReusableVar[String],
                   lookup      : Lookup,
                   validationFn: ValidationFn,
                   textSearch  : TextSearch,
                   extra       : Extra) {

    val parseResult =
      validationFn.correctAndValidate(lookup, edit.value)

    def render = Component(this)
  }

  type ValidationFn = Validator[Lookup, String, _, SetDiff[ReqId]]

  private val validator1 = {
    def checkEach(l: Lookup, s: String): String \/ ReqId =
      l.legalm.get(s).map(_.reqId.right) orElse
        l.illegal.get(s).map(-\/.apply) getOrElse
        -\/("Invalid: " + s)

    Validator.seqText(Grammar.pubid.seqFormat)(
      (l: Lookup) => s =>
        checkEach(l, s).leftMap(VFailure.looseMsg).validation)
  }

  private def validator2(p: Project, subject: Option[ReqId], initialValues: Set[ReqId], declFwd: Boolean) = {
    val validate: Set[ReqId] => ValidationResult[SetDiff[ReqId]] = in => {
      val newValues = subject.foldLeft(in)(_ - _) // Tolerate reflexivity
      val diff = SetDiff.compare(initialValues, newValues)

      val pi = p.implications
      var is = pi.dir(declFwd)
      for (i <- subject)
        is = is.mod(i, diff.apply)
      val r =
        if (Implications.cycleDetector.hasCycle(is.m))
          -\/(VFailure looseMsg "That would cause a cycle in your implication graph.")
        else
          \/-(diff)
      r.validation
    }
    ValidationPartU.lift(validate)
  }

  def validationFn(p: Project, subject: Option[ReqId], initialValues: Set[ReqId], declFwd: Boolean): ValidationFn =
    validator1
      .map(_.toSet)
      .addValidation(validator2(p, subject, initialValues, declFwd).liftS)

  private val editorRef = Ref[dom.html.Input]("i")

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxLookup = Px.bs($).propsA(_.lookup)
    private val pxTextSearch = Px.bs($).propsA(_.textSearch)

    val pxAutoComplete =
      for {
        l <- pxLookup
        s <- pxTextSearch
      } yield
        AutoComplete.req(s, l.legal, Plain)

    def render(p: Props) = {
      val validated = EditValidationFeature(p.parseResult)

      <.div(
        <.input.text(
          p.extra(validated.validated),
          ^.onChange  ==> ((e: ReactEventI) => p.edit.set(e.target.value)),
          ^.ref        := editorRef,
          ^.value      := p.edit.value),
        validated.renderFailure)
    }
  }

  private implicit def reusabilityValidationFn: Reusability[ValidationFn] = Reusability.byRef

  implicit val reusabilityLookup: Reusability[Lookup] =
    Reusability.byRef[Lookup] || Reusability.byUnivEq

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.caseClass

  val Component =
    ReactComponentB[Props]("ImpEditor")
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .configure(AutoCompleteFeature.installBP(editorRef, _.pxAutoComplete.value(), _.edit.set))
      .build
}
