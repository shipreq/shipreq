package shipreq.webapp.feature.publish

import scala.xml.{NodeSeq, Text}
import shipreq.webapp.feature.uc.UseCase
import shipreq.webapp.feature.uc.UseCaseFns.{fullName, reqId}
import shipreq.webapp.feature.uc.step.StepTreeZipper
import shipreq.webapp.feature.uc.text.FreeTextTerms._
import shipreq.webapp.feature.uc.text.{StepText, FreeText, ParsingConfig}
import shipreq.webapp.lib.Misc.DateTimeExt
import shipreq.webapp.lib.ScalazSubset._
import shipreq.webapp.lib.Types._
import ParsingConfig._
import MarkupTokens._

// =====================================================================================================================

object HtmlPublishingBase {
  import NodeSeq.{Empty => zero}

  class HtmlMarkupTokenRenderer(ftt: FTTRenderer[NodeSeq]) extends MarkupTokenAndGapRenderer[NodeSeq] {

    override def markupToken(t: MarkupToken) = t match {
      case BlankLine           => <br/>
      case NonBlankLine(terms) => terms foldMap ftt.term
      case UL(lis)             => <ul>{lis foldMap li}</ul>
    }

    def li(li: LI): NodeSeq =
      <li>{markupTokens(li.content)}</li>

    override def betweenMarkupTokens(a: MarkupToken, b: MarkupToken) =
      a match {
        case BlankLine | UL(_) => zero
        case NonBlankLine(_)   => b match {
          case BlankLine | NonBlankLine(_) => <br/>
          case UL(_)                       => zero
        }
      }
  }

  trait GeneralHtmlFTTs {
    this: FTTRenderer[NodeSeq] =>
    override def fttPlainText(t: PlainText)                 = Text(t.text)
    override def fttDeletedRef                              = <span class="bad ref">{DeletedRefStr}</span>
    override def fttInvalidStepRef(t: InvalidStepRef)       = <span class="bad ref">{makeInvalidStepRef(t.label)}</span>
    override def fttInvalidUseCaseRef(t: InvalidUseCaseRef) = <span class="bad ref">{makeInvalidUseCaseRef(t.num, t.title)}</span>
    override def fttMathTex(value: MathTexTerm)             = <script type="math/tex">{value.tex}</script>
  }

  class HtmlStepFlowRenderer(override val flowRefSep: NodeSeq) extends StepFlowRenderer[NodeSeq] {
    override def flowClause(arrow: String, refs: NodeSeq) = <span class="flow"> {arrow} {refs}</span>
  }
}

import HtmlPublishingBase._

// =====================================================================================================================

object HtmlFieldValuePublishers {

  object lr extends LinkRenderer[NodeSeq] {
    override def stepRef(l: StepLabel)                       = <span class="wouldbelink step">[{l}]</span>
    override def usecaseRef(n: UseCaseNumber, title: String) = <span class="wouldbelink uc">[{fullName(n, title)}]</span>
  }

  object fttRenderer extends FTTRenderer[NodeSeq] with UseLinkRendererInFTTs[NodeSeq] with GeneralHtmlFTTs {
    override val linkRenderer = lr
  }

  implicit val markupTokenRenderer = new HtmlMarkupTokenRenderer(fttRenderer)

  implicit val stepRenderer = new TypicalStepRenderer(lr, new HtmlStepFlowRenderer(Text(" ")))

  def textField(v: FreeText): NodeSeq = Helpers.markupAndRenderFTTs(v.terms)
  def stepField(v: StepText): NodeSeq = stepRenderer.step(v)
}

// =====================================================================================================================

object HtmlPublisher extends Publisher[NodeSeq] {
  override def publish(input: Input) = new HtmlPublisher(input).doc

  @inline def ucId(n: UseCaseNumber) = reqId(n)
  @inline def ucHref(n: UseCaseNumber) = "#" + ucId(n)
  @inline def stepId(lbl: StepLabel) = "step-" + lbl.replace('.','_')
  @inline def stepHref(lbl: StepLabel) = "#" + stepId(lbl)

  object HtmlLinkRenderer extends LinkRenderer[NodeSeq] {
    override def stepRef(l: StepLabel) =
      <a class="step" href={stepHref(l)}>{l}</a>

    override def usecaseRef(num: UseCaseNumber, title: String) =
      <a class="uc" title={fullName(num, title)} href={ucHref(num)}>{reqId(num)}</a>
  }

  def fttRendererWithScopedUcs(inScopeFn: UseCaseNumber => Boolean, lr: LinkRenderer[NodeSeq]): FTTRenderer[NodeSeq] =
    new FTTRenderer[NodeSeq] with SeparateUcRefsByScopeAndUseLinkRenderer[NodeSeq] with GeneralHtmlFTTs {
      override def linkRenderer = lr
      override def inScope(num: UseCaseNumber) = inScopeFn(num)
      override def fttUseCaseRefOutOfScope(t: UseCaseRef) =
        <span class="uc outofscope">{reqId(t.num)} <sup>({t.title})</sup></span>
    }

  val stepFlowRenderer = new HtmlStepFlowRenderer(Text(", "))
}

class HtmlPublisher(input: Input) extends GenericPublisher(input) {
  import HtmlPublisher.{publish => _, _}

  override type X = NodeSeq
  override def xMonoid = implicitly
  implicit val linkRenderer = HtmlLinkRenderer
  override implicit val fttRenderer = fttRendererWithScopedUcs(inScope, linkRenderer)
  override implicit val markupTokenRenderer = new HtmlMarkupTokenRenderer(fttRenderer)
  override implicit val stepRenderer = new TypicalStepRenderer(linkRenderer, stepFlowRenderer)(xMonoid, markupTokenRenderer)

  // -------------------------------------------------------------------------------------------------------------------
  // High-level

  override def docHeader(title: X, preface: X): X = <header>{title}{preface}</header>
  override def docHeaderTitle(t: String) = <h1>{t}</h1>
  override def docHeaderPreface(p: X) = <p>{p}</p>

  override def docLastUpdated(t: String @@ ISO8601) =
    <p class="last-updated">Last updated: <time class="showdatetime" datetime={t}></time></p>

  override def tocSurround(entries: X) =
    <nav>
      <h2>Table of Contents</h2>
      <ul class="toc">{entries}</ul>
    </nav>

  override def tocEntry(uc: UseCase) =
    <li><a href={ucHref(uc)}>{uc.fullName}</a></li>

  override def articles(articles: List[X]) =
    <main>{super.articles(articles)}</main>

  override def article(uc: UseCase, fields: X) =
    <article id={ucId(uc)}>
      <header>
        <h2>{uc.fullName}</h2>
      </header>
      <table class="fvs"><tbody class="fvs">{fields}</tbody></table>
    </article>

  // -------------------------------------------------------------------------------------------------------------------
  // Fields

  override def fieldTitle(title: String) = Text(title)

  // -------------------------------------------------------------------------------------------------------------------
  // Text field

  override def textFieldSurround(title: X, value: X) = <tr>{title}{value}</tr>
  override def textFieldTitleSurround(title: X)      = <th>{title}</th>
  override def textFieldValueSurround(value: X)      = <td class="fvpub">{value}</td>

  // -------------------------------------------------------------------------------------------------------------------
  // Step fields

  override def stepFieldSurround(title: X, value: X) = <tr>{title}{value}</tr>
  override def stepFieldTitleSurround(title: X)      = <th>{title}</th>
  override def stepFieldValueEmpty                   = <td></td>
  override def stepFieldValueSurround(value: X)      = <td class="steps fvpub">{value}</td>

  override def stepTreeGenSurround(level: Int, gen: X) = {
    val c = "lvl-" + level
    val x = <table class={c}>{gen}</table>
    if (level == 0)
      x
    else
      <tr class="ind"><td colspan="2">{x}</td></tr>
  }

  override def stepTreeNoChildren(step: StepTreeZipper.AnyFocus, stepLeader: String, text: X) =
    <tr id={stepId(step.label)}><th>{stepLeader}</th><td>{text}</td></tr>

  override def stepTreeWithChildren(step: StepTreeZipper.AnyFocus, stepLeader: String, text: X, children: X) =
    stepTreeNoChildren(step, stepLeader, text) ++ children

  // -------------------------------------------------------------------------------------------------------------------
  // Other fields

  override def revisionField(f: OF_Revision) =
    <tr class="rev"><th>Revision</th><td>{f.rev}</td></tr>

  override def lastUpdatedField(f: OF_LastUpdated) =
    <tr class="lastupdated"><th>Last Updated</th><td><time class="showdate" datetime={f.when.toIso8601Str}></time></td></tr>

  override def flowGraphField(f: OF_FlowGraph) =
    <tr class="flowgraph"><th>Flow Graph</th><td data-dot={f.dot.value}></td></tr>
}
