package com.beardedlogic.usecase.feature.publish

import scala.xml.{NodeSeq, Text}
import com.beardedlogic.usecase.feature.uc.UseCase
import com.beardedlogic.usecase.feature.uc.UseCaseFns.{fullName, reqId}
import com.beardedlogic.usecase.feature.uc.step.StepTreeZipper
import com.beardedlogic.usecase.feature.uc.text.FreeTextTerms._
import com.beardedlogic.usecase.feature.uc.text.ParsingConfig
import com.beardedlogic.usecase.lib.Misc.DateTimeExt
import com.beardedlogic.usecase.lib.ScalazSubset._
import com.beardedlogic.usecase.lib.Types._
import ParsingConfig._
import MarkupTokens._

object HtmlPublisher extends Publisher[NodeSeq] {
  override def publish(input: Input) = new HtmlPublisher(input).doc
}

class HtmlPublisher(input: Input) extends GenericPublisher(input) {
  type X = NodeSeq
  override def xMonoid = scalaz.std.nodeseq.nodeSeqInstance

  @inline private def ucId(n: UseCaseNumber) = reqId(n)
  @inline private def ucHref(n: UseCaseNumber) = "#" + ucId(n)

  @inline private def stepId(lbl: StepLabel) = "step-" + lbl.replace('.','_')
  @inline private def stepHref(lbl: StepLabel) = "#" + stepId(lbl)

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
  override def textFieldValueSurround(value: X)      = <td>{value}</td>

  override def markupToken(t: MarkupToken) = t match {
    case BlankLine           => <br/>
    case NonBlankLine(terms) => terms foldMap term
    case UL(lis)             => <ul>{lis foldMap li}</ul>
  }

  def li(li: LI): X = <li>{markupTokens(li.content)}</li>

  override def betweenMarkupTokens(a: MarkupToken, b: MarkupToken): X =
    a match {
      case BlankLine | UL(_) => zero
      case NonBlankLine(_)   => b match {
        case BlankLine | NonBlankLine(_) => <br/>
        case UL(_)                       => zero
      }
    }

  override def fttPlainText(t: PlainText)                 = Text(t.text)
  override def fttStepRef(t: StepRef)                     = stepRef(t.label)
  override def fttUseCaseRefInScope(t: AnyUseCaseRef)     = <a class="uc" title={fullName(t.num, t.title)} href={ucHref(t.num)}>{reqId(t.num)}</a>
  override def fttUseCaseRefOutOfScope(t: UseCaseRef)     = <span class="uc outofscope">{reqId(t.num)} <sup>({t.title})</sup></span>
  override def fttDeletedRef                              = <span class="bad ref">{DeletedRefStr}</span>
  override def fttInvalidStepRef(t: InvalidStepRef)       = <span class="bad ref">{makeInvalidStepRef(t.label)}</span>
  override def fttInvalidUseCaseRef(t: InvalidUseCaseRef) = <span class="bad ref">{makeInvalidUseCaseRef(t.num, t.title)}</span>
  override def fttMathTex(value: MathTexTerm)             = <script type="math/tex">{value.tex}</script>

  def stepRef(l: StepLabel): X = <a class="step" href={stepHref(l)}>{l}</a>

  // -------------------------------------------------------------------------------------------------------------------
  // Step fields

  override def stepFieldSurround(title: X, value: X) = <tr>{title}{value}</tr>
  override def stepFieldTitleSurround(title: X)      = <th>{title}</th>
  override def stepFieldValueEmpty                   = <td></td>
  override def stepFieldValueSurround(value: X)      = <td class="steps">{value}</td>

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

  override def flowClause(arrow: String, refs: X) = <span class="flow"> {arrow} {refs}</span>
  override def flowRef(l: StepLabel)              = stepRef(l)
  override val flowRefSep                         = Text(", ")

  // -------------------------------------------------------------------------------------------------------------------
  // Other fields

  override def revisionField(f: OF_Revision) =
    <tr class="rev"><th>Revision</th><td>{f.rev}</td></tr>

  override def lastUpdatedField(f: OF_LastUpdated) =
    <tr class="lastupdated"><th>Last Updated</th><td><time class="showdate" datetime={f.when.toIso8601Str}></time></td></tr>

  override def flowGraphField(f: OF_FlowGraph) =
    <tr class="flowgraph"><th>Flow Graph</th><td data-dot={f.dot.value}></td></tr>
}
