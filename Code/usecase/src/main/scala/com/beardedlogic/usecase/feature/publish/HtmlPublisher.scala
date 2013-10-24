package com.beardedlogic.usecase.feature.publish

import com.beardedlogic.usecase.feature.uc.UseCase
import com.beardedlogic.usecase.feature.uc.UseCaseFns.{fullName, reqId}
import com.beardedlogic.usecase.feature.uc.step.{StepNode, StepTreeZipper}
import com.beardedlogic.usecase.feature.uc.text.FreeTextTerms._
import com.beardedlogic.usecase.feature.uc.text.{FlowClause, ParsingConfig, StepText}
import com.beardedlogic.usecase.lib.Types._
import scala.xml.{NodeSeq, Text}
import scalaz.syntax.foldable._
import ParsingConfig._

object HtmlPublisher extends Publisher[NodeSeq] {
  override def publish(input: Input) = new HtmlPublisher(input).doc
}

class HtmlPublisher(input: Input) extends GenericPublisher(input) {
  type X = NodeSeq
  implicit val xMonoid = scalaz.std.nodeseq.nodeSeqInstance

  // TODO UCs wont always be in scope
  @inline private def ucId(n: UseCaseNumber) = reqId(n)
  @inline private def ucHref(n: UseCaseNumber) = "#" + ucId(n)

  @inline private def stepId(lbl: StepLabel) = "step-" + lbl.replace('.','_')
  @inline private def stepHref(lbl: StepLabel) = "#" + stepId(lbl)

  // -------------------------------------------------------------------------------------------------------------------
  // High-level

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

  def plainText(value: String): X =
    value.split('\n').toList.map(t => Text(t): NodeSeq).intercalate(<br/>) // TODO guard against \r?

  override def fttPlainText(t: PlainText)                 = plainText(t.text)
  override def fttStepRef(t: StepRef)                     = stepRef(t.label)
  override def fttAnyUseCaseRef(t: AnyUseCaseRef)         = <a class="uc" title={fullName(t.num, t.title)} href={ucHref(t.num)}>{reqId(t.num)}</a>
  override def fttDeletedRef                              = <span class="bad ref">{DeletedRefStr}</span>
  override def fttInvalidStepRef(t: InvalidStepRef)       = <span class="bad ref">{makeInvalidStepRef(t.label)}</span>
  override def fttInvalidUseCaseRef(t: InvalidUseCaseRef) = <span class="bad ref">{makeInvalidUseCaseRef(t.num, t.title)}</span>

  def stepRef(l: StepLabel): X = <a class="step" href={stepHref(l)}>{l}</a>

  // -------------------------------------------------------------------------------------------------------------------
  // Step fields

  override def stepFieldSurround(title: X, value: X) = <tr>{title}{value}</tr>
  override def stepFieldTitleSurround(title: X)      = <th>{title}</th>
  override def stepFieldValueEmpty                   = <td></td>
  override def stepFieldValueSurround(value: X)      = <td class="steps">{value}</td>

  override def stepFieldValueGenerationSurround(level: Int, generation: X) = {
    val c = "lvl-" + level
    val x = <table class={c}>{generation}</table>
    if (level == 0)
      x
    else
      <tr class="ind"><td colspan="2">{x}</td></tr>
  }

  override def stepFieldValueStep(value: StepTreeZipper.AnyFocus) = {
    val n: StepNode = value.node
    val v: StepText = value.value
    val l = if (value.level == 0) value.label else n.label
    <tr id={stepId(value.label)}><th>{l}.</th><td>{stepText(v)}</td></tr>
  }

  override def flowClause(c: FlowClause) =
    <span class="flow"> {c.flowObj.style.arrow} {c.sortedLabels.toList map flowRef intercalate flowRefSep}</span>

  def flowRef(l: StepLabel): X = stepRef(l)
  def flowRefSep: X = Text(", ")

  // -------------------------------------------------------------------------------------------------------------------
  // Other fields

  def flowGraphField(f: LogicalFlowGraphField) =
    <tr class="flowgraph"><th>Flow Graph</th><td data-dot={f.dot.value}></td></tr>
}
