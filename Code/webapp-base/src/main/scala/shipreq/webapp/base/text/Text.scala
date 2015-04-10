package shipreq.webapp.base.text

import org.parboiled2._
import shipreq.base.util.UnivEq
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.text.{Parsers => P}
import Atom.{ReqTitle => _, _}

object Text {
  type Generic = Atom.Base

  object Equality {
    @inline implicit final def eqAtom        [A <: Atom.Generic]: UnivEq[A]              = UnivEq.force
    @inline implicit final def eqOptionalText[T <: Text.Generic]: UnivEq[T#OptionalText] = UnivEq.force
    @inline implicit final def eqNonEmptyText[T <: Text.Generic]: UnivEq[T#NonEmptyText] = UnivEq.force
  }

  // ===================================================================================================================

  sealed trait TopBase {
    this: Literal =>
    type Parser <: P.TopBase[this.type]
    def parserI(p: Project)(i: ParserInput): Parser
    def parser(p: Project)(text: String): Parser =
      parserI(p)(P.preprocess(text))
  }

  sealed trait DefaultOptional extends TopBase {
    this: Literal =>
    type Parser <: P.DefaultOptional[this.type]
    def parse(p: Project)(text: String): OptionalText =
      parser(p)(text).main.run().get
  }

  sealed trait DefaultNonEmpty extends TopBase {
    this: Literal =>
    type Parser <: P.DefaultNonEmpty[this.type]
    def parse(p: Project)(text: String): Option[NonEmptyText] =
      parser(p)(text).main.run().toOption
  }

  sealed trait ReqTitle extends Atom.ReqTitle with DefaultOptional {
    override def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
    final class Parser(p: Project, i: ParserInput) extends P.ReqTitle[this.type](this, p, i) {
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(p)(_).inline))
    }
  }

  // ===================================================================================================================
  // Text instances

  object InlineIssueDesc extends DefaultNonEmpty
    with SingleLine
    with ReqRef {
    override def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
    final class Parser(val project: Project, val input: ParserInput) extends P.DefaultNonEmpty(this)
      with P.SingleLine
      with P.ReqRef {
      val token = () => rule(reqRef | singleLine)

      import Grammar.issueDescSurround.{parsing => G}
      def inlineEnd = rule(ows ~ G.suffix)
      val inlineUntil = () => rule(inlineEnd | token())
      def inline: Rule1[NonEmptyText] = rule(
        G.prefix ~ ows ~ oneOrMore(token() | literalUntil(inlineUntil)) ~ inlineEnd ~> atomsToVector ~ runNEV
      )
    }
  }

  object RecCodeGroupDesc extends ReqTitle

  object GenericReqDesc extends ReqTitle

  object CustomTextField extends MultiLine
    with ReqRef
    with Issue
    with TagRef
}
