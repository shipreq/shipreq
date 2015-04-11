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
    @inline implicit final def eqAtomVector  [A <: Atom.Generic]: UnivEq[Vector[A]]      = UnivEq.force
    @inline implicit final def eqOptionalText[T <: Text.Generic]: UnivEq[T#OptionalText] = UnivEq.force
    @inline implicit final def eqNonEmptyText[T <: Text.Generic]: UnivEq[T#NonEmptyText] = UnivEq.force
  }

  // ===================================================================================================================

  sealed trait TopBase {
    this: Literal =>
    type Parser <: P.TopBase[this.type]
    def parserI(p: Project)(i: ParserInput): Parser
    def parser (p: Project)(text: String)  : Parser = parserI(p)(P.preprocess(text))

    final def parse(p: Project)(text: String): OptionalText =
      parser(p)(text).optionalText.run().get

    final def parseNonEmpty(p: Project)(text: String): Option[NonEmptyText] =
      parser(p)(text).nonEmptyText.run().toOption
  }

  sealed trait ReqTitle extends Atom.ReqTitle with TopBase {
    override def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
    final class Parser(p: Project, i: ParserInput) extends P.ReqTitle[this.type](this, p, i) {
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(p)(_).inline))
    }
  }

  // ===================================================================================================================
  // Text instances

  object InlineIssueDesc extends TopBase
      with SingleLine
      with ReqRef {

    override def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
    final class Parser(val project: Project, val input: ParserInput) extends P.TopBase(this)
        with P.SingleLine
        with P.ReqRef {

      val token = () => rule(reqRef | singleLine)

      import Grammar.issueDescSurround.{parsing => G}
      val inlineEnd = () => rule(OWS ~ G.suffix)
      def inline: Rule1[NonEmptyText] = rule(G.prefix ~ OWS ~ textUntil(token, inlineEnd) ~ popNEV)
    }
  }

  object RecCodeGroupDesc extends ReqTitle

  object GenericReqDesc extends ReqTitle

  object CustomTextField extends TopBase
      with MultiLine
      with ReqRef
      with Issue
      with TagRef {

    override def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
    final class Parser(val project: Project, val input: ParserInput) extends P.TopBase(this)
        with P.MultiLine
        with P.ReqRef
        with P.Issue
        with P.TagRef {

      def hashToken = rule(hashRef ~ (tagRef | issueRef))
      override protected val additionalTokens = () => rule(hashToken | reqRef)
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(project)(_).inline))
    }
  }
}
