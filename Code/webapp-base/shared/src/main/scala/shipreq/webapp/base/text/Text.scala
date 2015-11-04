package shipreq.webapp.base.text

import org.parboiled2._
import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Atom => A, Parsers => P}

object Text {

  object Equality extends Equality
  trait Equality {
    @inline implicit final def univEqAnyAtom      [A <: Atom.AnyAtom]: UnivEq[A]         = UnivEq.force
    @inline implicit final def univEqAnyAtomVector[A <: Atom.AnyAtom]: UnivEq[Vector[A]] = UnivEq.force
  }

  type Generic = Base with A.Literal
  type AnyOptional = A.Base#OptionalText
  type AnyNonEmpty = A.Base#NonEmptyText

  // ===================================================================================================================

  sealed abstract class Base {
    this: A.Literal =>

    val multiLine: Boolean
    @inline final def singleLine = !multiLine

    type Parser <: P.TopBase[this.type]
    def parserI(p: Project)(i: ParserInput): Parser
    def parser (p: Project)(text: String)  : Parser = parserI(p)(P.preprocess(text, multiLine))

    final def parse(p: Project)(text: String): OptionalText =
      parser(p)(text).optionalText.run().get

    final def parseNonEmpty(p: Project)(text: String): Option[NonEmptyText] =
      parser(p)(text).nonEmptyText.run().toOption
  }

  sealed trait ReqTitle extends Base with A.ReqTitle {
    override def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
    final class Parser(p: Project, i: ParserInput) extends P.ReqTitle[this.type](this, p, i) {
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(p)(_).inline))
    }
  }

  // ===================================================================================================================
  // Text instances

  // After changing the structure of a text type, also update the following:
  // - AtomTC & TextTC
  // - Parsing rules in top-level text objects and Parsers
  // - RandomData


  /**
   * A "generic-req title", not a "generic req-title".
   * Title of a [[shipreq.webapp.base.data.GenericReq]].
   */
  object GenericReqTitle extends ReqTitle


  object InlineIssueDesc extends Base
      with A.SingleLine
      with A.ReqRef {
    override def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
    final class Parser(val project: Project, val input: ParserInput) extends P.TopBase(this)
        with P.SingleLine
        with P.ReqRef {
      val token = () => rule(reqRef | singleLine)
      import Grammar.issueDescSurround.{parsing => G}
      val inlineEnd = () => rule(OWS ~ G.suffix)
      def inline: Rule1[NonEmptyText] = rule(G.prefix ~ OWS ~ textUntil(token, inlineEnd) ~ popNEV)
    }

    /** Issue descs that demonstrate all types of inner atoms. */
    def demo(reqId: ReqId, reqCodeId: ReqCodeId): NonEmptyVector[NonEmptyText] =
      NonEmptyVector(
        NonEmptyVector(Literal("Need to finish "), ReqRef(reqId), Literal(" and "), CodeRef(reqCodeId)),
        NonEmptyVector(Literal("Ask "), EmailAddress("bob@gmail.com"), Literal(" about "), MathTeX("e=mc^2")))
  }


  object ReqCodeGroupTitle extends Base
      with A.SingleLine
      with A.Issue
      with A.ReqRef {
    override def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
    final class Parser(val project: Project, val input: ParserInput) extends P.TopBase(this)
        with P.SingleLine
        with P.Issue
        with P.ReqRef {
      def hashToken = rule(hashRef ~ issueRef)
      val token = () => rule(hashToken | reqRef | singleLine)
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(project)(_).inline))
    }
  }


  object CustomTextField extends Base
      with A.MultiLine
      with A.Issue
      with A.ReqRef
      with A.TagRef {
    override def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
    final class Parser(val project: Project, val input: ParserInput) extends P.TopBase(this)
        with P.MultiLine
        with P.Issue
        with P.ReqRef
        with P.TagRef {
      def hashToken = rule(hashRef ~ (tagRef | issueRef))
      override protected val additionalTokens = () => rule(hashToken | reqRef)
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(project)(_).inline))
    }

    /** A text value that demonstrates all types of atoms. */
    def demo(reqId: ReqId, reqCodeId: ReqCodeId, tagId: ApplicableTagId, issue: CustomIssueTypeId): NonEmptyText = {
      var uls = NonEmptyVector[ListItem](
        Vector(Literal("Req: "), ReqRef(reqId)),
        Vector(Literal("Code: "), CodeRef(reqCodeId)),
        Vector(Literal("Tag: "), TagRef(tagId)),
        Vector(Literal("Issue(∅): "), Issue(issue, Vector.empty)))
      uls ++= InlineIssueDesc.demo(reqId, reqCodeId).map(desc =>
        Vector(Literal("Issue(∃): "), Issue(issue, desc.whole)))
      uls ++= NonEmptyVector(
        Vector(),
        Vector(Literal("Math: "), MathTeX("""f(x) = {x+1 \over x - 1} + 9\pi^2""")),
        Vector(Literal("Email: "), EmailAddress("blah@google.com")),
        Vector(Literal("Web: "), WebAddress("https://shipreq.com"))
      )

      NonEmptyVector(
        Literal("Atom demonstration."),
        blankLine,
        Literal("Here we go:"),
        UnorderedList(uls))
    }
  }


  object DeletionReason extends Base
      with A.MultiLine
      with A.ReqRef
      with A.TagRef {
    override def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
    final class Parser(val project: Project, val input: ParserInput) extends P.TopBase(this)
        with P.MultiLine
        with P.ReqRef
        with P.TagRef {
      def hashToken = rule(hashRef ~ tagRef)
      override protected val additionalTokens = () => rule(hashToken | reqRef)
    }
  }

}
