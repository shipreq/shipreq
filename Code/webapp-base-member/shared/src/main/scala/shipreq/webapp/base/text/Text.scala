package shipreq.webapp.base.text

import org.parboiled2._
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Atom => A, MultiLine => ML, Parsers => P}

object Text {

  object Equality extends Equality
  trait Equality {
    @inline implicit final def univEqAnyAtom      [A <: Atom.AnyAtom]: UnivEq[A]         = UnivEq.force
    @inline implicit final def univEqAnyAtomVector[A <: Atom.AnyAtom]: UnivEq[Vector[A]] = UnivEq.force
  }

  type Generic = Base with A.Literal
  type AnyOptional = A.Base#OptionalText
  type AnyNonEmpty = A.Base#NonEmptyText

  /** Plain text, or rich text? */
  def isPlain(t: AnyOptional): Boolean =
    t.forall(_.isPlain)

  @inline def isRich(t: AnyOptional): Boolean =
    !isPlain(t)

  // ===================================================================================================================

  sealed abstract class Base {
    this: A.Literal =>

    val lineCardinality: LineCardinality

    type Parser <: P.TopBase[this.type]

    protected[text] def parserI(p: Project)(i: ParserInput): Parser

    final def parser(p: Project)(text: String): Parser =
      parserI(p)(P.preProcessor(lineCardinality)(text).value)

    final def parse(p: Project)(text: String): OptionalText =
      parser(p)(text).optionalText.run().get

    final def parseNonEmpty(p: Project)(text: String): Option[NonEmptyText] =
      parser(p)(text).nonEmptyText.run().toOption
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  // Text instances

  // After changing the structure of a text type, also update the following:
  // - AtomTC & TextTC
  // - Parsing rules in top-level text objects and Parsers
  // - RandomData

  sealed abstract class ReqTitle extends Base with A.ReqTitle {
    this: A.Literal =>

    final class Parser(val project: Project, val input: ParserInput) extends P.TopBase[this.type](this)
        with P.SingleLine
        with P.Issue
        with P.ReqRef
        with P.UseCaseStepRef
        with P.TagRef {

      def hashToken =                         rule(hashRef ~ (tagRef | issueRef))
      val token = () =>                       rule(hashToken | useCaseStepRef | reqRef | singleLine)
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(project)(_).inline))
    }

    override protected[text] def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /**
   * A "generic-req title", not a "generic req-title".
   * Title of a [[shipreq.webapp.base.data.GenericReq]].
   */
  object GenericReqTitle extends ReqTitle

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object InlineIssueDesc extends Base
      with A.SingleLine
      with A.ReqRef
      with A.UseCaseStepRef {

    final class Parser(val project: Project, val input: ParserInput) extends P.TopBase(this)
        with P.SingleLine
        with P.ReqRef
        with P.UseCaseStepRef {

      import Grammar.issueDescSurround.{parsing => G}
      val token     = () =>             rule(useCaseStepRef | reqRef | singleLine)
      val inlineEnd = () =>             rule(OWS ~ G.suffix)
      def inline: Rule1[NonEmptyText] = rule(G.prefix ~ OWS ~ textUntil(token, inlineEnd) ~ popNEV)
    }

    override protected[text] def parserI(p: Project)(i: ParserInput) = new Parser(p, i)

    /** Issue descs that demonstrate all types of inner atoms. */
    def demo(reqId: ReqId, reqCodeId: ReqCodeId, useCaseStepId: UseCaseStepId): NonEmptyVector[NonEmptyText] =
      NonEmptyVector(
        NonEmptyVector(
          Literal("Need to finish "), ReqRef(reqId),
          Literal(", "), UseCaseStepRef(useCaseStepId),
          Literal(" and "), CodeRef(reqCodeId)),
        NonEmptyVector(Literal("Ask "), EmailAddress("bob@gmail.com"), Literal(" about "), MathTeX("e=mc^2")))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  // Doesn't extend ReqTitle because Tags are prohibited in RCGs.
  object CodeGroupTitle extends Base
      with A.SingleLine
      with A.Issue
      with A.ReqRef
      with A.UseCaseStepRef {

    final class Parser(val project: Project, val input: ParserInput) extends P.TopBase(this)
        with P.SingleLine
        with P.Issue
        with P.ReqRef
        with P.UseCaseStepRef {

      def hashToken =                         rule(hashRef ~ issueRef)
      override val token = () =>              rule(hashToken | useCaseStepRef | reqRef | singleLine)
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(project)(_).inline))
    }

    override protected[text] def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object CustomTextField extends Base
      with A.MultiLine
      with A.Issue
      with A.ReqRef
      with A.UseCaseStepRef
      with A.TagRef {

    final class Parser(val project: Project, val input: ParserInput) extends P.TopBase(this)
        with P.MultiLine
        with P.Issue
        with P.ReqRef
        with P.UseCaseStepRef
        with P.TagRef {

      def hashToken =                                 rule(hashRef ~ (tagRef | issueRef))
      override protected val additionalTokens = () => rule(hashToken | useCaseStepRef | reqRef)
      override protected def issueInnerDesc =         rule(runSubParser(InlineIssueDesc.parserI(project)(_).inline))
    }

    override protected[text] def parserI(p: Project)(i: ParserInput) = new Parser(p, i)

    /** A text value that demonstrates all types of atoms. */
    def demo(reqId: ReqId, reqCodeId: ReqCodeId, useCaseStepId: UseCaseStepId, tagId: ApplicableTagId, issue: CustomIssueTypeId): NonEmptyText = {
      var uls = NonEmptyVector[ListItem](
        Vector(Literal("Req: "), ReqRef(reqId)),
        Vector(Literal("UC Step Req: "), UseCaseStepRef(useCaseStepId)),
        Vector(Literal("Code: "), CodeRef(reqCodeId)),
        Vector(Literal("Tag: "), TagRef(tagId)),
        Vector(Literal("Issue(∅): "), Issue(issue, Vector.empty)))
      uls ++= InlineIssueDesc.demo(reqId, reqCodeId, useCaseStepId).map(desc =>
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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object DeletionReason extends Base
      with A.MultiLine
      with A.ReqRef
      with A.UseCaseStepRef
      with A.TagRef {

    final class Parser(val project: Project, val input: ParserInput) extends P.TopBase(this)
        with P.MultiLine
        with P.ReqRef
        with P.UseCaseStepRef
        with P.TagRef {

      def hashToken =                                 rule(hashRef ~ tagRef)
      override protected val additionalTokens = () => rule(hashToken | useCaseStepRef | reqRef)
    }

    override protected[text] def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object UseCaseTitle extends ReqTitle

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object UseCaseStep extends Base
      with A.SingleLine
      with A.Issue
      with A.ReqRef
      with A.UseCaseStepRef
      with A.TagRef {

    final class Parser(val project: Project, val input: ParserInput) extends P.TopBase(this)
        with P.SingleLine
        with P.Issue
        with P.ReqRef
        with P.UseCaseStepRef
        with P.TagRef {

      def hashToken =                         rule(hashRef ~ (tagRef | issueRef))
      override val token = () =>              rule(hashToken | useCaseStepRef | reqRef | singleLine)
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(project)(_).inline))
    }

    override protected[text] def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
  }
}
