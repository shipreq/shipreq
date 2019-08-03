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

    protected[text] def parserI(p: Project, currentUseCase: Option[ReqTypePos])(i: ParserInput): Parser

    final def parser(p: Project, currentUseCase: Option[ReqTypePos])(text: String): Parser =
      parserI(p, currentUseCase)(P.preProcessor(lineCardinality)(text).value)

    final def parse(p: Project, currentUseCase: Option[ReqTypePos])(text: String): OptionalText =
      parser(p, currentUseCase)(text).optionalText.run().get

    final def parseNonEmpty(p: Project, currentUseCase: Option[ReqTypePos])(text: String): Option[NonEmptyText] =
      parser(p, currentUseCase)(text).nonEmptyText.run().toOption
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  // Text instances

  // After changing the structure of a text type, also update the following:
  // - AtomTC & TextTC
  // - Parsing rules in top-level text objects and Parsers
  // - RandomData

  sealed abstract class ReqTitle extends Base with A.ReqTitle {
    this: A.Literal =>

    final class Parser(override val project       : Project,
                       override val currentUseCase: Option[ReqTypePos],
                       override val input         : ParserInput) extends P.TopBase[this.type](this)
        with P.SingleLine
        with P.Issue
        with P.ContentRef
        with P.TagRef {

      def hashToken =                         rule(hashRef ~ (tagRef | issueRef))
      val token = () =>                       rule(hashToken | contentRef | singleLine)
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(project, currentUseCase)(_).inline))
    }

    override protected[text] def parserI(p: Project, currentUseCase: Option[ReqTypePos])(i: ParserInput) = new Parser(p, currentUseCase, i)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /**
   * A "generic-req title", not a "generic req-title".
   * Title of a [[shipreq.webapp.base.data.GenericReq]].
   */
  object GenericReqTitle extends ReqTitle

  object UseCaseTitle extends ReqTitle

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  // Doesn't extend ReqTitle because Tags are prohibited in RCGs.
  object CodeGroupTitle extends Base
      with A.SingleLine
      with A.Issue
      with A.ContentRef {

    final class Parser(override val project       : Project,
                       override val currentUseCase: Option[ReqTypePos],
                       override val input         : ParserInput) extends P.TopBase(this)
        with P.SingleLine
        with P.Issue
        with P.ContentRef {

      def hashToken =                         rule(hashRef ~ issueRef)
      override val token = () =>              rule(hashToken | contentRef | singleLine)
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(project, currentUseCase)(_).inline))
    }

    override protected[text] def parserI(p: Project, currentUseCase: Option[ReqTypePos])(i: ParserInput) = new Parser(p, currentUseCase, i)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object InlineIssueDesc extends Base
      with A.SingleLine
      with A.ContentRef {

    final class Parser(override val project       : Project,
                       override val currentUseCase: Option[ReqTypePos],
                       override val input         : ParserInput) extends P.TopBase(this)
        with P.SingleLine
        with P.ContentRef {

      import Grammar.issueDescSurround.{parsing => G}
      val token     = () =>             rule(contentRef | singleLine)
      val inlineEnd = () =>             rule(OWS ~ G.suffix)
      def inline: Rule1[NonEmptyText] = rule(G.prefix ~ OWS ~ textUntil(token, inlineEnd) ~ popNEV)
    }

    override protected[text] def parserI(p: Project, currentUseCase: Option[ReqTypePos])(i: ParserInput) = new Parser(p, currentUseCase, i)

    /** Issue descs that demonstrate all types of inner atoms. */
    def demo(reqId        : ReqId,
             reqCodeIdA   : ApReqCodeId,
             reqCodeIdG   : ReqCodeGroupId,
             useCaseStepId: UseCaseStepId): NonEmptyVector[NonEmptyText] =
      NonEmptyVector(
        NonEmptyVector(
          Literal("Need to finish "), ReqRef(reqId),
          Literal(", "), UseCaseStepRef(useCaseStepId),
          Literal(", "), CodeRef(reqCodeIdA),
          Literal(" and "), CodeRef(reqCodeIdG),
        ),
        NonEmptyVector(Literal("Ask "), EmailAddress("bob@gmail.com"), Literal(" about "), MathTeX("e=mc^2")))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object CustomTextField extends Base
      with A.MultiLine
      with A.Issue
      with A.ContentRef
      with A.TagRef {

    final class Parser(override val project       : Project,
                       override val currentUseCase: Option[ReqTypePos],
                       override val input         : ParserInput) extends P.TopBase(this)
        with P.MultiLine
        with P.Issue
        with P.ContentRef
        with P.TagRef {

      def hashToken =                                 rule(hashRef ~ (tagRef | issueRef))
      override protected val additionalTokens = () => rule(hashToken | contentRef)
      override protected def issueInnerDesc =         rule(runSubParser(InlineIssueDesc.parserI(project, currentUseCase)(_).inline))
    }

    override protected[text] def parserI(p: Project, currentUseCase: Option[ReqTypePos])(i: ParserInput) = new Parser(p, currentUseCase, i)

    /** A text value that demonstrates all types of atoms. */
    def demo(reqId        : ReqId,
             reqCodeIdA   : ApReqCodeId,
             reqCodeIdG   : ReqCodeGroupId,
             useCaseStepId: UseCaseStepId,
             tagId        : ApplicableTagId,
             issue        : CustomIssueTypeId): NonEmptyText = {

      var uls = NonEmptyVector[ListItem](
        Vector(Literal("Req: "), ReqRef(reqId)),
        Vector(Literal("UC Step Req: "), UseCaseStepRef(useCaseStepId)),
        Vector(Literal("Code: "), CodeRef(reqCodeIdA)),
        Vector(Literal("Code Group: "), CodeRef(reqCodeIdG)),
        Vector(Literal("Tag: "), TagRef(tagId)),
        Vector(Literal("Issue(∅): "), Issue(issue, Vector.empty)))
      uls ++= InlineIssueDesc.demo(reqId, reqCodeIdA, reqCodeIdG, useCaseStepId).map(desc =>
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

  object UseCaseStep extends Base
      with A.SingleLine
      with A.Issue
      with A.ContentRef
      with A.TagRef {

    final class Parser(override val project       : Project,
                       override val currentUseCase: Option[ReqTypePos],
                       override val input         : ParserInput) extends P.TopBase(this)
        with P.SingleLine
        with P.Issue
        with P.ContentRef
        with P.TagRef {

      def hashToken =                         rule(hashRef ~ (tagRef | issueRef))
      override val token = () =>              rule(hashToken | contentRef | singleLine)
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(project, currentUseCase)(_).inline))
    }

    override protected[text] def parserI(p: Project, currentUseCase: Option[ReqTypePos])(i: ParserInput) = new Parser(p, currentUseCase, i)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object DeletionReason extends Base
      with A.MultiLine
      with A.ContentRef
      with A.TagRef {

    final class Parser(override val project       : Project,
                       override val currentUseCase: Option[ReqTypePos],
                       override val input         : ParserInput) extends P.TopBase(this)
        with P.MultiLine
        with P.ContentRef
        with P.TagRef {

      def hashToken =                                 rule(hashRef ~ tagRef)
      override protected val additionalTokens = () => rule(hashToken | contentRef)
    }

    override protected[text] def parserI(p: Project, currentUseCase: Option[ReqTypePos])(i: ParserInput) = new Parser(p, currentUseCase, i)
  }
}
