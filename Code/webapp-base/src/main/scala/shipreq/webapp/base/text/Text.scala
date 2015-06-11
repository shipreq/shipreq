package shipreq.webapp.base.text

import org.parboiled2._
import shipreq.base.util.UnivEq
import shipreq.webapp.base.data.{ApplicableTagId, Project}
import shipreq.webapp.base.text.{Atom => A, Parsers => P}

object Text {

  object Equality {
    @inline implicit final def eqAnyAtom      [A <: Atom.AnyAtom]: UnivEq[A]         = UnivEq.force
    @inline implicit final def eqAnyAtomVector[A <: Atom.AnyAtom]: UnivEq[Vector[A]] = UnivEq.force
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

  sealed trait ReqTitle extends Base with Atom.ReqTitle {
    override def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
    final class Parser(p: Project, i: ParserInput) extends P.ReqTitle[this.type](this, p, i) {
      override protected def issueInnerDesc = rule(runSubParser(InlineIssueDesc.parserI(p)(_).inline))
    }
  }

  // ===================================================================================================================
  // Text instances

  // After changing the structure of a text type, also update the following:
  // - Codecs
  // - Parsing rules in top-level text objects and Parsers
  // - RandomData

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
  }

  object ReqCodeGroupTitle extends ReqTitle

  /** Title of a [[shipreq.webapp.base.data.GenericReq]]. Not a generic req-title. */
  object GenericReqTitle extends ReqTitle

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
  }

  // ===================================================================================================================
  // Utilities

  def findTags[T <: Atom.TagRef](text: Vector[T#Atom], into: Set[ApplicableTagId] = UnivEq.emptySet): Set[ApplicableTagId] =
    text.foldLeft(into)((q, a) => a match {
      case t: Atom.TagRef#TagRef => q + t.value
      case _                     => q
    })

}
