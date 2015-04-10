package shipreq.webapp.base.text

import org.parboiled2._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.text.{Parsers => P}
import Atom.{ReqTitle => _, _}

object Text {
  type Generic = Atom.Base

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
    final class Parser(p: Project, i: ParserInput) extends P.ReqTitle[this.type](this, p, i)
  }

  // ===================================================================================================================
  // Text instances

  object RecCodeGroupDesc extends ReqTitle

  object GenericReqDesc extends ReqTitle

  object InlineIssueDesc extends DefaultNonEmpty
    with SingleLine
    with ReqRef {
    override def parserI(p: Project)(i: ParserInput) = new Parser(p, i)
    final class Parser(val project: Project, val input: ParserInput) extends P.DefaultNonEmpty(this)
      with P.SingleLine
      with P.ReqRef {
      val token = () => rule(reqRef | singleLine)
    }
  }

  object CustomTextField extends MultiLine
    with ReqRef
    with Issue
    with TagRef
}
