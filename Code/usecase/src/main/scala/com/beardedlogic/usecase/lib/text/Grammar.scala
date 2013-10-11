package com.beardedlogic.usecase.lib.text

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers
import com.beardedlogic.usecase.lib.Types._
import ParsingConfig._

/**
 * These parsers expresses the syntax that enables various special features to sprout from plain UC text.
 *
 * @since 15/05/2013
 */
object Grammar extends RegexParsers {

  // Gobbles whitespace. In plain-text we need whitespace preserved.
  @inline private def gobbleWhitespace(sb: StringBuilder, _in: Input) = {
    var in = _in
    while (!in.atEnd && Character.isWhitespace(in.first)) {
      sb += in.first
      in = in.rest
    }
    in
  }

  /**
   * Non-greedily matches 0-n characters, followed by another given matcher.
   *
   * (In-built parsers are all greedy; even ".*?".r won't work.)
   *
   * @param keepText If true, whitespace at the end of the text will be preserved.
   * @param nextParser The parser that matches after this. It must succeed for this to stop.
   * @tparam T The type of the next parser.
   * @return A tuple of the characters collected here (can be an empty string), and the result of nextParser.
   */
  private def anyTextThen[T](keepText: Boolean, nextParser: Parser[T]) = Parser[(String, T)] { in =>
    val sb = new StringBuilder
    @tailrec def parse(_in: Input): ParseResult[(String, T)] = {
      val in = if (keepText) gobbleWhitespace(sb, _in) else _in
      nextParser(in) match {
        case Success(a, rest) => Success((sb.toString, a), rest)
        case e@Error(_, _)    => e // still have to propagate error
        case _ if (in.atEnd)  => Failure("end of input", in)
        case _                => sb += in.first; parse(in.rest)
      }
    }
    parse(in)
  }

  /**
   * Non-greedily matches 0-n characters, optionally followed by another given matcher.
   *
   * In-built parsers are all greedy; even ".*?".r won't work.
   *
   * @param keepText If true, whitespace at the end of the text will be preserved.
   * @param nextParser The parser that may match after this. If it succeeds, this stops; else this will collect the
   *                   entire string.
   * @tparam T The type of the next parser.
   * @return A tuple of the characters collected here (can be an empty string), and the result of nextParser.
   */
  private def anyTextThenOptional[T](keepText: Boolean, nextParser: Parser[T]) = Parser[(String, Option[T])] { in =>
    val sb = new StringBuilder
    @tailrec def parse(_in: Input): ParseResult[(String, Option[T])] = {
      val in = if (keepText) gobbleWhitespace(sb, _in) else _in
      nextParser(in) match {
        case Success(a, rest) => Success((sb.toString, Some(a)), rest)
        case e@Error(_, _)    => e // still have to propagate error
        case _ if (in.atEnd)  => Success((sb.toString, None), in)
        case _                => sb += in.first; parse(in.rest)
      }
    }
    parse(in)
  }

  private def braced[T](inner: Parser[T]): Parser[T] = RefBraceLs ~> inner <~ RefBraceRs

  private def optionallyBraced[T](inner: Parser[T]): Parser[T] = (braced(inner) | inner)

  // -------------------------------------------------------------------------------------------------------------------
  // Free Text

  // Denotes the label of a step. Eg. "1.2", "3.E.2.a"
  val StepLabel: Parser[String] = {
    val level: Parser[String] = "[A-Za-z]+|\\d+".r
    level ~ rep1("." ~> level) ^^ {case h ~ t => (h :: t).mkString(".")}
  }

  /**
   * Matches Text and the first step reference. If no refs, then matches the entire input as Text.
   */
  val TextAndPossibleRef: Parser[(String, Option[String])] = anyTextThenOptional(true, braced(StepLabel))

  // -------------------------------------------------------------------------------------------------------------------
  // Step Text

  object FlowParsers {
    val ValidRef: Parser[PotentiallyValidRef] = optionallyBraced(StepLabel) ^^ {
      case r => PotentiallyValidRef(r.asLabel)
    }
    val InvalidRef: Parser[InvalidRefToken] = "\\[[A-Za-z0-9 .?]+\\]".r ^^ {
      case token => InvalidRefToken(token)
    }
    val AnyRef: Parser[RefToken] = ValidRef | InvalidRef
    val FlowRefList: Parser[List[RefToken]] = rep1sep(AnyRef, "," ?)

    def flowClause(style: FlowStyle): Parser[ParsedFlowClause] = style.arrowRegex ~> FlowRefList ^^ {
      case refs => ParsedFlowClause(style, refs)
    }
    val FlowFromClause: Parser[ParsedFlowClause] = flowClause(FlowFromStyle)
    val FlowToClause: Parser[ParsedFlowClause] = flowClause(FlowToStyle)
    val FlowClause: Parser[ParsedFlowClause] = FlowFromClause | FlowToClause

    sealed trait RefToken
    case class PotentiallyValidRef(label: StepLabel) extends RefToken
    case class InvalidRefToken(token: String) extends RefToken

    case class ParsedFlowClause(style: FlowStyle, refs: List[RefToken])

    /**
     * Parsers flow clauses from text. Anything left of the flow clauses is considered free-text and not parsed by this
     * parser.
     */
    val TextAndFlowClauses: Parser[(String, List[ParsedFlowClause])] = anyTextThen(false, rep1(FlowClause))
  }
}
