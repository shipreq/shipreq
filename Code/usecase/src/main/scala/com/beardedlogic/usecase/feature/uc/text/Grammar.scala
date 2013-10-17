package com.beardedlogic.usecase.feature.uc.text

import java.util.regex.Pattern
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
        case   Success(a, rest)          => Success((sb.toString, a), rest)
        case e@Error(_, _)               => e
        case f@Failure(_,_) if in.atEnd  => f
        case   Failure(_,_) if !in.atEnd => sb += in.first; parse(in.rest)
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
        case   Success(a, rest)                        => Success((sb.toString, Some(a)), rest)
        case e@Error(_, _)                             => e
        case   Failure(_,_) if in.atEnd && !sb.isEmpty => Success((sb.toString, None), in)
        case f@Failure(_,_) if in.atEnd && sb.isEmpty  => f
        case   Failure(_,_) if !in.atEnd               => sb += in.first; parse(in.rest)
      }
    }
    parse(in)
  }

  private def braced[T](inner: Parser[T]): Parser[T] = RefBraceLs ~> inner <~ RefBraceRs

  private def optionallyBraced[T](inner: Parser[T]): Parser[T] = (braced(inner) | inner)

  // -------------------------------------------------------------------------------------------------------------------
  // Free Text

  sealed trait FreeTextToken
  object FreeTextToken {
    case class PlainTextToken(text: String) extends FreeTextToken
    case class StepLabelRefToken(label: StepLabel) extends FreeTextToken
    case class UseCaseRefToken(number: UseCaseNumber, title: Option[String]) extends FreeTextToken
  }

  object FreeTextParsers {
    import FreeTextToken._

    val StepLabel: Parser[String] = {
      val level: Parser[String] = "[A-Za-z]+|\\d+".r
      level ~ rep1("." ~> level) ^^ {case h ~ t => (h :: t).mkString(".")}
    }

    val StepLabelRef: Parser[StepLabelRefToken] = braced(StepLabel) ^^ (s => StepLabelRefToken(s.asLabel))

    val UseCaseRef: Parser[UseCaseRefToken] = braced(
      "UC" ~> opt("-") ~> "\\d+".r ~ opt(":" ~> s"[^${Pattern quote RefBraceRs}]+".r) ^^ {
        case num ~ title => UseCaseRefToken(num.toShort.tag[IsUseCaseNumber], title)
      })

    val Ref: Parser[FreeTextToken] = StepLabelRef | UseCaseRef

    val TextAndRefs: Parser[List[FreeTextToken]] =
      rep(anyTextThenOptional(true, Ref)) ^^ (
        _.foldRight(List.empty[FreeTextToken])((r, b) => {
          var acc = b
          if (r._2.isDefined) acc = r._2.get :: acc
          if (r._1.nonEmpty) acc = PlainTextToken(r._1) :: acc
          acc
        }))
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Step Text

  object FlowParsers {
    import FreeTextParsers._

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
