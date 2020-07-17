package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty._
import japgolly.microlibs.recursion.Fix
import org.parboiled2.{Parser => _, _}
import scala.util.{Failure, Success, Try}
import shipreq.webapp.base.data
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.data.{Off, On}
import shipreq.webapp.base.filter.Filter.Potential
import shipreq.webapp.base.filter.FilterAst._
import shipreq.webapp.base.util.ParsingUtil._
import shipreq.webapp.base.util.{ParsingUtil, PreProcessed, PreProcessor}

object FilterParser {

  val preProcessor = PreProcessor.singleLine

  def parse(input: String): Result =
    parsePreProcessed(preProcessor(input))

  def parsePreProcessed(input: PreProcessed): Result = {
    val parser = new FilterParser(input.value)
    parseResult(parser.main.run(), parser)
  }

  private def parseResult[A](t: Try[A], parser: FilterParser): Failure \/ A =
    t match {
      case Success(a)             => \/-(a)
      case Failure(e: ParseError) => -\/(ParseException(e, parser.formatError(e, _)))
      case Failure(e: Throwable)  => -\/(GeneralException(e))
    }

  type Result = Failure \/ Option[Potential]

  sealed trait Failure
  final case class GeneralException(t: Throwable) extends Failure
  final case class ParseException(error: ParseError, formatter: ErrorFormatter => String) extends Failure {
    def format: String =
      format(new ErrorFormatter())
    def format(ef: ErrorFormatter): String =
      formatter(ef)
  }

  def parseNumberRange(input: String): Failure \/ NonEmptySet[Int] = {
    val parser = new FilterParser(input)
    parseResult(parser.numberRangeAsFullString.run(), parser)
  }

  val attrChar =
    CharPredicate.AlphaNum ++ CharPredicate.from {
      case '/' | ',' | '-' => true
      case _               => false
    }

  val fieldNameUnquotedChar =
    CharPredicate.from(c => (c != EOI) && FilterAlgebra.isFieldNameUnquotedChar(c))

  // Allows ' / -
  val simpleTextChar =
    CharPredicate("""#:"`()|""".toCharArray).negated -- Whitespace -- EOI

  private val endGap =
    CharPredicate("""#()|""".toCharArray) ++ Whitespace ++ EOI

  private val mkIntSet1: Int => NonEmptySet[Int] =
    NonEmptySet.one[Int]

  private val mkIntSet: (Int, Option[Int]) => NonEmptySet[Int] =
    (a, o) =>
      o.fold(NonEmptySet one a) { b =>
        val s = (if (a < b) a to b else b to a).toSet
        NonEmptySet force s
      }

  private val flattenIntSets1: Seq[NonEmptySet[Int]] => NonEmptySet[Int] =
    _.reduce(_ ++ _)

  private val mkReqsSpec: (Mnemonic, Option[NonEmptySet[Int]]) => Potential.ReqSubset =
    (rt, ons) => ons match {
      case None     => IntensionalReqSet.WholeType(rt)
      case Some(ns) => IntensionalReqSet.SomeOfType(rt, ns)
    }

  private type ImpType = Potential.ReqSet => Potential
  private val mkImplies  : ImpType = i => Potential.impliesAnyOf(i)
  private val mkImpliedBy: ImpType = i => Potential.impliedByAnyOf(i)
  private val mkImplication: (ImpType, Potential.ReqSet) => Potential = _(_)

  private val mkReqs: (Mnemonic, NonEmptySet[Int]) => Potential =
    (m, ns) => Potential.reqs(NonEmptyVector(IntensionalReqSet.SomeOfType(m, ns)))

  private val mkClause: (Potential, Seq[Potential]) => NonEmptyVector[Potential] =
    (h, t) => NonEmptyVector(h, t: _*)

  private val mkImplicitAllOf: NonEmptyVector[Potential] => Potential =
    nev =>
      if (nev.tail.isEmpty)
        nev.head
      else
        Fix(AllOf(nev))
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

@nowarn("msg=Auto-application.*deprecated")
private[filter] class FilterParser(val input: ParserInput) extends ParsingUtil {
  import FilterParser._

  private def OWS = rule(zeroOrMore(Whitespace))

  /** Where this is present, whitespace is required between the current and (most) other FilterSpecs */
  private def end = rule(&(endGap))

  /** 1 or 1-5 */
  def numberRangeElement: Rule1[NonEmptySet[Int]] =
    rule(int1n ~ optional('-' ~ int1n) ~> mkIntSet)

  /** 1,3,5-9,12 */
  def numberRange: Rule1[NonEmptySet[Int]] =
    rule((numberRangeElement + ',') ~> flattenIntSets1)

  def numberRangeAsFullString: Rule1[NonEmptySet[Int]] =
    rule(numberRange ~ EOI)

  /** 1 or {1,3,5-9,12} */
  def numberOrRange: Rule1[NonEmptySet[Int]] =
    rule((int1n ~> mkIntSet1) | ('{' ~ numberRange ~ '}'))

  /** MF or MF-3 or MF-{1,3,5-9,12} */
  def reqsSpec: Rule1[Potential.ReqSubset] =
    rule(reqTypeMnemonicCI ~ optional('-'.? ~ numberOrRange) ~> mkReqsSpec)

  def reqSpecs: Rule1[Potential.ReqSet] =
    rule((reqsSpec + ',') ~ popSeqToNEV[Potential.ReqSubset])

  private case class QuoteRule(char: Char) {
    val charRule: () => Rule0       = () => rule(char)
    def toRule  : Rule1[Potential] =
      rule(charRule() ~!~ nonGreedyCapture(charRule) ~> ((s: String) => Potential.text(s: String, char)))
  }
  private val quote1 = QuoteRule('"')
  private val quote2 = QuoteRule('\'')
  private val quote3 = QuoteRule('`')

  private def quotedText: Rule1[Potential] =
    rule((quote1.toRule | quote2.toRule | quote3.toRule) ~ end)

  private def simpleText: Rule1[Potential] =
    rule(capture(simpleTextChar.+) ~ end ~> ((s: String) => Potential.text(s: String)))

  private def regexChar: Rule0 =
    rule(!'/' ~ '\\'.? ~ ANY)

  private def regex: Rule1[Potential] =
    rule('/' ~!~ capture(regexChar.+) ~!~ '/' ~!~ end ~> ((s: String) => Potential.regex(s.replace("\\/", "/"))))

  private def hashRef: Rule1[Potential] =
    rule(hashRefStr_! ~ end ~> ((s: String) => Potential.hashRef(data.HashRefKey(s))))

  private def reqs: Rule1[Potential] =
    rule(reqTypeMnemonicCS ~ '-'.? ~ numberOrRange ~ end ~> mkReqs)

  private def reqType: Rule1[Potential] =
    rule(reqTypeMnemonicCS ~ end ~> ((i: Mnemonic) => Potential.reqType(i)))

  private def attr: Rule1[String] =
    rule(capture(attrChar.+) ~ end)

  private def field: Rule1[Potential] = {
    def quoteChar: Rule0 = rule('"')
    def name: Rule1[String] = rule(capture(fieldNameUnquotedChar.+) | (quoteChar ~ nonGreedyCapture(() => quoteChar)))
    rule(
      "field:" ~ name ~ '=' ~ attr
        ~> ((f: String, a: String) => Potential.fieldProp(f, a))
    )
  }

  private def presence: Rule1[Potential] =
    rule("has:" ~!~ attr ~> ((i: String) => Potential.presence(i)))

  private def hasIssue: Rule1[Potential] =
    rule("has:issue:" ~!~ ('-' ~ push(Off)).? ~ capture(CharPredicate.Alpha.+) ~!~ zeroOrMore(',' ~!~ capture(CharPredicate.Alpha.+)) ~>
      ((o: Option[On], h: String, t: Seq[String]) => Potential.hasIssue(o.getOrElse(On), h, t: _*)))

  /** implies:MF or impliedBy:FR,CC-1 */
  private def implication: Rule1[Potential] =
    rule("implie" ~ (
      ('s' ~ push(mkImplies)) | ("dBy" ~ push(mkImpliedBy))
      ) ~ ':' ~!~ reqSpecs ~ end ~> mkImplication)

  private def positive: Rule1[Potential] =
    rule(anyOf | allOf | quotedText | regex | hashRef | hasIssue | presence | field | implication | reqs | reqType | simpleText)

  private def negative: Rule1[Potential] =
    rule('-' ~!~ (('-' ~!~ expr) | (expr ~> ((f: Potential) => Potential.not(f)))))

  private def expr: Rule1[Potential] =
    rule(negative | positive)

  private def clause: Rule1[NonEmptyVector[Potential]] =
    rule(OWS ~ expr ~ zeroOrMore(OWS ~ expr) ~ OWS ~> mkClause)

  /** `( a b c )` */
  private def allOf: Rule1[Potential] =
    rule('(' ~!~ clause ~ ')' ~> ((f: NonEmptyVector[Potential]) => Potential(AllOf(f))))

  /** `a b c` - no parens */
  private def allOfImplicit: Rule1[Potential] =
    rule(clause ~> mkImplicitAllOf)

  /** `( a | b | c )` */
  private def anyOf: Rule1[Potential] =
    rule('(' ~ anyOfImplicit ~ ')')

  /** `a | b | c` - no parens */
  private def anyOfImplicit: Rule1[Potential] =
    rule(allOfImplicit ~ oneOrMore('|' ~!~ allOfImplicit)
      ~ popSeqToNEV[Potential] ~> ((a: Potential, b: NonEmptyVector[Potential]) => Potential(AnyOf(a, b))))

  private def mainNonEmpty: Rule1[Potential] =
    rule(anyOfImplicit | (clause ~> mkImplicitAllOf))

  def main: Rule1[Option[Potential]] =
    rule(mainNonEmpty.? ~ EOI)
}
