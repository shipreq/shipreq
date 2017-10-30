package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty._
import japgolly.microlibs.recursion.Fix
import org.parboiled2.{Parser => _, _}
import scala.util.{Failure, Success}
import scalaz.{-\/, \/, \/-}
import shipreq.webapp.base.data
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.util.{ParsingUtil, PreProcessed, PreProcessor}
import FilterAst._
import Filter.Potential
import ParsingUtil._

object FilterParser {

  val preProcessor = PreProcessor.singleLine

  def parse(input: String): Result =
    parsePreProcessed(preProcessor(input))

  def parsePreProcessed(input: PreProcessed): Result = {
    val parser = new FilterParser(input.value)
    parser.main.run() match {
      case Success(f)             => \/-(f)
      case Failure(e: ParseError) => -\/(ParseException(e, parser.formatError(e, _)))
      case Failure(e: Throwable)  => -\/(GeneralException(e))
    }
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

  // Allows ' / -
  val simpleTextChar =
    CharPredicate("""#:"`(){}""".toCharArray).negated -- Whitespace -- EOI

  val attrChar =
    CharPredicate.AlphaNum

  private val endGap =
    CharPredicate("""#(){}""".toCharArray) ++ Whitespace ++ EOI

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

  private val mkMain: Option[NonEmptyVector[Potential]] => Option[Potential] =
    _.map(nev =>
      if (nev.tail.isEmpty)
        nev.head
      else
        Fix(AllOf(nev)))
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
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

  def quotedText: Rule1[Potential] =
    rule((quote1.toRule | quote2.toRule | quote3.toRule) ~ end)

  def simpleText: Rule1[Potential] =
    rule(capture(simpleTextChar.+) ~ end ~> ((s: String) => Potential.text(s: String)))

  def regexChar: Rule0 =
    rule(!'/' ~ '\\'.? ~ ANY)

  def regex: Rule1[Potential] =
    rule('/' ~!~ capture(regexChar.+) ~!~ '/' ~!~ end ~> ((s: String) => Potential.regex(s.replace("\\/", "/"))))

  def hashRef: Rule1[Potential] =
    rule(hashRefStr_! ~ end ~> ((s: String) => Potential.hashRef(data.HashRefKey(s))))

  def reqs: Rule1[Potential] =
    rule(reqTypeMnemonicCS ~ '-'.? ~ numberOrRange ~ end ~> mkReqs)

  def reqType: Rule1[Potential] =
    rule(reqTypeMnemonicCS ~ end ~> ((i: Mnemonic) => Potential.reqType(i)))

  def attr: Rule1[String] =
    rule(capture(attrChar.+) ~ end)

  def presence: Rule1[Potential] =
    rule("has:" ~!~ attr ~> ((i: String) => Potential.presence(i)))

  def lack: Rule1[Potential] =
    rule("no:" ~!~ attr ~> ((i: String) => Potential.lack(i)))

  /** implies:MF or impliedBy:FR,CC-1 */
  def implication: Rule1[Potential] =
    rule("implie" ~ (
      ('s' ~ push(mkImplies)) | ("dBy" ~ push(mkImpliedBy))
      ) ~ ':' ~!~ reqSpecs ~ end ~> mkImplication)

  def positive: Rule1[Potential] =
    rule(allOf | anyOf | quotedText | regex | hashRef | presence | lack | implication | reqs | reqType | simpleText)

  def negative: Rule1[Potential] =
    rule('-' ~!~ (('-' ~!~ expr) | (expr ~> ((f: Potential) => Potential.not(f)))))

  def expr: Rule1[Potential] =
    rule(negative | positive)

  def clause: Rule1[NonEmptyVector[Potential]] =
    rule(OWS ~ expr ~ zeroOrMore(OWS ~ expr) ~ OWS ~> mkClause)

  def allOf: Rule1[Potential] =
    rule('(' ~!~ clause ~ ')' ~> ((f: NonEmptyVector[Potential]) => Potential(AllOf(f))))

  def anyOf: Rule1[Potential] =
    rule('{' ~!~ clause ~ '}' ~> ((f: NonEmptyVector[Potential]) => Potential(AnyOf(f))))

  def main: Rule1[Option[Potential]] =
    rule(OWS ~ clause.? ~ EOI ~> mkMain)
}
