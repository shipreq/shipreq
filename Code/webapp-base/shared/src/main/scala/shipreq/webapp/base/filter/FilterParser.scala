package shipreq.webapp.base.filter

import org.parboiled2.{Parser => _, _}
import shipreq.base.util.{NonEmptyVector, NonEmptySet}
import shipreq.webapp.base.data.HashRefKey
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.util.ParsingUtil
import ParsingUtil._
import FilterSpec._

object FilterParser {

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
        val s = (if (a < b) (a to b) else (b to a)).toSet
        NonEmptySet force s
      }

  private val flattenIntSets1: Seq[NonEmptySet[Int]] => NonEmptySet[Int] =
    _.reduce(_ ++ _)

  private val mkReqsSpec: (Mnemonic, Option[NonEmptySet[Int]]) => ReqsSpec =
    (rt, ons) => ons match {
      case None     => WholeType(rt)
      case Some(ns) => SomeOfType(rt, ns)
    }

  private type ImpType = Reqs => FilterSpec
  private val mkImplies  : ImpType = Implies
  private val mkImpliedBy: ImpType = ImpliedBy
  private val mkImplication: (ImpType, Reqs) => FilterSpec = _(_)

  private val mkClause: (FilterSpec, Seq[FilterSpec]) => NonEmptyVector[FilterSpec] =
    (h, t) => NonEmptyVector(h, t: _*)

  private val mkMain: Option[NonEmptyVector[FilterSpec]] => Option[FilterSpec] =
    _.map(nev =>
      if (nev.tail.isEmpty)
        nev.head
      else
        AllOf(nev))
}

class FilterParser(val input: ParserInput) extends ParsingUtil {
  import FilterParser._

  private def WS  = rule(oneOrMore(Whitespace))
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
  def reqsSpec: Rule1[ReqsSpec] =
    rule(reqTypeMnemonicCI ~ optional('-' ~!~ numberOrRange) ~> mkReqsSpec)

  def reqs: Rule1[Reqs] =
    rule((reqsSpec + ',') ~ popSeqToNEV[ReqsSpec])

  private case class QuoteRule(char: Char) {
    val charRule: () => Rule0       = () => rule(char)
    def toRule  : Rule1[QuotedText] =
      rule(charRule() ~!~ nonGreedyCapture(charRule) ~> (QuotedText(_: String, char)))
  }
  private val quote1 = QuoteRule('"')
  private val quote2 = QuoteRule('\'')
  private val quote3 = QuoteRule('`')

  def quotedText: Rule1[QuotedText] =
    rule((quote1.toRule | quote2.toRule | quote3.toRule) ~ end)

  def simpleText: Rule1[SimpleText] =
    rule(capture(simpleTextChar.+) ~ end ~> SimpleText)

  def regexChar: Rule0 =
    rule(!'/' ~ '\\'.? ~ ANY)

  def regex: Rule1[Regex] =
    rule('/' ~!~ capture(regexChar.+) ~!~ '/' ~!~ end ~> ((s: String) => Regex(s.replace("\\/", "/"))))

  def hashRef: Rule1[HashRef] =
    rule(hashRefStr_! ~ end ~> HashRefKey ~> HashRef)

  def reqType: Rule1[ReqType] =
    rule(reqTypeMnemonicCS ~ end ~> ReqType)

  def attr: Rule1[String] =
    rule(capture(attrChar.+) ~ end)

  def presence: Rule1[Presence] =
    rule("has:" ~!~ attr ~> Presence)

  def lack: Rule1[Lack] =
    rule("no:" ~!~ attr ~> Lack)

  /** implies:MF or impliedBy:FR,CC-1 */
  def implication: Rule1[FilterSpec] =
    rule("implie" ~ (
      ('s' ~ push(mkImplies)) | ("dBy" ~ push(mkImpliedBy))
      ) ~ ':' ~!~ reqs ~ end ~> mkImplication)

  def not: Rule1[FilterSpec] =
    rule('-' ~!~ (('-' ~!~ expr) | (expr ~> Not)))

  def positive: Rule1[FilterSpec] =
    rule(allOf | anyOf | quotedText | regex | hashRef | presence | lack | implication | reqType | simpleText)

  def negative: Rule1[FilterSpec] =
    rule('-' ~!~ (('-' ~!~ expr) | (expr ~> Not)))

  def expr: Rule1[FilterSpec] =
    rule(negative | positive)

  def clause: Rule1[NonEmptyVector[FilterSpec]] =
    rule(OWS ~ expr ~ zeroOrMore(OWS ~ expr) ~ OWS ~> mkClause)

  def allOf: Rule1[AllOf] =
    rule('(' ~!~ clause ~ ')' ~> AllOf)

  def anyOf: Rule1[AnyOf] =
    rule('{' ~!~ clause ~ '}' ~> AnyOf)

  def main: Rule1[Option[FilterSpec]] =
    rule(OWS ~ clause.? ~ EOI ~> mkMain)
}
