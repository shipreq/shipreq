package shipreq.webapp.base.filter

import org.parboiled2.{Parser => _, _}
import shipreq.base.util.NonEmptyVector
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.util.ParsingUtil
import ParsingUtil._
import FilterSpec._

object FilterParser {

  // Allows ' / -
  private val simpleTextChar =
    CharPredicate("""#:"`(){}""".toCharArray).negated -- whitespace -- EOI

  private val attrChar =
    CharPredicate.AlphaNum

  private val mkIntSet1: Int => Set[Int] =
    Set.empty[Int].+

  private val mkIntSet: (Int, Option[Int]) => Set[Int] =
    (a, o) =>
      o.fold(Set.empty[Int] + a)(b =>
        (if (a < b) (a to b) else (b to a)).toSet)

  private val flattenIntSets: Seq[Set[Int]] => Set[Int] =
    s => if (s.isEmpty) Set.empty else s.reduce(_ ++ _)

  private val mkReqsSpec: (Mnemonic, Option[Set[Int]]) => ReqsSpec =
    (rt, ons) => ons match {
      case None     => ReqsSpec.WholeType(rt)
      case Some(ns) => ReqsSpec.SomeOfType(rt, ns)
    }

  private type ImpType = Reqs => FilterSpec
  private val mkImplies  : ImpType = Implies
  private val mkImpliedBy: ImpType = ImpliedBy
  private val mkImplication: (ImpType, Reqs) => FilterSpec = _(_)

  private val mkAllOf: Seq[FilterSpec] => AllOf = a => AllOf(NonEmptyVector(a.head, a.tail: _*))
  private val mkAnyOf: Seq[FilterSpec] => AnyOf = a => AnyOf(NonEmptyVector(a.head, a.tail: _*))

  private val mkMain: Seq[FilterSpec] => Option[FilterSpec] = a =>
    NonEmptyVector.maybe(a.toVector, None: Option[FilterSpec])(nev =>
      if (nev.tail.isEmpty)
        Some(nev.head)
      else
        Some(AllOf(nev)))
}

class FilterParser(val input: ParserInput) extends ParsingUtil {
  import FilterParser._

  private def WS  = rule(oneOrMore(whitespace))
  private def OWS = rule(zeroOrMore(whitespace))

  /** 1 or 1-5 */
  def numberRangeElement: Rule1[Set[Int]] =
    rule(int1n ~ optional('-' ~ int1n) ~> mkIntSet)

  /** 1,3,5-9,12 */
  def numberRange: Rule1[Set[Int]] =
    rule((numberRangeElement + ',') ~> flattenIntSets)

  /** 1 or {1,3,5-9,12} */
  def numberOrRange: Rule1[Set[Int]] =
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
    rule(quote1.toRule | quote2.toRule | quote3.toRule)

  def simpleText: Rule1[SimpleText] =
    rule(capture(simpleTextChar.+) ~> SimpleText)

  def regexChar: Rule0 =
    rule(!'/' ~ '\\'.? ~ ANY)

  def regex: Rule1[Regex] =
    rule('/' ~!~ capture(regexChar.+) ~!~ '/' ~> ((s: String) => Regex(s.replace("\\/", "/"))))

  def hashRef: Rule1[HashRef] =
    rule(hashRefStr_! ~> HashRef)

  def reqType: Rule1[ReqType] =
    rule(reqTypeMnemonicCS ~> ReqType)

  def attr: Rule1[String] =
    rule(capture(attrChar.+))

  def presence: Rule1[Presence] =
    rule("has:" ~!~ attr ~> Presence)

  def lack: Rule1[Lack] =
    rule("no:" ~!~ attr ~> Lack)

  /** implies:MF or impliedBy:FR,CC-1 */
  def implication: Rule1[FilterSpec] =
    rule("implie" ~ (
      ('s' ~ push(mkImplies)) | ("dBy" ~ push(mkImpliedBy))
    ) ~ ':' ~!~ reqs ~> mkImplication)

  def allOf: Rule1[AllOf] =
    rule('(' ~!~ OWS ~ oneOrMore(expr ~ OWS) ~ ')' ~> mkAllOf)

  def anyOf: Rule1[AnyOf] =
    rule('{' ~!~ OWS ~ oneOrMore(expr ~ OWS) ~ '}' ~> mkAnyOf)

  def positive: Rule1[FilterSpec] =
    rule(allOf | anyOf | quotedText | regex | hashRef | presence | lack | implication | reqType | simpleText)

  def negative: Rule1[FilterSpec] =
    rule('-' ~!~ (('-' ~!~ expr) | (expr ~> Not)))

  def expr: Rule1[FilterSpec] =
    rule(negative | positive)

  def main: Rule1[Option[FilterSpec]] =
    rule(OWS ~ zeroOrMore(expr ~ OWS) ~ EOI ~> mkMain)
}
