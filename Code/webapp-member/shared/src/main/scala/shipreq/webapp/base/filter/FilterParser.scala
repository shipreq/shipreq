package shipreq.webapp.base.filter

import japgolly.microlibs.recursion.Fix
import org.parboiled2.{Parser => _, _}
import scala.util.{Failure, Success, Try}
import shipreq.webapp.base.data
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.data.ReqTypePos
import shipreq.webapp.base.filter.Filter.{Potential, PotentialF}
import shipreq.webapp.base.filter.FilterAst._
import shipreq.webapp.base.util.ParsingUtil._
import shipreq.webapp.base.util._

object FilterParser {

  val preProcessor = PreProcessor.singleLine

  def parse(input: String): Result =
    parsePreProcessed(preProcessor(input))

  def parsePreProcessed(input: PreProcessed): Result = {
    val parser = new FilterParser(input.charArray)
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

  private type ImpType = Potential.ImpCriteria => Potential
  private val mkImplies  : ImpType = i => Potential.impliesAnyOf(i)
  private val mkImpliedBy: ImpType = i => Potential.impliedByAnyOf(i)
  private val mkImplication: (ImpType, Potential.ImpCriteria) => Potential = _(_)

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
    val charRule: () => Rule0 =
      () => rule(char)

    def stringRule: Rule1[String] =
      rule(charRule() ~!~ nonGreedyCapture(charRule))

    def astRule: Rule1[Potential] =
      rule(stringRule ~> ((s: String) => Potential.text(s, char)))
  }
  private val quote1 = QuoteRule('"')
  private val quote2 = QuoteRule('\'')
  private val quote3 = QuoteRule('`')

  private def quotedString: Rule1[String] =
    rule(quote1.stringRule | quote2.stringRule | quote3.stringRule)

  private def quotedText: Rule1[Potential] =
    rule((quote1.astRule | quote2.astRule | quote3.astRule) ~ end)

  private def simpleText: Rule1[Potential] =
    rule(capture(simpleTextChar.+) ~ end ~> ((s: String) => Potential.text(s: String)))

  private def regexChar: Rule0 =
    rule(!'/' ~ '\\'.? ~ ANY)

  private def regex: Rule1[Potential] =
    rule('/' ~!~ capture(regexChar.+) ~!~ '/' ~!~ end ~> ((s: String) => Potential.regex(s.replace("\\/", "/"))))

  private def orderOp: Rule1[OrderOp] =
    rule(
        ((">=" | '≥') ~ push(OrderOp.>=))
      | (("<=" | '≤') ~ push(OrderOp.<=))
      // keep the above, above; and the below, below; else below will prevent the above.
      | (">" ~ push(OrderOp.>))
      | ("<" ~ push(OrderOp.<))
    )

  private def relTags: Rule1[Potential] =
    rule(orderOp ~ OWS ~ hashRefStr_! ~ end ~> ((op: OrderOp, s: String) => Potential.relativeTags(op, data.HashRefKey(s))))

  private def hashRef: Rule1[Potential] =
    rule(
      ('=' ~ OWS).? ~ // allow this just for consistency with relTags
      hashRefStr_! ~ end ~> ((s: String) => Potential.hashRef(data.HashRefKey(s))))

  private def reqs: Rule1[Potential] =
    rule(reqTypeMnemonicCS ~ '-'.? ~ numberOrRange ~ end ~> mkReqs)

  private def reqType: Rule1[Potential] =
    rule(reqTypeMnemonicCS ~ end ~> ((i: Mnemonic) => Potential.reqType(i)))

  private def attr: Rule1[String] =
    rule(capture(attrChar.+) ~ end)

  private def field: Rule1[Potential] = {
    def quoteChar: Rule0 =
      rule('"')

    def name: Rule1[String] =
      rule(capture(fieldNameUnquotedChar.+) | (quoteChar ~ nonGreedyCapture(() => quoteChar)))

    def valueRule[A](r: () => Rule1[A])(f: A => Potential.FieldCriteria): RuleAB[String, Potential] =
      rule(r() ~> ((name: String, a: A) => Potential.fieldProp(name, f(a))))

    def value: RuleAB[String, Potential] =
      rule(
        valueRule(() => subQuery   )(FieldCriteria.Query(_))
      | valueRule(() => numberRange)(is => FieldCriteria.ReqTypePosSet(is.map(ReqTypePos)))
      | valueRule(() => attr       )(FieldCriteria.Attr(_))
      )

    rule("field:" ~ name ~ '=' ~!~ value)
  }

  private def subQuery: Rule1[Potential] =
    rule("(" ~ subQueries.mainNonEmpty ~ OWS ~ ")")

  private def presence: Rule1[Potential] =
    rule("has:" ~!~ attr ~> ((i: String) => Potential.presence(i)))

  private def hasIssue: Rule1[Potential] =
    rule("has:issue:" ~!~ ('-' ~ push(Off)).? ~ capture(CharPredicate.Alpha.+) ~!~ zeroOrMore(',' ~!~ capture(CharPredicate.Alpha.+)) ~>
      ((o: Option[On], h: String, t: Seq[String]) => Potential.hasIssue(o.getOrElse(On), h, t: _*)))

  /** implies:MF or impliedBy:FR,CC-1 */
  private def implication: Rule1[Potential] = {
    def criteria: Rule1[Potential.ImpCriteria] =
      rule(
        (subQuery ~> (FilterAst.ImpCriteria.Query.apply[Potential] _))
        | (reqSpecs ~> (FilterAst.ImpCriteria.Reqs.apply[Potential.ReqSet] _))
      )

    rule(
      "implie" ~ (('s' ~ push(mkImplies)) | ("dBy" ~ push(mkImpliedBy)))
        ~ ':'
        ~!~ criteria
        ~ end
        ~> mkImplication)
  }

  private def scoped: Rule1[Potential] = {
    type S = Scope[String]

    val fieldSuffix: () => Rule0 =
      () => rule(OWS ~ Scope.Derivation.fieldSuffix)

    def fieldName: Rule1[String] =
      rule(
        Scope.Derivation.fieldPrefix ~
        OWS ~
        (
          (quotedString ~ fieldSuffix())
          | nonGreedyCapture(fieldSuffix)
        )
      )

    val mkScope: Option[String] => S =
      Scope.Derivation(_)

    def scope: Rule1[S] =
      rule(Scope.Derivation.keyword ~ fieldName.? ~> mkScope)

    val mkResult: (Option[String], Seq[S], Potential, Option[Potential]) => Option[Potential] =
      (main, scopes, sub, mainClause) => {
        val ss = NonEmptyVector.force(scopes.toVector)
        (main, mainClause) match {
          case (_, None)          => Some(Potential.scoped1(main.isDefined, ss, sub))
          case (None, Some(mc))   => Some(Potential.scoped2(ss, sub, mc))
          case (Some(_), Some(_)) => None // should really specify an ErrorMsg here
        }
      }

    rule(
      capture(Scope.mainPrefix).? ~
        oneOrMore(scope).separatedBy(Scope.separator) ~
        Scope.suffix ~!~
        subQuery ~!~
        (Scope.mainPrefix ~ subQuery).?
        ~> mkResult ~ popOptional[Potential]
        ~ ((Whitespace ~ OWS) | EOI)
    )
  }

  // ===================================================================================================================

  private final class Universe(val legal: () => Rule1[Potential]) {

    def positive: Rule1[Potential] =
      rule(anyOf | allOf | legal())

    def negative: Rule1[Potential] =
      rule('-' ~!~ (('-' ~!~ expr) | (expr ~> ((f: Potential) => Potential.not(f)))))

    def expr: Rule1[Potential] =
      rule(negative | positive)

    def clause: Rule1[NonEmptyVector[Potential]] =
      rule(OWS ~ expr ~ zeroOrMore(OWS ~ expr) ~ OWS ~> mkClause)

    /** `( a b c )` */
    def allOf: Rule1[Potential] =
      rule('(' ~!~ clause ~ ')' ~> ((f: NonEmptyVector[Potential]) => Potential(AllOf(f))))

    /** `a b c` - no parens */
    def allOfImplicit: Rule1[Potential] =
      rule(clause ~> mkImplicitAllOf)

    /** `( a | b | c )` */
    def anyOf: Rule1[Potential] =
      rule('(' ~ anyOfImplicit ~ ')')

    /** `a | b | c` - no parens */
    def anyOfImplicit: Rule1[Potential] =
      rule(allOfImplicit ~ oneOrMore('|' ~!~ allOfImplicit)
        ~ popSeqToNEV[Potential] ~> ((a: Potential, b: NonEmptyVector[Potential]) => Potential(AnyOf(a, b))))

    def mainNonEmpty: Rule1[Potential] =
      rule(anyOfImplicit | (clause ~> mkImplicitAllOf))
  }

  private val topLevel = new Universe(() => rule(
    quotedText | regex | hashRef | relTags | hasIssue | presence | scoped | field | implication | reqs | reqType | simpleText
  ))

  private val subQueries = {
    val isValid: PotentialF[Potential] => Boolean = {
      case Scoped1(_, _, _)
         | Scoped2(_, _, _) => false
      case _                => true
    }

    def rejectInvalid: RuleAB[Potential, Potential] =
      rule(run((p: Potential) => test(isValid(p.unfix)) ~ push(p)))

    new Universe(() => rule(topLevel.legal() ~ rejectInvalid))
  }

  def main: Rule1[Option[Potential]] =
    rule(topLevel.mainNonEmpty.? ~ EOI)
}
