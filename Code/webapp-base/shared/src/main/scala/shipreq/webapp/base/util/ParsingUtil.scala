package shipreq.webapp.base.util

import org.parboiled2._
import shapeless._
import shipreq.base.util.{NonEmptySet, NonEmptyVector}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{ReqTypePos, ReqType}
import shipreq.webapp.base.text.{Grammar => G}

object ParsingUtil {

  // (0 to 65535).filter(i => java.lang.Character.isWhitespace(i.toChar)).map("\\u%04x".format(_)).mkString("\"","","\"")
  val whitespace = CharPredicate("\u0009\u000a\u000b\u000c\u000d\u001c\u001d\u001e\u001f\u0020\u1680\u180e\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2008\u2009\u200a\u2028\u2029\u205f\u3000".toCharArray)

  val nonWhitespace = whitespace.negated -- EOI

  val mkReqTypeMnemonicCI = G.reqTypeMnemonic.caseInsensitiveParsePost andThen ReqType.Mnemonic.apply

  val trim = (_: String).trim

  val toInt = (_: String).toInt
}

abstract class ParsingUtil extends Parser {
  import ParsingUtil._

  final type RuleAB[-A, +B] = Rule[A :: HNil, B :: HNil]

  /** Beginning Of Input */
  def BOI: Rule0 =
    rule(test(cursor == 0))

  /** newline */
  def NL: Rule0 =
    rule('\n' | ('\r' ~ '\n'.?))

  /** End Of Line (consumes NL) */
  def EOL: Rule0 =
    rule(NL | EOI)

  /** int ≥ 1 */
  def int1n: Rule1[Int] =
    rule(capture(CharPredicate.Digit19 ~ CharPredicate.Digit.*) ~> toInt)

  def popOptional[A]: RuleAB[Option[A], A] =
    rule(run((o: Option[A]) => test(o.isDefined) ~ push(o.get)))

  def popPF[A, B](pf: PartialFunction[A, B]): RuleAB[A, B] =
    rule(run((a: A) => test(pf isDefinedAt a) ~ push(pf(a))))
    // rule(run{(a: A) => val o = pf.lift(a); test(o.isDefined) ~ push(o.get)})

  def popSeqToNEV[A]: RuleAB[Seq[A], NonEmptyVector[A]] =
    rule(run((v: Seq[A]) => test(v.nonEmpty) ~ push(NonEmptyVector(v.head, v.tail.toVector))))

  def popNEV[A]: RuleAB[Vector[A], NonEmptyVector[A]] =
    rule(run((v: Vector[A]) => test(v.nonEmpty) ~ push(NonEmptyVector(v.head, v.tail))))

  def popSeqToNES[A: UnivEq]: RuleAB[Seq[A], NonEmptySet[A]] =
    rule(run((v: Seq[A]) => test(v.nonEmpty) ~ push(NonEmptySet(v.head, v.tail.toSet))))

  def popNES[A: UnivEq]: RuleAB[Set[A], NonEmptySet[A]] =
    rule(run((v: Set[A]) => test(v.nonEmpty) ~ push(NonEmptySet(v.head, v.tail))))

  def grammarStr[G](g: G)(f: G => G.FirstChar, w: G => G.CharWhitelist, l: G => G.Length): Rule0 =
    rule( f(g).charPredicate ~ (l(g).minus1 times w(g).charPredicate) )

  def nonGreedyCapture(stopAt: () => Rule0): Rule1[String] =
    rule(capture(oneOrMore(!stopAt() ~ ANY)) ~ stopAt())

//  def surroundedBy(s: () => Rule0): Rule1[String] =
//    rule(s() ~ nonGreedyCapture(s))

  def surround(s: G.Surrounds): Rule1[String] =
    surround(s.parsing)

  def surround(s: G.Surround): Rule1[String] = {
    val end = () => rule(s.suffix)
    rule(s.prefix ~ nonGreedyCapture(end) ~> trim)
  }

  def reqTypeMnemonicCI: Rule1[ReqType.Mnemonic] =
    rule(capture(G.reqTypeMnemonic.length.total times G.reqTypeMnemonic.caseInsensitiveParseChar) ~> mkReqTypeMnemonicCI)

  def reqTypeMnemonicCS: Rule1[ReqType.Mnemonic] =
    rule(capture(G.reqTypeMnemonic.length.total times G.reqTypeMnemonic.caseSensitiveParseChar) ~> ReqType.Mnemonic)

  def reqTypePos: Rule1[ReqTypePos] =
    rule(int1n ~> ReqTypePos)

  def hashRefStr: Rule1[String] =
    rule(G.hashRefKey.prefix ~ capture(grammarStr(G.hashRefKey)(_.firstChar, _.allChars, _.length)))

  def hashRefStr_! : Rule1[String] =
    rule(G.hashRefKey.prefix ~!~ capture(grammarStr(G.hashRefKey)(_.firstChar, _.allChars, _.length)))
}
