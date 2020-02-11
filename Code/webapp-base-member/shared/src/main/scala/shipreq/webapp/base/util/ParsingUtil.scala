package shipreq.webapp.base.util

import japgolly.microlibs.nonempty._
import org.parboiled2._
import scala.annotation.elidable
import scalaz.{\/, \/-}
import shapeless._
import shipreq.base.util.Util
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{ReqType, ReqTypePos}
import shipreq.webapp.base.text.{Grammar => G}
import shipreq.webapp.base.text.GrammarSpec._

object ParsingUtil {

  def toCharPredicate(c: CharSubset): CharPredicate =
    c.consume {
      // Stack overflow. Yay.
      // var i = ranges.iterator.map(r => CharPredicate(r.start.toChar to r.end.toChar))
      // if (direct.nonEmpty) {
      //   val c = CharPredicate(direct.toIterator.map(_.toChar).toArray)
      //   i = Iterator.single(c) ++ i
      // }
      // i.reduce(_ ++ _)

      val set = collection.mutable.Set.empty[Char]

      val addChar: Int => Unit =
        set += _.toChar

      c.direct foreach addChar
      c.ranges foreach (_ foreach addChar)

      CharPredicate(set.contains _)
    }

  val PunctuationOrSymbol: CharPredicate =
    toCharPredicate(CharSubset.PunctuationOrSymbol)

  val Whitespace: CharPredicate =
    toCharPredicate(CharSubset.Whitespace)

  val NonWhitespace: CharPredicate =
    Whitespace.negated -- EOI

  val mkReqTypeMnemonicCI: String => ReqType.Mnemonic =
    G.reqTypeMnemonic.caseInsensitiveParsePost andThen ReqType.Mnemonic.apply

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

  /** Single-Line WhiteSpace (one char) */
  def SLWS: Rule0 =
    rule(!EOL ~ Whitespace)

  def lastCharIs(cp: CharPredicate): Rule0 =
    rule(test(cursor != 0 && cp(lastChar)))

  /** int ≥ 1 */
  def int1n: Rule1[Int] =
    rule(ch('0').* ~ capture(CharPredicate.Digit19 ~ CharPredicate.Digit.*) ~> toInt)

  def popOptional[A]: RuleAB[Option[A], A] =
    rule(run((o: Option[A]) => test(o.isDefined) ~ push(o.get)))

  def pop_\/-[A]: RuleAB[Any \/ A, A] =
    rule(run((d: Any \/ A) => test(d.isRight) ~ push(d.asInstanceOf[\/-[A]].b)))

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

  def pushOptional[A](o: Option[A]): Rule1[A] =
    rule(run(test(o.isDefined) ~ push(o.get)))

  def grammarStr[G](g: G)(f: G => FirstChar, w: G => CharWhitelist, l: G => Length): Rule0 =
    rule( f(g).charPredicate ~ (l(g).minus1 times w(g).charPredicate) )

  def nonGreedyCapture(stopAt: () => Rule0): Rule1[String] =
    rule(capture(oneOrMore(!stopAt() ~ ANY)) ~ stopAt())

  def nonGreedyCapture0(stopAt: () => Rule0): Rule1[String] =
    rule(capture(zeroOrMore(!stopAt() ~ ANY)) ~ stopAt())

//  def surroundedBy(s: () => Rule0): Rule1[String] =
//    rule(s() ~ nonGreedyCapture(s))

  def surround(s: Surrounds): Rule1[String] =
    surround(s.parsing)

  def surround(s: Surround): Rule1[String] = {
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
    rule(G.hashRefKey.prefix ~ capture(grammarStr(G.hashRefKey)(_.firstChar, _.tailChars, _.length)))

  def hashRefStr_! : Rule1[String] =
    rule(G.hashRefKey.prefix ~!~ capture(grammarStr(G.hashRefKey)(_.firstChar, _.tailChars, _.length)))

  def unindentBy(spaces: Int): String => String =
    Util.unindentBy(_ , spaces)

  @elidable(elidable.FINE)
  def debugPrintRemainder: Rule0 =
    rule((capture(ANY.*) ~> ((i: String) => println(s"remainder: [${i.replace("\n", "\\n")}]")) ~ "fail") | test(true))

  @elidable(elidable.FINE)
  def debugPrint(s: => Any): Rule0 =
    rule(push(0) ~> ((_: Int) => println(s)))

}
