package shipreq.webapp.base.text

import japgolly.microlibs.utils.Utils
import java.util.regex.Pattern
import org.parboiled2.CharPredicate
import scala.collection.immutable.NumericRange
import scala.runtime.AbstractFunction1
import scalaz.std.list.listInstance
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.validation.CommonValidation
import shipreq.webapp.base.validation.Simple._

/**
  * Various aids to facilitate building a grammar specification that can be used to enforcement code.
  */
object GrammarSpec {

  def quoteCh(c: Char): String =
    if ("""$()*+-.?[]{|}\""" contains c) "\\" + c else c.toString

  class Chars(val chn: String, val ch1: Char, val rs: NumericRange[Char]*) {
    def iterator(): Iterator[Char] =
      Iterator.single(ch1) ++ chn.iterator ++ rs.iterator.flatMap(_.iterator)

    final val regex =
      ((Iterator.single(ch1) ++ chn.iterator).map(quoteCh) ++ rs.iterator.map(r => s"${r.min}-${r.max}")).mkString

    @inline final def one  = "[" + regex + "]"
    @inline final def not  = "[^" + regex + "]"
    @inline final def *    = "[" + regex + "]*"
    @inline final def +    = "[" + regex + "]+"

    final val charPredicate: CharPredicate =
      rs.foldLeft(CharPredicate(ch1 :: chn.toList))((q, r) => q ++ CharPredicate(r))
  }

  class CharWhitelist(chn: String, ch1: Char, rs: NumericRange[Char]*)(ruleErrMsg: String) extends Chars(chn, ch1, rs: _*) {
    final val validator: EndoValidator[String] =
      CommonValidation.endoValidator.whitelistCharRangeRegex(regex, Invalidity(ruleErrMsg))
  }

  object CharWhitelist {
    val az09_ = new CharWhitelist("", '_', 'a' to 'z', '0' to '9')("may only consist of letters, numbers, and underscores.")
  }

  class CharBlacklist(chn: String, ch1: Char, rs: NumericRange[Char]*)(ruleErrMsg: String) extends Chars(chn, ch1, rs: _*) {
    final val validator: EndoValidator[String] =
      CommonValidation.endoValidator.blacklistCharRangeRegex(regex, Invalidity(ruleErrMsg))
  }

  object CharBlacklist {
    val dblQuotes = new CharBlacklist("", '"')("mustn't contain double quotation marks.")
  }

  class FirstChar(chn: String, ch1: Char, rs: NumericRange[Char]*)(ruleErrMsg: String) extends Chars(chn, ch1, rs: _*) {
    final val invalidator: Invalidator[String] =
      CommonValidation.invalidator.startsWithRegex(one)(Invalidity(ruleErrMsg))
  }

  object FirstChar {
    val az     = new FirstChar("",             'a', 'b' to 'z')            ("must start with a letter.")
  //val azAZ   = new FirstChar("",             'a', 'b' to 'z', 'A' to 'Z')("must start with a letter.")
    val az09   = new FirstChar("", '0', '1' to '9', 'a' to 'z')            ("must start with a letter or number.")
    val azAZ09 = new FirstChar("", '0', '1' to '9', 'a' to 'z', 'A' to 'Z')("must start with a letter or number.")
  }

  class LastChar(chn: String, ch1: Char, rs: NumericRange[Char]*)(ruleErrMsg: String) extends Chars(chn, ch1, rs: _*) {
    final val invalidator: Invalidator[String] =
      CommonValidation.invalidator.endsWithRegex(one)(Invalidity(ruleErrMsg))
  }

  object LastChar {
  //val az     = new LastChar("",             'a', 'b' to 'z')            ("must end with a letter.")
  //val azAZ   = new LastChar("",             'a', 'b' to 'z', 'A' to 'Z')("must end with a letter.")
  //val az09   = new LastChar("", '0', '1' to '9', 'a' to 'z')            ("must end with a letter or number.")
    val azAZ09 = new LastChar("", '0', '1' to '9', 'a' to 'z', 'A' to 'Z')("must end with a letter or number.")
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case class Length(total: Range.Inclusive) {
    final val validator: EndoValidator[String] =
      CommonValidation.endoValidator.lengthInRange(total)

    val minus1   = (total.min - 1).max(0) to (total.max - 1).max(0)
    val minus2   = (total.min - 2).max(0) to (total.max - 2).max(0)
    def regexMod = s"{${total.min},${total.max}}"
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  class Surround(val prefix: String, val suffix: String) extends AbstractFunction1[String, String] {
    def apply(s: String): String =
      prefix + s + suffix

    def regexEscapeAndWrap: (String, String) =
      (Utils.regexEscapeAndWrap(prefix), Utils.regexEscapeAndWrap(suffix))

    def addInner(innerPrefix: String, innerSuffix: String): Surround =
      new Surround(prefix + innerPrefix, innerSuffix + suffix)
  }

  class Surrounds(val parsing: Surround, val display: Surround) {
    def addInnerForDisplay(innerPrefix: String, innerSuffix: String): Surrounds =
      new Surrounds(parsing, display.addInner(innerPrefix, innerSuffix))
  }

  object Surrounds {
    def apply(prefix: String, suffix: String): Surrounds = {
      val s = new Surround(prefix, suffix)
      new Surrounds(s, s)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /**
   * Transformations between a sequence of strings and a single string.
   */
  final case class SeqFormat(normAll : EndoFn[String],
                             sep     : Pattern,
                             normEach: EndoFn[String],
                             ignore  : String => Boolean,
                             merge   : IterableOnce[String] => String) {
    def split(input: String): Iterator[String] =
      (input |> normAll |> sep.split).iterator map normEach filterNot ignore

    def list(input: String): List[String] =
      split(input).toList

    def corrector: Corrector[String, List[String]] =
      Corrector.full(list, merge)

    def validator[V](elementAuditor: Auditor[String, V]): Validator[String, List[String], List[V]] =
      corrector withAuditor elementAuditor.liftTraverse[List]
  }
}
