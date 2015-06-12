package shipreq.webapp.base.text

import java.util.regex.Pattern
import org.parboiled2.CharPredicate
import scala.collection.immutable.NumericRange
import scala.runtime.AbstractFunction1
import shipreq.base.util.ScalaExt._
import shipreq.base.util.Util
import shipreq.webapp.base.validation.{Constraints, Rules}

object Grammar {
  private val whitespace = "\\s+".r

  private def quoteCh(c: Char): String =
    if ("""$()*+-.?[]{|}\""" contains c) "\\" + c else c.toString

  class Chars(val chn: String, val ch1: Char, val rs: NumericRange[Char]*) {
    def toStream: Stream[Char] =
      ch1 #:: chn.toStream append rs.toStream.flatMap(_.toStream)

    final val regex = ((ch1 #:: chn.toStream).map(quoteCh) append rs.toStream.map(r => s"${r.min}-${r.max}")).mkString
    @inline final def one  = "[" + regex + "]"
    @inline final def not  = "[^" + regex + "]"
    @inline final def *    = "[" + regex + "]*"
    @inline final def +    = "[" + regex + "]+"

    final val charPredicate: CharPredicate =
      rs.foldLeft(CharPredicate(ch1 :: chn.toList))((q, r) => q ++ CharPredicate(r))
  }

  class CharWhitelist(chn: String, ch1: Char, rs: NumericRange[Char]*)(ruleErrMsg: String) extends Chars(chn, ch1, rs: _*) {
    final val rule = Rules.whitelistCharsR(regex, ruleErrMsg)
  }
  object CharWhitelist {
    val az09_ = new CharWhitelist("", '_', 'a' to 'z', '0' to '9')("may only consist of letters, numbers, and underscores.")
  }

  class FirstChar(chn: String, ch1: Char, rs: NumericRange[Char]*)(ruleErrMsg: String) extends Chars(chn, ch1, rs: _*) {
    final val constraint = Constraints.startsWithR(one)(ruleErrMsg)
  }
  object FirstChar {
    val az     = new FirstChar("",             'a', 'b' to 'z')            ("must start with a letter.")
    val azAZ   = new FirstChar("",             'a', 'b' to 'z', 'A' to 'Z')("must start with a letter.")
    val az09   = new FirstChar("", '0', '1' to '9', 'a' to 'z')            ("must start with a letter or number.")
    val azAZ09 = new FirstChar("", '0', '1' to '9', 'a' to 'z', 'A' to 'Z')("must start with a letter or number.")
  }

  case class Length(total: Range.Inclusive) {
    val rule   = Rules lengthInRange total
    val minus1 = (total.min - 1) to (total.max - 1)
  }

  class Surround(val prefix: String, val suffix: String) extends AbstractFunction1[String, String] {
    def apply(s: String): String = prefix + s + suffix

    def regexEscapeAndWrap: (String, String) =
      (Util.regexEscapeAndWrap(prefix), Util.regexEscapeAndWrap(suffix))
  }
  class Surrounds(val parsing: Surround, val display: Surround)
  def surrounds(prefix: String, suffix: String) = {
    val s = new Surround(prefix, suffix)
    new Surrounds(s, s)
  }
  def surrounds(prefix: String, suffix: String, innerPrefix: String, innerSuffix: String) =
    new Surrounds(
      new Surround(prefix, suffix),
      new Surround(prefix + innerPrefix, innerSuffix + suffix)
    )

  /**
   * Describes how to pre-process (for parsing) text representing a sequence of values.
   */
  final case class SeqFormat(normAll: EndoFn[String], sep: Pattern, normEach: EndoFn[String], ignore: String => Boolean) {
    def apply(input: String): Stream[String] =
      (input |> normAll |> sep.split).toStream map normEach filterNot ignore
  }

  // ===================================================================================================================

  /** [[shipreq.webapp.base.data.ReqType.Mnemonic]] */
  object reqTypeMnemonic {
    val length = Length(1 to 6)
    val chars  = new CharWhitelist("", 'A', 'B' to 'Z')("may only consist of letters.")

    val caseSensitiveParseChar   = CharPredicate.UpperAlpha
    val caseInsensitiveParseChar = CharPredicate.Alpha
    val caseInsensitiveParsePost = (_: String).toUpperCase
  }

  val pubidSeqFormat = SeqFormat(_.trim, "[ ,]+".r.pattern, _.replace("-", "") |> reqTypeMnemonic.caseInsensitiveParsePost, _.isEmpty)

  // TODO hashrefkey & mnemonic are both case-insensitive but char ranges are defined differently

  /** [[shipreq.webapp.base.data.HashRefKey]] */
  // DD-18: Hashtag-like refkeys (groupings, incmp) must match this format: /[A-Za-z0-9][A-Za-z0-9_-=.]*/
  // Must not contain: []{}<>#
  object hashRefKey {
    val length    = Length(1 to 20)
    def firstChar = FirstChar.azAZ09
    val allChars  = new CharWhitelist("_=-", '.', 'A' to 'Z', 'a' to 'z', '0' to '9')("may only consist of letters, numbers, and these symbols: . _ = -")
    val prefix    = "#"
    val seqFormat = SeqFormat(_.trim, "[# ,]+".r.pattern, "^# *".r.replaceFirstIn(_, ""), _.isEmpty)
  }

  /** [[shipreq.webapp.base.data.FieldRefKey]] */
  // DD-20: Field refkeys must match this format: /[a-z][a-z0-9_]*/
  // Must not contain: []{}<>.?"
  object fieldRefKey {
    def length    = hashRefKey.length
    def firstChar = FirstChar.az
    def allChars  = CharWhitelist.az09_
  }

  /** [[shipreq.webapp.base.data.ReqCode]] */
  // DD-17: Semantic-ID components must match this format: /[a-z0-9][a-z0-9_]*/
  // Must not contain: []{}<>.-?:"
  object reqCode {
    def nodeLength = hashRefKey.length
    def firstChar  = FirstChar.az09
    def allChars   = CharWhitelist.az09_

    def nodeSeparator = '.'

    /** Max number of nodes in [[shipreq.webapp.base.data.ReqCode.Value]] */
    def maxNodes = 20

    /** Max number of codes per [[shipreq.webapp.base.data.ReqCode.Target]] */
    def maxCodes = 20

    /** For parsing a single value into nodes */
    val nodeSeqFormat = SeqFormat(whitespace.replaceAllIn(_, ""), quoteCh(nodeSeparator).r.pattern, identity, _ => false)
  }

  val issueDescSurround = surrounds("{", "}", " ", " ")

  val reflinkSurround = surrounds("[", "]")

  val mathTexSurround = surrounds("<math>", "</math>")
}
