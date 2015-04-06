package shipreq.webapp.base.text

import scala.collection.immutable.NumericRange
import shipreq.webapp.base.validation.{Constraints, Rules}

object Grammar {
  class Chars(val chn: String, val ch1: Char, val rs: NumericRange[Char]*) {
    final val regex = ((ch1 #:: chn.toStream).map("\\" + _) append rs.toStream.map(r => s"${r.min}-${r.max}")).mkString
    @inline final def one  = "[" + regex + "]"
    @inline final def not  = "[^" + regex + "]"
    @inline final def *    = "[" + regex + "]*"
    @inline final def +    = "[" + regex + "]+"

    import org.parboiled2._
    final val charPredicate: CharPredicate =
      rs.foldLeft(CharPredicate(ch1 :: chn.toList))((q, r) => q ++ CharPredicate(r))
  }

  class CharWhitelist(chn: String, ch1: Char, rs: NumericRange[Char]*)(ruleErrMsg: String) extends Chars(chn, ch1, rs: _*) {
    final val rule = Rules.whitelistCharsR(regex, ruleErrMsg)
  }

  class FirstChar(chn: String, ch1: Char, rs: NumericRange[Char]*)(ruleErrMsg: String) extends Chars(chn, ch1, rs: _*) {
    final val constraint = Constraints.startsWithR(one)(ruleErrMsg)
  }
  object FirstChar {
    val azAZ   = new FirstChar("",             'a', 'b' to 'z', 'A' to 'Z')("must start with a letter.")
    val azAZ09 = new FirstChar("", '0', '1' to '9', 'a' to 'z', 'A' to 'Z')("must start with a letter or number.")
  }

  case class Length(total: Range.Inclusive) {
    val rule   = Rules lengthInRange total
    val minus1 = (total.min - 1) to (total.max - 1)
  }

  // ===================================================================================================================

  /** [[shipreq.webapp.base.data.ReqType.Mnemonic]] */
  object reqTypeMnemonic {
    val length = Length(1 to 6)
    val chars  = new CharWhitelist("", 'A', 'B' to 'Z')("may only consist of letters.")
  }

  // TODO hashrefkey & mnemonic are both case-insensitive but char ranges are defined differently

  /** [[shipreq.webapp.base.data.HashRefKey]] */
  // DD-18: Hashtag-like refkeys (groupings, incmp) must match this format: /[A-Za-z0-9][A-Za-z0-9_-=.]*/
  // Must not contain: []{}<>#
  object hashRefKey {
    val length    = Length(1 to 20)
    def firstChar = FirstChar.azAZ09
    val allChars  = new CharWhitelist("_=-", '.', 'A' to 'Z', 'a' to 'z', '0' to '9')("may only consist of letters, numbers, and these symbols: . _ = -")
  }

  /** [[shipreq.webapp.base.data.FieldRefKey]] min & max lengths. */
  // DD-20: Field refkeys must match this format: /[a-z][a-z0-9_]*/
  // Must not contain: []{}<>.?"
  object fieldRefKey {
    def length    = hashRefKey.length
    def firstChar = FirstChar.azAZ
    val allChars  = new CharWhitelist("" , '_', 'a' to 'z', '0' to '9')("may only consist of letters, numbers, and underscores.")
  }

  /** [[shipreq.webapp.base.data.ReqCode.Node]] min & max lengths. */
  // TODO Grammar.reqCodeNode only has length atm
  def reqCodeNodeLength = hashRefKey.length

  val issueDescPrefix = "{ "
  val issueDescSuffix = " }"

  val reflinkPrefix = "["
  val reflinkSuffix = "]"

  val hashtagPrefix = "#"

  val mathTexPrefix = "<math>"
  val mathTexSuffix = "</math>"

}
