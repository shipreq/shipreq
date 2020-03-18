package shipreq.webapp.base.text

import monocle._
import org.parboiled2.CharPredicate
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data.{ReqType, ReqTypePos}
import shipreq.webapp.base.text.GrammarSpec._
import shipreq.webapp.base.util.TextMod

object Grammar {

  private val whitespace = "\\s+".r

  /** [[shipreq.webapp.base.data.ReqType.Mnemonic]] */
  object reqTypeMnemonic {
    val length = Length(1 to 6)
    val chars  = new CharWhitelist("", 'A', 'B' to 'Z')("may only consist of letters.")

    val caseInsensitiveRegexStr  = "[a-zA-Z]" + length.regexMod
    val caseInsensitiveParseChar = CharPredicate.Alpha

    val caseInsensitiveParsePost = (_: String).toUpperCase
    val caseSensitiveParseChar   = CharPredicate.UpperAlpha
  }

  object pubid {
    val caseInsensitiveRegexStr = "(" + reqTypeMnemonic.caseInsensitiveRegexStr + """)\s*(?:-\s*)?(\d+)"""
    val caseInsensitiveRegex    = caseInsensitiveRegexStr.r

    /**
     * This doesn't guarantee validity.
     * Both reqtype and pos still need to be checked against a Project in order to create a valid Pubid, thus,
     * ReqTypePos can be 0 here. This allows something like UC-0 to be recognised as a typo and presented as not-found.
     */
    val stringPrism = Prism[String, (ReqType.Mnemonic, ReqTypePos)]({
      case caseInsensitiveRegex(a, b) =>
        val rtm = ReqType.Mnemonic(reqTypeMnemonic caseInsensitiveParsePost a)
        val pos = ReqTypePos(b.toInt)
        Some((rtm, pos))
      case _ => None
    })(t => t._1.value + "-" + t._2.value)

    val seqFormat = SeqFormat(
      _.trim, "[ ,]+".r.pattern, _.replace("-", "") |> reqTypeMnemonic.caseInsensitiveParsePost, _.isEmpty,
      _ mkString " ")

    val preprocessor: String => String =
      TextMod.noWhitespace(_).toUpperCase
  }

  /**
   * [[shipreq.webapp.base.data.HashRefKey]]
   *
   * DD-18: Hashtag-like refkeys (groupings, incmp) must match this format: [A-Za-z0-9][A-Za-z0-9_-=.]*
   * Must not contain: []{}<>#
   *
   * The case used at creation/update is retain but in all other regards, this is case-insensitive.
   * For example: if #Hello exists, User can enter #HELLO in text and it will be replaced with #Hello; User cannot
   * create another tag called #hello but they can rename #Hello to #hello.
   */
  object hashRefKey {
    val length    = Length(1 to 20)
    def firstChar = FirstChar.azAZ09
    val tailChars = new CharWhitelist("_=-", '.', 'A' to 'Z', 'a' to 'z', '0' to '9')("may only consist of letters, numbers, and these symbols: . _ = -")
    val prefix    = "#"
    val seqFormat = SeqFormat(_.trim, "[# ,]+".r.pattern, "^# *".r.replaceFirstIn(_, ""), _.isEmpty, _ mkString " ")
  }

  /**
   * [[shipreq.webapp.base.data.ReqCode]]
   *
   * DD-17: Semantic-ID components must match this format: [a-z0-9][a-z0-9_]*
   * Must not contain: []{}<>.-?:"
   */
  object reqCode {
    def nodeLength = hashRefKey.length
    def firstChar  = FirstChar.az09
    def tailChars  = CharWhitelist.az09_

    def nodeSeparator = '.'

    /** Max number of nodes in [[shipreq.webapp.base.data.ReqCode.Value]] */
    def maxNodes = 20

    /** Max number of codes per ReqCode target */
    def maxCodes = 20

    /** For parsing a single value into nodes */
    val nodeSeqFormat = SeqFormat(
      whitespace.replaceAllIn(_, ""), quoteCh(nodeSeparator).r.pattern, identity, _ => false,
      _ mkString nodeSeparator.toString)
  }

  val issueDescSurround = Surrounds("{", "}").addInnerForDisplay(" ", " ")

  val reflinkSurround = Surrounds("[", "]")

  final val texTag = "tex"

  val texSurround = Surrounds("<" + texTag + ">", "</" + texTag + ">")
}
