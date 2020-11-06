package shipreq.webapp.member.project.text

import monocle._
import org.parboiled2.CharPredicate
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.member.project.data.{ExternalPubid, ReqType, ReqTypePos}
import shipreq.webapp.member.project.text.GrammarSpec._

object Grammar {

  private val whitespace = "\\s+".r

  object fieldName {
    val length = Length(1 to 48)
    val chars  = CharBlacklist.dblQuotes // " is used to escape field names in filters
  }

  @inline def reqTypeName = fieldName
  @inline def tagGroupName = fieldName

  /** [[shipreq.webapp.member.project.data.ReqType.Mnemonic]] */
  object reqTypeMnemonic {
    val length = Length(1 to 6)
    val chars  = new CharWhitelist("", 'A', 'B' to 'Z')("may only consist of letters.")

    val caseInsensitiveRegexStr  = "[a-zA-Z]" + length.regexMod
    val caseInsensitiveParseChar = CharPredicate.Alpha

    val caseInsensitiveParsePost = (_: String).toUpperCase
    val caseSensitiveParseChar   = CharPredicate.UpperAlpha
  }

  object pubid {
    private val caseInsensitiveRegexStr = "(" + reqTypeMnemonic.caseInsensitiveRegexStr + """)\s*(?:-\s*)?(\d+)"""
    private val caseInsensitiveRegex    = caseInsensitiveRegexStr.r

    /**
     * This doesn't guarantee validity.
     * Both reqtype and pos still need to be checked against a Project in order to create a valid Pubid, thus,
     * ReqTypePos can be 0 here. This allows something like UC-0 to be recognised as a typo and presented as not-found.
     */
    val stringPrism = Prism[String, ExternalPubid]({
      case caseInsensitiveRegex(a, b) =>
        val rtm = ReqType.Mnemonic(reqTypeMnemonic caseInsensitiveParsePost a)
        val pos = ReqTypePos(b.toInt)
        Some(ExternalPubid(rtm, pos))
      case _ => None
    })(e => e.mnemonic.value + "-" + e.pos.value)

    val seqFormat = SeqFormat(
      _.trim, "[ ,]+".r.pattern, _.replace("-", "") |> reqTypeMnemonic.caseInsensitiveParsePost, _.isEmpty,
      _.iterator.mkString(" "))

    val preprocessor: String => String =
      TextMod.noWhitespace(_).toUpperCase
  }

  /**
   * [[shipreq.webapp.member.project.data.HashRefKey]]
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
    val midChars  = new CharWhitelist("_=-", '.', 'A' to 'Z', 'a' to 'z', '0' to '9')("may only consist of letters, numbers, and these symbols: . _ = -")
    def lastChar  = LastChar.azAZ09 // avoids Parser ambiguity with Underscore
    val prefix    = "#"
    val seqFormat = SeqFormat(_.trim, "[# ,]+".r.pattern, "^# *".r.replaceFirstIn(_, ""), _.isEmpty, _.iterator.mkString(" "))
  }

  /**
   * [[shipreq.webapp.member.project.data.ReqCode]]
   *
   * DD-17: Semantic-ID components must match this format: [a-z0-9][a-z0-9_]*
   * Must not contain: []{}<>.-?:"
   */
  object reqCode {
    def nodeLength = hashRefKey.length
    def firstChar  = FirstChar.az09
    def tailChars  = CharWhitelist.az09_

    def nodeSeparator = '.'

    /** Max number of nodes in [[shipreq.webapp.member.project.data.ReqCode.Value]] */
    def maxNodes = 20

    /** Max number of codes per ReqCode target */
    def maxCodes = 20

    /** For parsing a single value into nodes */
    val nodeSeqFormat = SeqFormat(
      whitespace.replaceAllIn(_, ""), quoteCh(nodeSeparator).r.pattern, identity, _ => false,
      _.iterator.mkString(nodeSeparator.toString))
  }

  val issueDescSurround = Surrounds("{", "}").addInnerForDisplay(" ", " ")

  val reflinkSurround = Surrounds("[", "]")

  final val texTag = "tex"

  val texSurround = Surrounds("<" + texTag + ">", "</" + texTag + ">")
}
