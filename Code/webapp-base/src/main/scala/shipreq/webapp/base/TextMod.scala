package shipreq.webapp.base

import java.util.regex.Pattern
import scala.util.matching.Regex
import scalaz.Endo
import scalaz.Isomorphism.<=>

object TextMod {

  def literal(from: Char, to: Char) = Endo[String](_.replace(from, to))
  def literal(from: String, to: String) = Endo[String](_.replace(from, to))

  def regexReplace(regex: Regex, repl: String) = Endo[String](regex.replaceAllIn(_, repl))

  private[this] val punctuationOrSymbol =
    // Originally /[\p{S}\p{P}]/
    "[\u0021-\u002f\u003a-\u0040\\[-\u0060\u007b-\u007e\u00a1-\u00a9\u00ab\u00ac\u00ae-\u00b1\u00b4\u00b6-\u00b8\u00bb\u00bf\u00d7\u00f7\u02c2-\u02c5\u02d2-\u02df\u02e5-\u02eb\u02ed\u02ef-\u02ff\u0375\u037e\u0384\u0385\u0387\u03f6\u0482\u055a-\u055f\u0589\u058a\u058f\u05be\u05c0\u05c3\u05c6\u05f3\u05f4\u0606-\u060f\u061b\u061e\u061f\u066a-\u066d\u06d4\u06de\u06e9\u06fd\u06fe\u0700-\u070d\u07f6-\u07f9\u0830-\u083e\u085e\u0964\u0965\u0970\u09f2\u09f3\u09fa\u09fb\u0af0\u0af1\u0b70\u0bf3-\u0bfa\u0c7f\u0d79\u0df4\u0e3f\u0e4f\u0e5a\u0e5b\u0f01-\u0f17\u0f1a-\u0f1f\u0f34\u0f36\u0f38\u0f3a-\u0f3d\u0f85\u0fbe-\u0fc5\u0fc7-\u0fcc\u0fce-\u0fda\u104a-\u104f\u109e\u109f\u10fb\u1360-\u1368\u1390-\u1399\u1400\u166d\u166e\u169b\u169c\u16eb-\u16ed\u1735\u1736\u17d4-\u17d6\u17d8-\u17db\u1800-\u180a\u1940\u1944\u1945\u19de-\u19ff\u1a1e\u1a1f\u1aa0-\u1aa6\u1aa8-\u1aad\u1b5a-\u1b6a\u1b74-\u1b7c\u1bfc-\u1bff\u1c3b-\u1c3f\u1c7e\u1c7f\u1cc0-\u1cc7\u1cd3\u1fbd\u1fbf-\u1fc1\u1fcd-\u1fcf\u1fdd-\u1fdf\u1fed-\u1fef\u1ffd\u1ffe\u2010-\u2027\u2030-\u205e\u207a-\u207e\u208a-\u208e\u20a0-\u20ba\u2100\u2101\u2103-\u2106\u2108\u2109\u2114\u2116-\u2118\u211e-\u2123\u2125\u2127\u2129\u212e\u213a\u213b\u2140-\u2144\u214a-\u214d\u214f\u2190-\u23f3\u2400-\u2426\u2440-\u244a\u249c-\u24e9\u2500-\u26ff\u2701-\u2775\u2794-\u2b4c\u2b50-\u2b59\u2ce5-\u2cea\u2cf9-\u2cfc\u2cfe\u2cff\u2d70\u2e00-\u2e2e\u2e30-\u2e3b\u2e80-\u2e99\u2e9b-\u2ef3\u2f00-\u2fd5\u2ff0-\u2ffb\u3001-\u3004\u3008-\u3020\u3030\u3036\u3037\u303d-\u303f\u309b\u309c\u30a0\u30fb\u3190\u3191\u3196-\u319f\u31c0-\u31e3\u3200-\u321e\u322a-\u3247\u3250\u3260-\u327f\u328a-\u32b0\u32c0-\u32fe\u3300-\u33ff\u4dc0-\u4dff\ua490-\ua4c6\ua4fe\ua4ff\ua60d-\ua60f\ua673\ua67e\ua6f2-\ua6f7\ua700-\ua716\ua720\ua721\ua789\ua78a\ua828-\ua82b\ua836-\ua839\ua874-\ua877\ua8ce\ua8cf\ua8f8-\ua8fa\ua92e\ua92f\ua95f\ua9c1-\ua9cd\ua9de\ua9df\uaa5c-\uaa5f\uaa77-\uaa79\uaade\uaadf\uaaf0\uaaf1\uabeb\ufb29\ufbb2-\ufbc1\ufd3e\ufd3f\ufdfc\ufdfd\ufe10-\ufe19\ufe30-\ufe52\ufe54-\ufe66\ufe68-\ufe6b\uff01-\uff0f\uff1a-\uff20\uff3b-\uff40\uff5b-\uff65\uffe0-\uffe6\uffe8-\uffee\ufffc\ufffd]"
  private[this] val notPunctuationOrSymbol =
    punctuationOrSymbol.replaceFirst("^.", "[^")

  private[this] def symbol(from: String, to: String) =
    //regex(s"(?<!$punctuationOrSymbol)${Pattern quote from}(?!$punctuationOrSymbol)".r, to)
    regexReplace(s"(^|$notPunctuationOrSymbol)$from(?!$punctuationOrSymbol)".r, "$1" + to)

  // ---------------------------------------------------------------------------

  val trim = Endo[String](_.trim)

  val lowerCase = Endo[String](_.toLowerCase)

  val upperCase = Endo[String](_.toUpperCase)

  val niceSymbols =
    symbol("<=", "≤") compose
    symbol(">=", "≥")

  val whitespaceRegex = "\\s+".r

  val singleLineWhitespace =
    regexReplace(whitespaceRegex, " ") andThen trim

  val multiLineWhitespace =
    regexReplace("\r\n?".r, "\n") andThen
    literal('\t', ' ') andThen
    trim

  val noWhitespace =
    regexReplace(whitespaceRegex, "")

  val squashUnderscores =
    regexReplace("__+".r, "_")

  def truncateToLength(range: Range.Inclusive): Endo[String] =
    truncateToLength(range.end)

  def truncateToLength(maxLen: Int): Endo[String] =
    Endo(s => if (s.length <= maxLen) s else s.substring(0, maxLen))

  object nonBlank extends (String <=> Option[String]) {
    override def to = s => if (s.isEmpty) None else Some(s)
    override def from = _ getOrElse ""
  }

}
