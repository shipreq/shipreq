package shipreq.webapp.shared

import java.util.regex.Pattern
import scala.util.matching.Regex
import scalaz.Endo
import scalaz.Isomorphism.<=>

object TextMod {

  def literal(from: Char, to: Char) = Endo[String](_.replace(from, to))
  def literal(from: String, to: String) = Endo[String](_.replace(from, to))

  def regex(regex: Regex, repl: String) = Endo[String](regex.replaceAllIn(_, repl))

  // TODO /[\p{S}\p{P}]/ isn't going to work in JS land
  private[this] val punctuationOrSymbol = """[\p{S}\p{P}]"""
  def symbol(from: String, to: String) =
    regex(s"(?<!$punctuationOrSymbol)${Pattern quote from}(?!$punctuationOrSymbol)".r, to)

  // ---------------------------------------------------------------------------

  val trim = Endo[String](_.trim)

  val lowerCase = Endo[String](_.toLowerCase)

  val upperCase = Endo[String](_.toUpperCase)

  val niceSymbols =
    symbol("<=", "≤") compose
    symbol(">=", "≥")

  val whitespaceRegex = "\\s+".r

  val singleLineWhitespace =
    regex(whitespaceRegex, " ") andThen trim

  val multiLineWhitespace =
    regex("\r\n?".r, "\n") andThen
    literal('\t', ' ') andThen
    trim

  val noWhitespace =
    regex(whitespaceRegex, "")

  def truncateToLength(range: Range.Inclusive): Endo[String] =
    truncateToLength(range.end)

  def truncateToLength(maxLen: Int): Endo[String] =
    Endo(s => if (s.length <= maxLen) s else s.substring(0, maxLen))

  object nonBlank extends (String <=> Option[String]) {
    override def to = s => if (s.isEmpty) None else Some(s)
    override def from = _ getOrElse ""
  }

}
