package shipreq.webapp.base.util

import cats.Endo
import monocle.Iso
import scala.util.matching.Regex

object TextMod {

  def literal(from: Char, to: Char): Endo[String] = _.replace(from, to)
  def literal(from: String, to: String): Endo[String] = _.replace(from, to)

  def regexReplace(regex: Regex, repl: String): Endo[String] = regex.replaceAllIn(_, repl)

  def regexReplaceFirst(regex: Regex, repl: String): Endo[String] = regex.replaceFirstIn(_, repl)

  private[this] def symbol(from: String, to: String): Endo[String] =
    CharSubset.PunctuationOrSymbol.notAroundReplaceAll(from, to)

  // ---------------------------------------------------------------------------

  val trim: Endo[String] = _.trim

  val lowerCase: Endo[String] = _.toLowerCase

  val upperCase: Endo[String] = _.toUpperCase

  val niceSymbols =
    symbol("<=", "≤") compose
    symbol(">=", "≥")

  val whitespaceRegex: Regex =
    (CharSubset.Whitespace.regexChar + "+").r

  val rightWhitespaceRegex: Regex =
    (CharSubset.Whitespace.regexChar + "+$").r

  // TODO do properly with unicode
  val onlyAllowSpacesAsWhitespace =
    regexReplace("[\t\r\n]+".r, " ")

  // TODO do properly with unicode
  val onlyAllowSpacesAndNewlinesAsWhitespace =
    literal('\t', ' ')

  val singleLineWhitespace =
    regexReplace(whitespaceRegex, " ") andThen trim

  val multiLineWhitespace =
    regexReplace("\r\n?".r, "\n") andThen
    onlyAllowSpacesAndNewlinesAsWhitespace andThen
    trim

  val noWhitespace =
    regexReplace(whitespaceRegex, "")

  val noWhitespaceRight =
    regexReplaceFirst(rightWhitespaceRegex, "")

  val squashUnderscores =
    regexReplace("__+".r, "_")

  val maxTwoConsecutiveNewLines =
    regexReplace("\n{3,}".r, "\n\n")

  def truncateToLength(range: Range.Inclusive): Endo[String] =
    truncateToLength(range.end)

  def truncateToLength(maxLen: Int): Endo[String] =
    s => if (s.length <= maxLen) s else s.take(maxLen)

  lazy val nonBlank: Iso[String, Option[String]] =
    Iso[String, Option[String]](s => if (s.isEmpty) None else Some(s))(_ getOrElse "")

  def removeTrailingChar(c: Char): Endo[String] =
    s => if (s.length != 0 && s(0) == c) s.substring(1) else s

  def blacklistChars(isBlacklisted: Char => Boolean): Endo[String] =
    _.filterNot(isBlacklisted)
}
